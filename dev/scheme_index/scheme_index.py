#!/usr/bin/env python3
"""Maintain PhyloTrace's index of published typing schemes.

The index the app ships (app/logic/data/scheme_index.csv) is generated from a
curated file (dev/scheme_index/curation.csv). This tool reads the providers'
public scheme listings, merges what it finds into the curation file and
rebuilds the index:

* a scheme seen for the first time is added as ``pending`` - it reaches the app
  only once a maintainer has set its species and type and marked it
  ``include``;
* a listed scheme whose locus count changed is updated in place; a changed
  name or page address of a scheme already marked ``include`` is written to
  the row but sends it back to ``pending``, so a maintainer confirms it before
  the app links to it again;
* a scheme a provider no longer lists is marked ``listed = no`` and drops out of
  the app's index.

Only scheme metadata is read - names, locus counts and page addresses - never
alleles, profiles or locus tables. Each source is switched on in config.json
only where its operator permits automated listing; robots.txt is checked before
every host and a disallowed path stops that source. Requests are sequential,
paced and identify the maintainer. PubMLST and Institut Pasteur are not read
at all; their rows are curated by hand (``source = manual``).

Usage:
    scheme_index.py update [--report FILE] [--offline]
    scheme_index.py build
"""

import argparse
import csv
import datetime
import html
import json
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
import urllib.robotparser
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
CURATION = HERE / "curation.csv"
CONFIG = HERE / "config.json"
INDEX = ROOT / "app" / "logic" / "data" / "scheme_index.csv"

CURATION_FIELDS = [
    "source", "source_id", "status", "listed", "provider", "species", "type",
    "scheme", "loci", "url", "note", "first_seen",
]
INDEX_FIELDS = ["species", "type", "provider", "scheme", "loci", "url", "note"]
STATUSES = {"include", "exclude", "pending"}
TYPES = {"cgMLST", "wgMLST", "MLST"}
PROVIDERS = HERE / "providers.json"


def load_provider_hosts(path=PROVIDERS):
    """Hosts an index row of each provider may link to, from providers.json.

    The app shows the url as a link, so only these (over https) pass. Provider
    ids are those of app/logic/scheme_sources.R; "own" and "other" have no
    public page and so cannot appear in the index.
    """
    providers = json.loads(Path(path).read_text(encoding="utf-8"))["providers"]
    return {pid: set(p["hosts"]) for pid, p in providers.items()}


PROVIDER_HOSTS = load_provider_hosts()
# Fields a listing may change on an included row only after review.
REVIEWED_FIELDS = ("scheme", "url")
SPECIES_RE = re.compile(r"^[A-Z][A-Za-z0-9-]* ?([A-Za-z0-9-]+(/[A-Za-z0-9-]+)*)?$")


class SourceError(Exception):
    """A source could not be read; its entries are left untouched."""

    def __init__(self, message, status=None):
        super().__init__(message)
        self.status = status


# HTTP answers of a database that needs a login: skipped, never a failure.
RESTRICTED = {401, 403}


class Client:
    """Polite HTTP client: identifies itself, honours robots.txt, paces calls.

    ``robots_permission`` names the written permission under which a provider
    lets this tool use an API its robots.txt closes to crawlers (PubMLST);
    without it, robots.txt always decides.
    """

    def __init__(self, user_agent, delay, robots_permission=None):
        self.user_agent = user_agent
        self.delay = delay
        self.robots_permission = robots_permission
        self._robots = {}
        self._last = {}

    def _allowed(self, url):
        if self.robots_permission:
            return True
        parts = urllib.parse.urlsplit(url)
        host = f"{parts.scheme}://{parts.netloc}"
        if host not in self._robots:
            parser = urllib.robotparser.RobotFileParser()
            try:
                text = self._fetch(host + "/robots.txt", check_robots=False)
                # A host without robots.txt answers with its web app's HTML;
                # that holds no rules, so nothing is disallowed.
                lines = [] if text.lstrip().startswith("<") else text.splitlines()
            except SourceError:
                lines = []
            parser.parse(lines)
            self._robots[host] = parser
        return self._robots[host].can_fetch(self.user_agent, url)

    def _fetch(self, url, check_robots=True):
        if check_robots and not self._allowed(url):
            raise SourceError(f"robots.txt disallows {url}")
        host = urllib.parse.urlsplit(url).netloc
        wait = self.delay - (time.monotonic() - self._last.get(host, 0))
        if wait > 0:
            time.sleep(wait)
        request = urllib.request.Request(url, headers={"User-Agent": self.user_agent})
        try:
            with urllib.request.urlopen(request, timeout=60) as response:
                body = response.read().decode("utf-8", errors="replace")
        except urllib.error.HTTPError as err:
            raise SourceError(f"{url}: {err}", status=err.code) from err
        except Exception as err:  # network or decoding failure
            raise SourceError(f"{url}: {err}") from err
        finally:
            self._last[host] = time.monotonic()
        return body

    def text(self, url):
        return self._fetch(url)

    def json(self, url):
        try:
            return json.loads(self._fetch(url))
        except json.JSONDecodeError as err:
            raise SourceError(f"{url}: not JSON ({err})") from err


def guess_type(name):
    """Scheme type suggested by a scheme's name; the curator has the last word."""
    lowered = name.lower()
    if "wgmlst" in lowered:
        return "wgMLST"
    if "cgmlst" in lowered:
        return "cgMLST"
    if re.search(r"\bmlst\b", lowered):
        return "MLST"
    return ""


def clean_species(name):
    """Best species guess from a provider's label ("Brucella spp." -> "Brucella spp")."""
    name = re.sub(r"\(.*?\)", " ", html.unescape(name))
    name = re.sub(r"\b(cg|wg)?MLST\b|sensu lato|species complex|complex", " ", name, flags=re.I)
    name = re.sub(r"\s+", " ", name).strip(" .")
    return name


# Sources ---------------------------------------------------------------------

def parse_cgmlst_org(page):
    """Rows of the cgMLST.org scheme list (/ncs/): name, target count, link."""
    rows = []
    pattern = re.compile(
        r"<tr><td><a href='(https://www\.cgmlst\.org/ncs/schema/([A-Za-z0-9_]+)/)'>"
        r"(.*?)</a></td><td[^>]*>([\d,]+)</td>",
        re.S,
    )
    for url, abb, label, loci in pattern.findall(page):
        name = re.sub(r"<[^>]+>", "", html.unescape(label)).strip()
        rows.append({
            "source_id": abb,
            "scheme": name,
            "species": clean_species(name),
            "type": "cgMLST",
            "loci": int(loci.replace(",", "")),
            "url": url,
        })
    if not rows:
        raise SourceError("cgMLST.org: no scheme rows found - the page layout may have changed")
    return rows


def fetch_cgmlst_org(client, settings):
    return parse_cgmlst_org(client.text(settings["url"]))


CHEWIE_PAGE = "https://chewbbaca.online/stats"


def parse_chewie_species(listing):
    """(species id, name) pairs of the Chewie-NS species list."""
    out = []
    for entry in listing:
        uri = entry["species"]["value"]
        out.append((uri.rstrip("/").rsplit("/", 1)[1], entry["name"]["value"]))
    return out


def parse_chewie_totals(species_id, species_name, totals):
    """Schemas of one Chewie-NS species from its /stats/species/<id>/totals."""
    rows = []
    for schema in totals.get("message", []):
        schema_id = schema["uri"].rstrip("/").rsplit("/", 1)[1]
        rows.append({
            "source_id": f"{species_id}/{schema_id}",
            "scheme": f"{species_name} {schema['name']}",
            "species": clean_species(species_name),
            "type": guess_type(schema["name"]) or "wgMLST",
            "loci": int(schema["nr_loci"]),
            # Chewie-NS's species and schema pages only render when reached
            # inside the site; opened from a link they stay blank (a script
            # error in its web app, seen 2026-11-19). The schemas overview
            # loads directly; the row's note names the species to open there.
            "url": CHEWIE_PAGE,
        })
    return rows


def fetch_chewie_ns(client, settings):
    api = settings["api"].rstrip("/")
    rows = []
    for species_id, name in parse_chewie_species(client.json(f"{api}/species/list")):
        rows += parse_chewie_totals(species_id, name, client.json(f"{api}/stats/species/{species_id}/totals"))
    return rows


def parse_bigsdb_schemes(db_description, db_page, schemes, details):
    """Schemes of one BIGSdb typing database (PubMLST, Institut Pasteur)."""
    rows = []
    species = clean_species(re.sub(r"sequence/profile definitions|REST API access to|seqdef database",
                                   " ", db_description, flags=re.I))
    for scheme, detail in zip(schemes, details):
        description = scheme["description"].strip()
        lowered = description.lower()
        # rMLST is licensed for academic, non-commercial use only and may not be
        # used to provide a service; it is never listed.
        if "rmlst" in lowered or "ribosomal" in lowered:
            continue
        kind = guess_type(description)
        if not kind:
            continue
        scheme_id = scheme["scheme"].rstrip("/").rsplit("/", 1)[1]
        db_name = scheme["scheme"].split("/db/", 1)[1].split("/", 1)[0]
        loci = detail.get("locus_count") or len(detail.get("loci", [])) or ""
        rows.append({
            "source_id": f"{db_name}/{scheme_id}",
            "scheme": f"{species} {description}".strip(),
            "species": species,
            "type": kind,
            "loci": loci,
            "url": db_page,
        })
    return rows


def fetch_bigsdb(client, settings):
    """Scheme metadata of every sequence-definition database of a BIGSdb site.

    Reads names, locus counts and page links only - never alleles, profiles or
    isolates. Requests run one after another from one process, as the
    operators asked.
    """
    rows = []
    for group in client.json(settings["api"]):
        for db in group.get("databases", []):
            name = db["name"]
            # rMLST databases are never read; isolate databases hold no schemes.
            if not name.endswith("seqdef") or "rmlst" in name.lower():
                continue
            try:
                listed = client.json(db["href"].rstrip("/") + "/schemes").get("schemes", [])
                wanted = [s for s in listed if "rmlst" not in s["description"].lower()
                          and "ribosomal" not in s["description"].lower()
                          and guess_type(s["description"])]
                details = [client.json(s["scheme"]) for s in wanted]
            except SourceError as err:
                # A database behind a login is not public; it is left out.
                # Any other failure stops the source, so nothing it lists is
                # marked as gone because of a hiccup.
                if err.status in RESTRICTED:
                    continue
                raise
            page = settings["page"].format(db=name)
            rows += parse_bigsdb_schemes(db["description"], page, wanted, details)
    return rows


FETCHERS = {
    "cgmlst_org": fetch_cgmlst_org,
    "chewie_ns": fetch_chewie_ns,
    "pubmlst": fetch_bigsdb,
    "pasteur": fetch_bigsdb,
}


# Curation --------------------------------------------------------------------

def read_curation(path=CURATION):
    with open(path, newline="", encoding="utf-8") as fh:
        return list(csv.DictReader(fh))


def row_order(row):
    """Stable position of a curation row: source, then scheme id with numbers
    compared as numbers ("2/1" before "18/1"). Status is deliberately not part
    of it, so reviewing a row never moves it and the monthly diff stays quiet."""
    parts = re.split(r"(\d+)", row["source_id"].lower())
    return (row["source"], [int(p) if p.isdigit() else p for p in parts])


def canonical(rows):
    return sorted(rows, key=row_order)


def write_curation(rows, path=CURATION):
    rows = canonical(rows)
    with open(path, "w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=CURATION_FIELDS, lineterminator="\n")
        writer.writeheader()
        writer.writerows({k: r.get(k, "") for k in CURATION_FIELDS} for r in rows)


def merge(curation, source, found, today):
    """Merge one source's listing into the curation rows; returns the changes.

    A new name or page address of an included row is never taken over silently:
    the row goes back to ``pending`` (and out of the app's index) until a
    maintainer has checked it in the pull request.
    """
    changes = {"new": [], "updated": [], "review": [], "unlisted": [], "relisted": []}
    known = {r["source_id"]: r for r in curation if r["source"] == source}
    seen = set()
    for entry in found:
        sid = entry["source_id"]
        seen.add(sid)
        row = known.get(sid)
        if row is None:
            row = {
                "source": source, "source_id": sid, "status": "pending", "listed": "yes",
                "provider": source, "species": entry["species"], "type": entry["type"],
                "scheme": entry["scheme"], "loci": str(entry["loci"]), "url": entry["url"],
                "note": "", "first_seen": today,
            }
            curation.append(row)
            changes["new"].append(row)
            continue
        if row.get("listed") == "no":
            row["listed"] = "yes"
            changes["relisted"].append(row)
        diffs = []
        needs_review = False
        for field in ("scheme", "loci", "url"):
            value = str(entry[field])
            if value and row.get(field, "") != value:
                diffs.append((field, row.get(field, ""), value))
                row[field] = value
                needs_review |= field in REVIEWED_FIELDS
        if needs_review and row.get("status") == "include":
            row["status"] = "pending"
            changes["review"].append((row, diffs))
        elif diffs:
            changes["updated"].append((row, diffs))
    for sid, row in known.items():
        if sid not in seen and row.get("listed") != "no":
            row["listed"] = "no"
            changes["unlisted"].append(row)
    return changes


def url_ok(url, provider):
    """True for a plain https address on one of the provider's own hosts."""
    if len(url) > 500 or re.search(r"[\s<>\"'`\\]", url):
        return False
    try:
        parts = urllib.parse.urlsplit(url)
        port = parts.port
    except ValueError:
        return False
    return (
        parts.scheme == "https"
        and "@" not in parts.netloc
        and port is None
        and parts.hostname in PROVIDER_HOSTS.get(provider, set())
    )


def row_problems(r):
    """Problems that would put a broken row into the app's index."""
    tag = f"{r['source']}:{r['source_id']}"
    if r["status"] not in STATUSES:
        return [f"{tag}: unknown status '{r['status']}'"]
    if r["status"] != "include":
        return []
    problems = []
    if r["type"] not in TYPES:
        problems.append(f"{tag}: type must be one of {sorted(TYPES)}")
    if r["provider"] not in PROVIDER_HOSTS:
        problems.append(f"{tag}: provider '{r['provider']}' cannot be linked from the index")
    if not SPECIES_RE.match(r["species"]):
        problems.append(f"{tag}: species '{r['species']}' is not 'Genus epithet[/epithet]'")
    if not url_ok(r["url"], r["provider"]):
        problems.append(f"{tag}: url must be a plain https address on the provider's own host")
    if r["loci"] and not r["loci"].isdigit():
        problems.append(f"{tag}: loci must be a whole number")
    return problems


def validate(rows):
    """Problems of all curation rows (see row_problems)."""
    return [p for r in rows for p in row_problems(r)]


def drop_superseded(rows):
    """Leaves out hand-entered rows an approved listed row now covers.

    PubMLST and Institut Pasteur were curated by hand until they allowed
    automated listing. A hand-entered row stays in the index until a row read
    from the provider for the same species and scheme type is approved, so no
    link disappears before its replacement is checked.
    """
    def key(r):
        return (r["provider"], r["type"], r["species"].strip().lower())

    covered = {key(r) for r in rows if r["source"] != "manual"}
    return [r for r in rows if r["source"] != "manual" or key(r) not in covered]


def build_index(curation, path=INDEX):
    """Write the app's index from the included, still listed, valid curation rows.

    A row validate() flags is left out, so a bad address never reaches the app
    even if the problem report is overlooked.
    """
    rows = [
        r for r in curation
        if r["status"] == "include" and r.get("listed", "yes") != "no" and not row_problems(r)
    ]
    rows = drop_superseded(rows)
    rows.sort(key=lambda r: (r["type"] != "cgMLST", r["species"].lower(), r["provider"], r["scheme"].lower()))
    with open(path, "w", newline="", encoding="utf-8") as fh:
        writer = csv.DictWriter(fh, fieldnames=INDEX_FIELDS, quoting=csv.QUOTE_ALL, lineterminator="\n")
        writer.writeheader()
        writer.writerows({k: r.get(k, "") for k in INDEX_FIELDS} for r in rows)
    return len(rows)


# Report ----------------------------------------------------------------------

def md_code(value, limit=200):
    """Provider-supplied text as an inert Markdown code span.

    Scheme names and addresses come from the providers; inside a code span
    GitHub renders no links, mentions or markup. Backticks, which would end
    the span, and line breaks, which would end the list item, are replaced.
    """
    text = re.sub(r"[\s\x00-\x1f\x7f]+", " ", str(value)).replace("`", "'").strip()
    if len(text) > limit:
        text = text[:limit - 3] + "..."
    return f"`{text or '-'}`"


def md_diffs(diffs):
    return "; ".join(f"{field}: {md_code(old)} -> {md_code(new)}" for field, old, new in diffs)


def report_markdown(results, problems, n_index):
    lines = ["Automated check of the providers' public scheme listings "
             "(dev/scheme_index/scheme_index.py). Only scheme names, locus counts "
             "and page addresses are read.", ""]
    for source, outcome in results.items():
        lines.append(f"### {source}")
        if isinstance(outcome, str):
            lines += [f"Skipped: {md_code(outcome, 500)}", ""]
            continue
        changes = outcome
        if not any(changes.values()):
            lines += ["No changes.", ""]
            continue
        if changes["new"]:
            lines.append("**New - pending review** (set species/type, then status `include` or `exclude`):")
            lines += [f"- {md_code(r['source_id'])} {md_code(r['scheme'])} "
                      f"({md_code(r['loci'] or '?')} loci) - {md_code(r['url'])}"
                      for r in changes["new"]]
        if changes["review"]:
            lines.append("**Name or address changed - back to pending review** (removed from the "
                         "app's index; check the new values, then set status `include` again):")
            lines += [f"- {md_code(r['source_id'])}: {md_diffs(d)}" for r, d in changes["review"]]
        if changes["updated"]:
            lines.append("**Changed:**")
            lines += [f"- {md_code(r['source_id'])} {md_code(r['scheme'])}: {md_diffs(d)}"
                      for r, d in changes["updated"]]
        if changes["unlisted"]:
            lines.append("**No longer listed** (removed from the app's index):")
            lines += [f"- {md_code(r['source_id'])} {md_code(r['scheme'])}"
                      for r in changes["unlisted"]]
        if changes["relisted"]:
            lines.append("**Listed again:**")
            lines += [f"- {md_code(r['source_id'])} {md_code(r['scheme'])}"
                      for r in changes["relisted"]]
        lines.append("")
    if problems:
        lines.append("### Problems to fix before merging (these rows are left out of the index)")
        lines += [f"- {md_code(p, 500)}" for p in problems]
        lines.append("")
    lines.append(f"The app's index now lists {n_index} schemes.")
    return "\n".join(lines) + "\n"


def update(report_path=None, offline=False):
    config = json.loads(CONFIG.read_text(encoding="utf-8"))
    curation = read_curation()
    today = datetime.date.today().isoformat()
    results = {}
    for source, settings in config["sources"].items():
        if not settings.get("enabled"):
            results[source] = settings.get("reason", "disabled")
            continue
        if offline:
            results[source] = "offline run"
            continue
        if source not in FETCHERS:
            results[source] = "no fetcher for this source"
            continue
        # Each source gets its own pacing; PubMLST asked for no pause, the
        # others keep the default one.
        client = Client(
            config["user_agent"],
            settings.get("delay_seconds", config["delay_seconds"]),
            settings.get("robots_permission"),
        )
        try:
            found = FETCHERS[source](client, settings)
        except SourceError as err:
            # A failed source changes nothing: its schemes are not marked as
            # unlisted because of a network hiccup or a layout change.
            results[source] = f"failed - {err}"
            continue
        results[source] = merge(curation, source, found, today)
    problems = validate(curation)
    write_curation(curation)
    n_index = build_index(curation)
    report = report_markdown(results, problems, n_index)
    if report_path:
        Path(report_path).write_text(report, encoding="utf-8")
    print(report)
    return 1 if problems else 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="command", required=True)
    up = sub.add_parser("update", help="read the provider listings, merge them, rebuild the index")
    up.add_argument("--report", help="write the change report (Markdown) to this file")
    up.add_argument("--offline", action="store_true", help="skip all network sources")
    sub.add_parser("build", help="rebuild the app's index from curation.csv")
    args = parser.parse_args(argv)
    if args.command == "update":
        return update(args.report, args.offline)
    curation = read_curation()
    problems = validate(curation)
    for problem in problems:
        print(problem, file=sys.stderr)
    write_curation(curation)
    print(f"{build_index(curation)} schemes written to {INDEX.relative_to(ROOT)}")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
