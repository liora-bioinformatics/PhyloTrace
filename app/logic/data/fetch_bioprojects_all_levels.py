"""
Fetch NCBI Assembly metadata for every species / species-complex listed in
cgmlst_schemes.csv, group by BioProject, and report the top N largest
BioProjects (by assembly count, N=3 by default) per row.

Unlike an earlier "Complete Genome / Chromosome only" version, this pass
includes ALL assembly levels (contig, scaffold, chromosome, complete) and does
not distinguish between them - matching a request that noticed a 250+ genome
BioProject (all Contig-level) was being excluded by the strict filter.

Strategy: NCBI's per-assembly REST/CLI queries are too slow at this scale
(Escherichia coli alone has ~230,000 GenBank assemblies; enumerating every
record's full JSON metadata species-by-species would mean downloading
hundreds of MB - GBs per species and take a very long time). Instead this
script does ONE streaming pass over NCBI's bulk flat file
    https://ftp.ncbi.nlm.nih.gov/genomes/genbank/bacteria/assembly_summary.txt
(~1.5 GB, one row per GenBank bacterial assembly, refreshed daily), tallying
bioproject counts per target species/genus in memory (no per-assembly JSON,
just counters). BioProject titles for the resulting top-N winners are then
fetched individually and cheaply via
    datasets summary genome accession <BioProjectAccession> --limit 1

Requires the NCBI `datasets` command-line tool (ncbi-datasets-cli) on PATH.
"""

import argparse
import csv
import json
import re
import subprocess
import time
from collections import Counter
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

HERE = Path(__file__).parent
CSV_PATH = HERE / "cgmlst_schemes.csv"
SUMMARY_URL = "https://ftp.ncbi.nlm.nih.gov/genomes/genbank/bacteria/assembly_summary.txt"
LOCAL_SUMMARY_PATH = HERE / "assembly_summary.txt"
LOG = HERE / "fetch_all_levels_log.txt"


def log(msg):
    line = f"[{time.strftime('%H:%M:%S')}] {msg}"
    print(line, flush=True)
    with open(LOG, "a") as f:
        f.write(line + "\n")


# NCBI has reclassified the genus for some organisms since these cgMLST scheme
# names were assigned; without these aliases the new genus name silently
# fails to match and the row looks like it has zero public assemblies.
GENUS_RENAME_ALIASES = {
    "Mycoplasma gallisepticum": ["Mycoplasmoides gallisepticum"],
}


def species_query_names(raw_name):
    """Turn a cgmlst_schemes.csv 'species' field into one or more NCBI taxon query
    strings. Handles '/' complexes, genus-level '_spp' rows, strips
    parenthetical scheme qualifiers like '(RKI)'/'(FLI)', and adds known
    genus-rename aliases."""
    name = raw_name.strip().replace("_", " ")
    name = re.sub(r"\s*\([^)]*\)\s*$", "", name).strip()

    if name.endswith(" spp"):
        return [name[: -len(" spp")]]  # genus-level, e.g. "Brucella"

    if "/" in name:
        parts = name.split("/")
        genus = parts[0].split(" ")[0]
        species_list = [parts[0]]
        for p in parts[1:]:
            p = p.strip()
            species_list.append(p if " " in p else f"{genus} {p}")
        result = []
        for sp in species_list:
            result.append(sp)
            result.extend(GENUS_RENAME_ALIASES.get(sp, []))
        return result

    return [name] + GENUS_RENAME_ALIASES.get(name, [])


def load_rows():
    rows = []
    with open(CSV_PATH) as f:
        for r in csv.DictReader(f):
            rows.append({
                "id": r[""], "raw": r["species"], "abb": r["abb"],
                "taxa": species_query_names(r["species"]),
            })
    return rows


def stream_and_count(rows):
    """One pass over the bulk assembly_summary.txt, tallying
    taxon -> Counter(bioproject_accession -> assembly count), all levels included."""
    two_word_targets = {}   # (genus, species) -> taxon string
    genus_only_targets = {}  # genus -> taxon string
    for row in rows:
        for t in row["taxa"]:
            words = t.split(" ")
            if len(words) == 1:
                genus_only_targets[words[0]] = t
            else:
                two_word_targets[(words[0], words[1])] = t

    taxon_counts = {t: Counter() for row in rows for t in row["taxa"]}

    # Download to a local file first and verify its size against the server's
    # Content-Length before processing - a bare curl-to-pipe silently truncated
    # partway through on a previous attempt (curl exit 18, "partial file"),
    # which produced zero counts for several taxa without any obvious error.
    if not LOCAL_SUMMARY_PATH.exists():
        download_verified(SUMMARY_URL, LOCAL_SUMMARY_PATH)
    else:
        log(f"Using existing local file {LOCAL_SUMMARY_PATH} "
            f"({LOCAL_SUMMARY_PATH.stat().st_size} bytes) - assumed already verified")

    log(f"Scanning {LOCAL_SUMMARY_PATH} ...")
    n_total = 0
    n_matched = 0
    with open(LOCAL_SUMMARY_PATH) as f:
        for line in f:
            if line.startswith("#"):
                continue
            n_total += 1
            if n_total % 500000 == 0:
                log(f"  ...{n_total} rows scanned, {n_matched} matched so far")
            fields = line.rstrip("\n").split("\t")
            if len(fields) < 8:
                continue
            bioproject = fields[1]
            organism_name = fields[7]
            words = organism_name.split(" ")
            if not words:
                continue
            genus = words[0]
            if genus in genus_only_targets:
                taxon_counts[genus_only_targets[genus]][bioproject] += 1
                n_matched += 1
            if len(words) >= 2:
                key2 = (words[0], words[1])
                if key2 in two_word_targets:
                    taxon_counts[two_word_targets[key2]][bioproject] += 1
                    n_matched += 1
    log(f"Done scanning: {n_total} total rows, {n_matched} matched rows")
    return taxon_counts


def download_verified(url, dest_path, max_attempts=15):
    """Download url to dest_path with resume+retry, verified against the
    server's current Content-Length (the file is refreshed daily, so re-check
    the expected size on each attempt rather than trusting a stale value)."""
    for attempt in range(1, max_attempts + 1):
        head = subprocess.run(
            ["curl", "-sI", "-m", "20", url], capture_output=True, text=True,
        )
        expected = None
        for line in head.stdout.splitlines():
            if line.lower().startswith("content-length:"):
                expected = int(line.split(":", 1)[1].strip())
        cur_size = dest_path.stat().st_size if dest_path.exists() else 0
        if expected is not None and cur_size == expected:
            log(f"Download verified complete: {cur_size} bytes")
            return
        log(f"Download attempt {attempt}: local={cur_size} expected={expected}, fetching...")
        subprocess.run(
            ["curl", "-sS", "--fail", "-C", "-", "--retry", "10",
             "--retry-delay", "5", "--retry-all-errors",
             "-o", str(dest_path), url],
            timeout=1800,
        )
    raise RuntimeError(f"Failed to fully download {url} after {max_attempts} attempts")


def fetch_bioproject_title(accession, retries=3):
    for attempt in range(1, retries + 1):
        try:
            result = subprocess.run(
                ["datasets", "summary", "genome", "accession", accession,
                 "--limit", "1", "--as-json-lines"],
                capture_output=True, text=True, timeout=60,
            )
            for line in result.stdout.splitlines():
                if not line.strip():
                    continue
                rec = json.loads(line)
                ai = rec.get("assembly_info", {})
                for grp in ai.get("bioproject_lineage", []):
                    for bp in grp.get("bioprojects", []):
                        if bp.get("accession") == accession and bp.get("title"):
                            return accession, bp["title"]
            return accession, ""
        except (subprocess.TimeoutExpired, json.JSONDecodeError) as e:
            log(f"  retrying title lookup for {accession}: {e}")
            time.sleep(2 * attempt)
    return accession, ""


def parse_args():
    parser = argparse.ArgumentParser(
        description="Rank BioProjects per cgMLST species/complex by assembly count."
    )
    parser.add_argument(
        "--max-assemblies", type=int, default=None,
        help=(
            "Exclude BioProjects with more than this many assemblies for a given "
            "species/complex row before ranking. Useful for filtering out huge "
            "automated collections (e.g. NCBI's Pathogen Detection Assembly "
            "Project) in favor of smaller, more specific research collections. "
            "Default: no cap."
        ),
    )
    parser.add_argument(
        "--top-n", type=int, default=3,
        help="Number of largest BioProjects to report per species/complex row. Default: 3.",
    )
    return parser.parse_args()


def main():
    args = parse_args()
    max_assemblies = args.max_assemblies
    top_n = args.top_n
    suffix = f"_max{max_assemblies}" if max_assemblies is not None else ""
    suffix += f"_top{top_n}" if top_n != 3 else ""

    rows = load_rows()
    log(f"Loaded {len(rows)} CSV rows, "
        f"{len(set(t for r in rows for t in r['taxa']))} unique taxa, "
        f"max_assemblies={max_assemblies}, top_n={top_n}")

    taxon_counts = stream_and_count(rows)

    final_rows = []
    for row in rows:
        combined = Counter()
        for t in row["taxa"]:
            combined.update(taxon_counts.get(t, Counter()))
        if max_assemblies is not None:
            combined = Counter({
                acc: cnt for acc, cnt in combined.items() if cnt <= max_assemblies
            })
        top = combined.most_common(top_n)
        final_rows.append({
            "id": row["id"], "raw": row["raw"], "taxa": row["taxa"],
            "total_assemblies": sum(combined.values()),
            "n_bioprojects": len(combined),
            "top": [{"accession": a, "count": c} for a, c in top],
        })

    winning_accessions = sorted({bp["accession"] for r in final_rows for bp in r["top"]})
    log(f"Fetching titles for {len(winning_accessions)} winning BioProjects...")
    titles = {}
    with ThreadPoolExecutor(max_workers=6) as ex:
        futs = {ex.submit(fetch_bioproject_title, a): a for a in winning_accessions}
        for fut in as_completed(futs):
            acc, title = fut.result()
            titles[acc] = title
            log(f"  title: {acc} -> {title!r}")

    for r in final_rows:
        for bp in r["top"]:
            bp["title"] = titles.get(bp["accession"], "")

    out_json = HERE / f"results_all_levels{suffix}.json"
    with open(out_json, "w") as f:
        json.dump(final_rows, f, indent=2)
    log(f"Wrote {out_json.name}")

    out_md = HERE / f"results_all_levels{suffix}.md"
    with open(out_md, "w") as f:
        count_label = (
            f"Assembly Count (all levels, capped at {max_assemblies}/BioProject)"
            if max_assemblies is not None else "Assembly Count (all levels)"
        )
        f.write(f"| Species / Complex Name | BioProject Accession | BioProject Title / Summary | {count_label} |\n")
        f.write("| :--- | :--- | :--- | :--- |\n")
        for r in final_rows:
            display = r["raw"].replace("_", " ")
            for bp in r["top"]:
                title = bp["title"] or "_(title unavailable)_"
                f.write(f"| {display} | {bp['accession']} | {title} | {bp['count']} |\n")
    log(f"Wrote {out_md.name}")
    log("DONE")


if __name__ == "__main__":
    main()
