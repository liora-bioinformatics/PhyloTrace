"""Offline tests of the scheme index tooling (run: python -m unittest discover -s dev/scheme_index)."""

import contextlib
import csv
import io
import json
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))

import scheme_index as si  # noqa: E402

FIXTURES = HERE / "fixtures"
URL = "https://www.cgmlst.org/ncs/schema/X/"


def row(**fields):
    base = {
        "source": "cgmlst_org", "source_id": "X", "status": "include", "listed": "yes",
        "provider": "cgmlst_org", "species": "Testus organismus", "type": "cgMLST",
        "scheme": "T. organismus cgMLST", "loci": "100", "url": URL,
        "note": "", "first_seen": "2026-01-01",
    }
    base.update(fields)
    return base


class ParserTests(unittest.TestCase):
    def test_cgmlst_org_list(self):
        rows = si.parse_cgmlst_org((FIXTURES / "cgmlst_org_ncs.html").read_text())
        by_id = {r["source_id"]: r for r in rows}
        self.assertEqual(set(by_id), {"Abaumannii", "Brucella", "Bmallei_fli", "Kpneumoniae_complex"})
        self.assertEqual(by_id["Abaumannii"]["loci"], 2390)
        self.assertEqual(by_id["Abaumannii"]["scheme"], "Acinetobacter baumannii cgMLST")
        self.assertEqual(by_id["Abaumannii"]["url"], "https://www.cgmlst.org/ncs/schema/Abaumannii/")
        self.assertEqual(by_id["Brucella"]["species"], "Brucella spp")
        self.assertEqual(by_id["Bmallei_fli"]["species"], "Burkholderia mallei")
        self.assertEqual(
            by_id["Kpneumoniae_complex"]["species"],
            "Klebsiella pneumoniae/variicola/quasipneumoniae",
        )

    def test_cgmlst_org_layout_change_is_an_error_not_an_empty_list(self):
        # An empty result would otherwise mark every scheme as no longer listed.
        with self.assertRaises(si.SourceError):
            si.parse_cgmlst_org("<html><body>maintenance</body></html>")

    def test_chewie_ns(self):
        species = si.parse_chewie_species(json.loads((FIXTURES / "chewie_species_list.json").read_text()))
        self.assertIn(("1", "Streptococcus pyogenes"), species)
        rows = si.parse_chewie_totals("1", "Streptococcus pyogenes",
                                      json.loads((FIXTURES / "chewie_totals_1.json").read_text()))
        self.assertEqual(rows[0]["source_id"], "1/1")
        self.assertEqual(rows[0]["type"], "wgMLST")
        self.assertEqual(rows[0]["loci"], 3044)
        # The overview page: Chewie-NS's deep links render blank when opened directly.
        self.assertEqual(rows[0]["url"], "https://chewbbaca.online/stats")


class BigsdbTests(unittest.TestCase):
    def test_bigsdb_keeps_typing_schemes_and_never_rmlst(self):
        schemes = [
            {"description": "MLST", "scheme": "https://rest.pubmlst.org/db/pubmlst_x_seqdef/schemes/1"},
            {"description": "cgMLST", "scheme": "https://rest.pubmlst.org/db/pubmlst_x_seqdef/schemes/2"},
            {"description": "Ribosomal MLST", "scheme": "https://rest.pubmlst.org/db/pubmlst_x_seqdef/schemes/3"},
            {"description": "Antigen typing", "scheme": "https://rest.pubmlst.org/db/pubmlst_x_seqdef/schemes/4"},
        ]
        details = [{"loci": list("abcdefg")}, {"locus_count": 1500}, {"loci": ["r"] * 53}, {"loci": ["a"]}]
        rows = si.parse_bigsdb_schemes("Testus spp. sequence/profile definitions",
                                       "https://pubmlst.org/bigsdb?db=pubmlst_x_seqdef", schemes, details)
        self.assertEqual([r["type"] for r in rows], ["MLST", "cgMLST"])
        self.assertEqual([r["loci"] for r in rows], [7, 1500])
        self.assertEqual(rows[0]["source_id"], "pubmlst_x_seqdef/1")
        self.assertEqual(rows[0]["species"], "Testus spp")

    def test_fetch_reads_only_typing_scheme_metadata(self):
        class FakeClient:
            def __init__(self):
                self.urls = []

            def json(self, url):
                self.urls.append(url)
                if url == "https://api.test/db":
                    return [{"databases": [
                        {"name": "pubmlst_x_isolates", "href": "https://api.test/db/pubmlst_x_isolates",
                         "description": "Testus isolates"},
                        {"name": "pubmlst_rmlst_seqdef", "href": "https://api.test/db/pubmlst_rmlst_seqdef",
                         "description": "rMLST"},
                        {"name": "pubmlst_locked_seqdef", "href": "https://api.test/db/pubmlst_locked_seqdef",
                         "description": "Lockus sequence/profile definitions"},
                        {"name": "pubmlst_x_seqdef", "href": "https://api.test/db/pubmlst_x_seqdef",
                         "description": "Testus organismus sequence/profile definitions"},
                    ]}]
                if "locked" in url:
                    raise si.SourceError(url + ": HTTP Error 401", status=401)
                if url.endswith("/schemes"):
                    return {"schemes": [
                        {"description": "MLST", "scheme": "https://api.test/db/pubmlst_x_seqdef/schemes/1"},
                        {"description": "rMLST", "scheme": "https://api.test/db/pubmlst_x_seqdef/schemes/2"},
                    ]}
                return {"locus_count": 7}

        client = FakeClient()
        rows = si.fetch_bigsdb(client, {"api": "https://api.test/db",
                                        "page": "https://pubmlst.org/bigsdb?db={db}"})
        self.assertEqual([r["source_id"] for r in rows], ["pubmlst_x_seqdef/1"])
        # Isolate databases and rMLST are never even requested; a database
        # behind a login is skipped without failing the source.
        self.assertFalse(any("isolates" in u or "rmlst" in u or u.endswith("/schemes/2")
                             for u in client.urls))
        self.assertEqual(rows[0]["url"], "https://pubmlst.org/bigsdb?db=pubmlst_x_seqdef")


class MergeTests(unittest.TestCase):
    def test_new_changed_unlisted_and_relisted(self):
        curation = [
            row(source_id="Kept", loci="100"),
            row(source_id="Gone"),
            row(source_id="Back", listed="no"),
            row(source="manual", source_id="untouched"),
        ]
        found = [
            {"source_id": "Kept", "scheme": "T. organismus cgMLST", "species": "Testus organismus",
             "type": "cgMLST", "loci": 120, "url": URL},
            {"source_id": "Back", "scheme": "T. organismus cgMLST", "species": "Testus organismus",
             "type": "cgMLST", "loci": 100, "url": URL},
            {"source_id": "Fresh", "scheme": "Novus cgMLST", "species": "Novus",
             "type": "cgMLST", "loci": 50, "url": "https://example.org/n"},
        ]
        changes = si.merge(curation, "cgmlst_org", found, "2026-11-17")
        by_id = {r["source_id"]: r for r in curation}
        self.assertEqual(by_id["Kept"]["loci"], "120")
        self.assertEqual(by_id["Gone"]["listed"], "no")
        self.assertEqual(by_id["Back"]["listed"], "yes")
        self.assertEqual(by_id["Fresh"]["status"], "pending")
        self.assertEqual(by_id["untouched"]["listed"], "yes")
        self.assertEqual([len(changes[k]) for k in ("new", "updated", "unlisted", "relisted")], [1, 1, 1, 1])
        self.assertEqual(changes["review"], [])
        self.assertEqual(by_id["Kept"]["status"], "include")

    def test_new_address_or_name_of_an_included_row_needs_review(self):
        # A provider (or whoever controls its page) must not be able to change
        # what the app links to without a maintainer seeing it.
        curation = [
            row(source_id="Moved"), row(source_id="Renamed"), row(source_id="Draft", status="pending"),
        ]
        found = [
            {"source_id": "Moved", "scheme": "T. organismus cgMLST", "species": "Testus organismus",
             "type": "cgMLST", "loci": 100, "url": "https://www.cgmlst.org/ncs/schema/Moved2/"},
            {"source_id": "Renamed", "scheme": "Renamed cgMLST", "species": "Testus organismus",
             "type": "cgMLST", "loci": 100, "url": URL},
            {"source_id": "Draft", "scheme": "Draft cgMLST", "species": "Testus organismus",
             "type": "cgMLST", "loci": 100, "url": "https://www.cgmlst.org/ncs/schema/Draft/"},
        ]
        changes = si.merge(curation, "cgmlst_org", found, "2026-11-17")
        by_id = {r["source_id"]: r for r in curation}
        self.assertEqual(by_id["Moved"]["status"], "pending")
        self.assertEqual(by_id["Moved"]["url"], "https://www.cgmlst.org/ncs/schema/Moved2/")
        self.assertEqual(by_id["Renamed"]["status"], "pending")
        self.assertEqual([r["source_id"] for r, _ in changes["review"]], ["Moved", "Renamed"])
        # A row not yet in the index is simply updated.
        self.assertEqual(by_id["Draft"]["status"], "pending")
        self.assertEqual([r["source_id"] for r, _ in changes["updated"]], ["Draft"])
        with tempfile.TemporaryDirectory() as tmp:
            self.assertEqual(si.build_index(curation, Path(tmp) / "index.csv"), 0)

    def test_an_unchanged_listing_changes_nothing(self):
        curation = [row(source_id="Kept")]
        found = [{"source_id": "Kept", "scheme": "T. organismus cgMLST", "species": "Testus organismus",
                  "type": "cgMLST", "loci": 100, "url": URL}]
        changes = si.merge(curation, "cgmlst_org", found, "2026-11-17")
        self.assertFalse(any(changes.values()))


class BuildTests(unittest.TestCase):
    def test_only_included_listed_rows_reach_the_index(self):
        curation = [
            row(source_id="A"),
            row(source_id="B", status="pending"),
            row(source_id="C", status="exclude"),
            row(source_id="D", listed="no"),
        ]
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "index.csv"
            self.assertEqual(si.build_index(curation, path), 1)
            with path.open() as fh:
                rows = list(csv.DictReader(fh))
        self.assertEqual(rows[0]["scheme"], "T. organismus cgMLST")
        self.assertEqual(list(rows[0]), si.INDEX_FIELDS)

    def test_validation_catches_rows_the_app_could_not_use(self):
        problems = si.validate([
            row(source_id="ok"),
            row(source_id="bad-species", species="klebsiella"),
            row(source_id="bad-url", url="javascript:alert(1)"),
            row(source_id="bad-type", type="rMLST"),
            row(source_id="pending-anything", status="pending", species="?"),
        ])
        self.assertEqual(len(problems), 3)

    def test_only_https_addresses_on_the_providers_host_pass(self):
        good = [
            ("cgmlst_org", "https://www.cgmlst.org/ncs/schema/Ecoli/"),
            ("chewie_ns", "https://chewbbaca.online/species/1/schemas/1"),
            ("pubmlst", "https://pubmlst.org/bigsdb?db=pubmlst_neisseria_seqdef"),
            ("pasteur", "https://bigsdb.pasteur.fr/ecoli/"),
        ]
        bad = [
            ("cgmlst_org", "http://www.cgmlst.org/ncs/schema/Ecoli/"),
            ("cgmlst_org", "https://evil.example/ncs/schema/Ecoli/"),
            ("cgmlst_org", "https://www.cgmlst.org.evil.example/"),
            ("cgmlst_org", "https://www.cgmlst.org@evil.example/"),
            ("cgmlst_org", "https://www.cgmlst.org:8443/"),
            ("cgmlst_org", "https://chewbbaca.online/species/1/schemas/1"),
            ("cgmlst_org", "javascript:alert(1)"),
            ("cgmlst_org", 'https://www.cgmlst.org/x"onclick="alert(1)'),
            ("cgmlst_org", "https://www.cgmlst.org/a b"),
            ("own", "https://www.cgmlst.org/"),
        ]
        for provider, url in good:
            self.assertTrue(si.url_ok(url, provider), url)
        for provider, url in bad:
            self.assertFalse(si.url_ok(url, provider), (provider, url))

    def test_rows_failing_validation_are_left_out_of_the_index(self):
        curation = [
            row(source_id="ok"),
            row(source_id="elsewhere", url="https://evil.example/x"),
            row(source_id="plain-http", url="http://www.cgmlst.org/ncs/schema/X/"),
            row(source_id="bad-species", species="klebsiella"),
        ]
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "index.csv"
            self.assertEqual(si.build_index(curation, path), 1)
            with path.open() as fh:
                urls = [r["url"] for r in csv.DictReader(fh)]
        self.assertEqual(urls, [URL])

    def test_shipped_curation_is_valid(self):
        self.assertEqual(si.validate(si.read_curation()), [])

    def test_shipped_curation_is_in_canonical_order(self):
        # Otherwise every monthly run would rewrite the file and open a pull
        # request without any provider change. Fix with `scheme_index.py build`.
        rows = si.read_curation()
        self.assertEqual([r["source_id"] for r in rows], [r["source_id"] for r in si.canonical(rows)])

    def test_reviewing_a_row_does_not_move_it(self):
        rows = [row(source_id="2/1", status="include"), row(source_id="18/1", status="pending")]
        reviewed = [dict(rows[0]), dict(rows[1], status="exclude")]
        self.assertEqual([r["source_id"] for r in si.canonical(rows)],
                         [r["source_id"] for r in si.canonical(reviewed)])


class ProviderRegistryTests(unittest.TestCase):
    def setUp(self):
        self.providers = json.loads(si.PROVIDERS.read_text(encoding="utf-8"))["providers"]

    def test_every_curated_provider_is_registered(self):
        used = {r["provider"] for r in si.read_curation() if r["status"] == "include"}
        self.assertEqual(used - set(self.providers), set())

    def test_only_providers_allowing_automated_listing_are_collected(self):
        config = json.loads(si.CONFIG.read_text(encoding="utf-8"))
        for source in config["sources"]:
            self.assertIn(source, self.providers)
            self.assertEqual(self.providers[source]["listing"]["method"], "automated")

    def test_every_provider_entry_is_complete(self):
        for pid, p in self.providers.items():
            for key in ("name", "hosts", "offers", "terms_url", "listing", "permission"):
                self.assertIn(key, p, f"{pid} lacks {key}")
            self.assertIn(p["listing"]["method"], {"automated", "manual"})


class UpdateTests(unittest.TestCase):
    def test_a_failing_source_leaves_its_schemes_alone(self):
        # A provider being down, or changing its page layout, must not read as
        # every one of its schemes having been withdrawn.
        with tempfile.TemporaryDirectory() as tmp:
            tmp = Path(tmp)
            curation, index, config = tmp / "curation.csv", tmp / "index.csv", tmp / "config.json"
            si.write_curation([row(source_id="Kept")], curation)
            config.write_text(json.dumps({
                "user_agent": "test", "delay_seconds": 0,
                "sources": {"cgmlst_org": {"enabled": True, "url": "https://example.org/ncs/"}},
            }))

            def down(client, settings):
                raise si.SourceError("connection refused")

            saved = (si.CURATION, si.INDEX, si.CONFIG, si.FETCHERS["cgmlst_org"])
            si.CURATION, si.INDEX, si.CONFIG, si.FETCHERS["cgmlst_org"] = curation, index, config, down
            try:
                report = tmp / "report.md"
                # The report is printed too; kept out of the test log, where its
                # made-up failure would read like a real one.
                with contextlib.redirect_stdout(io.StringIO()):
                    self.assertEqual(si.update(str(report)), 0)
            finally:
                si.CURATION, si.INDEX, si.CONFIG, si.FETCHERS["cgmlst_org"] = saved
            self.assertEqual(si.read_curation(curation)[0]["listed"], "yes")
            self.assertIn("failed - connection refused", report.read_text())
            # Only the temporary files are used, never the shipped curation.
            self.assertIn("The app's index now lists 1 schemes.", report.read_text())


class ReportTests(unittest.TestCase):
    def test_provider_text_cannot_inject_markdown(self):
        hostile = "Evil `cgMLST`\n### @maintainer [click](https://evil.example) <img src=x>"
        changes = {
            "new": [row(source_id="N", scheme=hostile, url="https://evil.example/`x`")],
            "updated": [], "review": [(row(source_id="R"), [("scheme", "old", hostile)])],
            "unlisted": [row(source_id="U", scheme=hostile)], "relisted": [],
        }
        report = si.report_markdown({"cgmlst_org": changes}, [], 1)
        body = [line for line in report.splitlines() if line.startswith("- ")]
        self.assertEqual(len(body), 3)
        self.assertNotIn("\n###", report.split("### cgmlst_org", 1)[1])
        for line in body:
            # Every provider string sits inside a code span: the backticks pair
            # up and nothing outside them carries markup or a mention.
            self.assertEqual(line.count("`") % 2, 0, line)
            outside = "".join(line.split("`")[0::2])
            for token in ("@", "[", "](", "<", "#"):
                self.assertNotIn(token, outside, line)

    def test_md_code_neutralises_backticks_and_line_breaks(self):
        self.assertEqual(si.md_code("a`b\r\nc\u2028d"), "`a'b c d`")
        self.assertEqual(si.md_code(""), "`-`")


class RobotsTests(unittest.TestCase):
    def test_disallowed_host_stops_the_source(self):
        client = si.Client("test-agent", 0)
        parser = si.urllib.robotparser.RobotFileParser()
        parser.parse(["User-agent: *", "Disallow: /"])
        client._robots["https://rest.example.org"] = parser
        with self.assertRaises(si.SourceError):
            client.text("https://rest.example.org/db")

    def test_written_permission_lifts_robots_only_where_recorded(self):
        allowed = si.Client("test-agent", 0, robots_permission="granted 2026-11-19")
        self.assertTrue(allowed._allowed("https://rest.example.org/db"))
        config = json.loads(si.CONFIG.read_text(encoding="utf-8"))
        with_permission = [s for s, c in config["sources"].items() if c.get("robots_permission")]
        self.assertEqual(with_permission, ["pubmlst"])


class SupersededTests(unittest.TestCase):
    def test_hand_entered_row_leaves_once_a_listed_row_is_approved(self):
        manual = row(source="manual", source_id="pubmlst:Testus:MLST", provider="pubmlst",
                     type="MLST", url="https://pubmlst.org/organisms/testus")
        listed = row(source="pubmlst", source_id="pubmlst_t_seqdef/1", provider="pubmlst",
                     type="MLST", url="https://pubmlst.org/bigsdb?db=pubmlst_t_seqdef")
        other = row(source="manual", source_id="enterobase:x", provider="enterobase", type="MLST",
                    url="https://enterobase.warwick.ac.uk/species/index/x")
        self.assertEqual(si.drop_superseded([manual, listed, other]), [listed, other])
        # Still pending, the listed row covers nothing yet.
        self.assertEqual(si.drop_superseded([manual]), [manual])


class BigsdbFailureTests(unittest.TestCase):
    def test_other_errors_still_stop_the_source(self):
        class Broken:
            def json(self, url):
                if url == "https://api.test/db":
                    return [{"databases": [{"name": "x_seqdef", "href": "https://api.test/db/x_seqdef",
                                            "description": "X"}]}]
                raise si.SourceError(url + ": HTTP Error 500", status=500)

        with self.assertRaises(si.SourceError):
            si.fetch_bigsdb(Broken(), {"api": "https://api.test/db", "page": "https://pubmlst.org/{db}"})


if __name__ == "__main__":
    unittest.main()
