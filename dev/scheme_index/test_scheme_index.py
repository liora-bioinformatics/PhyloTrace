"""Offline tests of the scheme index tooling (run: python -m unittest discover -s dev/scheme_index)."""

import csv
import json
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))

import scheme_index as si  # noqa: E402

FIXTURES = HERE / "fixtures"


def row(**fields):
    base = {
        "source": "cgmlst_org", "source_id": "X", "status": "include", "listed": "yes",
        "provider": "cgmlst_org", "species": "Testus organismus", "type": "cgMLST",
        "scheme": "T. organismus cgMLST", "loci": "100", "url": "https://example.org/x",
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
        self.assertEqual(rows[0]["url"], "https://chewbbaca.online/species/1/schemas/1")

    def test_bigsdb_keeps_typing_schemes_and_never_rmlst(self):
        schemes = [
            {"description": "MLST", "scheme": "https://rest.pubmlst.org/db/pubmlst_x_seqdef/schemes/1"},
            {"description": "cgMLST", "scheme": "https://rest.pubmlst.org/db/pubmlst_x_seqdef/schemes/2"},
            {"description": "Ribosomal MLST", "scheme": "https://rest.pubmlst.org/db/pubmlst_x_seqdef/schemes/3"},
            {"description": "Antigen typing", "scheme": "https://rest.pubmlst.org/db/pubmlst_x_seqdef/schemes/4"},
        ]
        details = [{"loci": list("abcdefg")}, {"loci": ["l"] * 1500}, {"loci": ["r"] * 53}, {"loci": ["a"]}]
        rows = si.parse_bigsdb_schemes("Testus spp. sequence/profile definitions",
                                       "https://pubmlst.org/bigsdb?db=pubmlst_x_seqdef", schemes, details)
        self.assertEqual([r["type"] for r in rows], ["MLST", "cgMLST"])
        self.assertEqual([r["loci"] for r in rows], [7, 1500])
        self.assertEqual(rows[0]["source_id"], "pubmlst_x_seqdef/1")
        self.assertEqual(rows[0]["species"], "Testus spp")


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
             "type": "cgMLST", "loci": 120, "url": "https://example.org/x"},
            {"source_id": "Back", "scheme": "T. organismus cgMLST", "species": "Testus organismus",
             "type": "cgMLST", "loci": 100, "url": "https://example.org/x"},
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

    def test_an_unchanged_listing_changes_nothing(self):
        curation = [row(source_id="Kept")]
        found = [{"source_id": "Kept", "scheme": "T. organismus cgMLST", "species": "Testus organismus",
                  "type": "cgMLST", "loci": 100, "url": "https://example.org/x"}]
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

    def test_shipped_curation_is_valid(self):
        self.assertEqual(si.validate(si.read_curation()), [])

    def test_shipped_curation_is_in_canonical_order(self):
        # Otherwise every weekly run would rewrite the file and open a pull
        # request without any provider change. Fix with `scheme_index.py build`.
        rows = si.read_curation()
        self.assertEqual([r["source_id"] for r in rows], [r["source_id"] for r in si.canonical(rows)])

    def test_reviewing_a_row_does_not_move_it(self):
        rows = [row(source_id="2/1", status="include"), row(source_id="18/1", status="pending")]
        reviewed = [dict(rows[0]), dict(rows[1], status="exclude")]
        self.assertEqual([r["source_id"] for r in si.canonical(rows)],
                         [r["source_id"] for r in si.canonical(reviewed)])


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
                self.assertEqual(si.update(str(report)), 0)
            finally:
                si.CURATION, si.INDEX, si.CONFIG, si.FETCHERS["cgmlst_org"] = saved
            self.assertEqual(si.read_curation(curation)[0]["listed"], "yes")
            self.assertIn("failed - connection refused", report.read_text())


class RobotsTests(unittest.TestCase):
    def test_disallowed_host_stops_the_source(self):
        client = si.Client("test-agent", 0)
        parser = si.urllib.robotparser.RobotFileParser()
        parser.parse(["User-agent: *", "Disallow: /"])
        client._robots["https://rest.example.org"] = parser
        with self.assertRaises(si.SourceError):
            client.text("https://rest.example.org/db")


if __name__ == "__main__":
    unittest.main()
