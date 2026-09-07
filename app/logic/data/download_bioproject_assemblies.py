"""
Download the genome assemblies belonging to each species' chosen BioProject in
cgmlst_schemes.csv (the "bioproject" column, curated by hand from
fetch_bioprojects_all_levels.py's ranking output).

For every selected row, this:
  1. Finds every GenBank (GCA_) assembly that is BOTH in that row's BioProject
     AND identified as that row's species/complex (a BioProject can span more
     than one species - e.g. PRJNA288601 "CDC HAI-Seq Gram-negative bacteria" -
     so filtering by BioProject accession alone would pull in the wrong genomes).
  2. Downloads only the latest version of each matching assembly (no GCF/RefSeq
     pair, no superseded/replaced duplicate versions).
  3. Writes them as a single zip archive named "<abb>.zip" inside a folder
     named "<abb>" (the cgMLST scheme's short species code).
  4. Writes a manifest.json alongside the zip with the assembly accessions,
     the BioProject accession/URL, and publication URL(s) (PubMed, when the
     assembly_summary.txt "pubmed_id" column has one) for that BioProject.

Matching reuses the same bulk-flat-file approach as fetch_bioprojects_all_levels.py
(https://ftp.ncbi.nlm.nih.gov/genomes/genbank/bacteria/assembly_summary.txt) rather
than querying NCBI per-species, and the same species_query_names()/
GENUS_RENAME_ALIASES/download_verified() helpers are imported from that script.

Requires the NCBI `datasets` command-line tool (ncbi-datasets-cli) on PATH.

Usage:
    python3 download_bioproject_assemblies.py [CSV] [--species ABB [ABB ...]]
                                               [--out-dir DIR]
                                               [--assembly-summary PATH]
                                               [--include TYPES]

    CSV                 Path to cgmlst_schemes.csv (must have a 'bioproject'
                         column). Default: cgmlst_schemes.csv next to this script.
    --species            One or more 'abb' values to restrict the download to.
                         Default: all rows in the CSV (can be tens of GB - see
                         the script's disk-usage estimate before running unset).
    --out-dir            Directory under which per-species '<abb>/' folders are
                         created. Default: the current user's home directory.
    --assembly-summary   Path to a local copy of NCBI's bulk assembly_summary.txt
                         (downloaded and verified automatically if missing).
    --include            Data types to fetch, per `datasets download genome
                         accession --include` (default: genome, i.e. FASTA only).

Examples:
    python3 download_bioproject_assemblies.py --species Bmallei_fli Mgallisepticum
    python3 download_bioproject_assemblies.py --out-dir ~/Desktop/PhyloTrace_TEST_DATA
    python3 download_bioproject_assemblies.py

Output per selected species: <out-dir>/<abb>/<abb>.zip (genome FASTAs) and
<out-dir>/<abb>/manifest.json (accessions, BioProject URL, publication URLs);
plus <out-dir>/download_summary.json summarizing every processed row.
"""

import argparse
import csv
import json
import subprocess
import sys
import tempfile
from collections import defaultdict
from pathlib import Path

HERE = Path(__file__).parent
sys.path.insert(0, str(HERE))
from fetch_bioprojects_all_levels import (  # noqa: E402
    SUMMARY_URL, download_verified, species_query_names,
)

DEFAULT_CSV_PATH = HERE / "cgmlst_schemes.csv"
# Downloads can run to tens of GB per species, so default outside the repo
# working tree.
DEFAULT_OUT_DIR = Path.home()
DEFAULT_SUMMARY_PATH = HERE / "assembly_summary.txt"


def load_rows(csv_path):
    rows = []
    with open(csv_path) as f:
        for r in csv.DictReader(f):
            rows.append({
                "id": r[""], "raw": r["species"], "abb": r["abb"],
                "bioproject": (r.get("bioproject") or "").strip(),
                "taxa": species_query_names(r["species"]),
            })
    return rows


def find_matches(rows, summary_path):
    """One pass over assembly_summary.txt: for each row, collect the latest-version
    GCA assemblies that are in that row's BioProject AND match its species/genus,
    plus any PubMed IDs attached to those assemblies."""
    by_bioproject = defaultdict(list)
    for row in rows:
        if row["bioproject"]:
            by_bioproject[row["bioproject"]].append(row)

    matches = {row["id"]: [] for row in rows}
    pubmed_ids = {row["id"]: set() for row in rows}

    with open(summary_path) as f:
        for line in f:
            if line.startswith("#"):
                continue
            fields = line.rstrip("\n").split("\t")
            if len(fields) < 38:
                continue
            bioproject = fields[1]
            candidates = by_bioproject.get(bioproject)
            if not candidates:
                continue
            assembly_accession = fields[0]
            organism_name = fields[7]
            version_status = fields[10]
            pubmed_id = fields[37]
            if version_status != "latest":
                continue
            words = organism_name.split(" ")
            if not words:
                continue
            genus = words[0]
            two_word = tuple(words[:2]) if len(words) >= 2 else None
            for row in candidates:
                taxa_set = set(row["taxa"])
                is_match = genus in taxa_set or (two_word and " ".join(two_word) in taxa_set)
                if not is_match:
                    continue
                matches[row["id"]].append(assembly_accession)
                if pubmed_id and pubmed_id != "na":
                    pubmed_ids[row["id"]].add(pubmed_id)

    return matches, pubmed_ids


def download_species(row, accessions, pubmed_ids, out_dir, include):
    abb = row["abb"]
    species_dir = out_dir / abb
    species_dir.mkdir(parents=True, exist_ok=True)
    zip_path = species_dir / f"{abb}.zip"
    manifest_path = species_dir / "manifest.json"

    bioproject_url = f"https://www.ncbi.nlm.nih.gov/bioproject/{row['bioproject']}"
    publication_urls = sorted(
        f"https://pubmed.ncbi.nlm.nih.gov/{pmid}/" for pmid in pubmed_ids
    )

    manifest = {
        "species": row["raw"].replace("_", " "),
        "abb": abb,
        "bioproject_accession": row["bioproject"],
        "bioproject_url": bioproject_url,
        "publication_urls": publication_urls,
        "n_assemblies": len(accessions),
        "assembly_accessions": accessions,
    }

    if not accessions:
        print(f"[{abb}] no matching assemblies found for BioProject "
              f"{row['bioproject']} - skipping download, writing empty manifest")
        with open(manifest_path, "w") as f:
            json.dump(manifest, f, indent=2)
        return manifest

    print(f"[{abb}] downloading {len(accessions)} assemblies from "
          f"{row['bioproject']} -> {zip_path}")
    with tempfile.NamedTemporaryFile("w", suffix=".txt", delete=False) as tmp:
        tmp.write("\n".join(accessions))
        tmp_path = tmp.name
    try:
        subprocess.run(
            ["datasets", "download", "genome", "accession",
             "--inputfile", tmp_path,
             "--assembly-source", "GenBank",
             "--assembly-version", "latest",
             "--include", include,
             "--filename", str(zip_path),
             "--no-progressbar"],
            check=True,
        )
    finally:
        Path(tmp_path).unlink(missing_ok=True)

    with open(manifest_path, "w") as f:
        json.dump(manifest, f, indent=2)
    print(f"[{abb}] done: {zip_path} ({zip_path.stat().st_size} bytes), "
          f"{len(publication_urls)} publication URL(s)")
    return manifest


def parse_args():
    parser = argparse.ArgumentParser(
        description=(
            "Download the assemblies from each species' chosen BioProject in "
            "cgmlst_schemes.csv, one zip per species folder (named by 'abb')."
        )
    )
    parser.add_argument(
        "csv", nargs="?", default=str(DEFAULT_CSV_PATH),
        help="Path to cgmlst_schemes.csv (must have a 'bioproject' column). "
             f"Default: {DEFAULT_CSV_PATH}",
    )
    parser.add_argument(
        "--species", nargs="*", default=None,
        help="One or more 'abb' values to restrict the download to. "
             "Default: all species/rows in the CSV.",
    )
    parser.add_argument(
        "--out-dir", default=str(DEFAULT_OUT_DIR),
        help=f"Directory under which per-species '<abb>/' folders are created. "
             f"Default: {DEFAULT_OUT_DIR}",
    )
    parser.add_argument(
        "--assembly-summary", default=str(DEFAULT_SUMMARY_PATH),
        help="Path to a local copy of NCBI's bulk assembly_summary.txt "
             "(downloaded and verified automatically if missing).",
    )
    parser.add_argument(
        "--include", default="genome",
        help="Data types to download, as accepted by `datasets download genome "
             "accession --include` (comma-separated: genome,rna,protein,cds,gff3,"
             "gtf,gbff,seq-report,all,none). Default: genome (FASTA only).",
    )
    return parser.parse_args()


def main():
    args = parse_args()
    csv_path = Path(args.csv)
    out_dir = Path(args.out_dir)
    summary_path = Path(args.assembly_summary)

    rows = load_rows(csv_path)

    if args.species is not None:
        wanted = set(args.species)
        known = {row["abb"] for row in rows}
        unknown = wanted - known
        if unknown:
            sys.exit(f"Unknown --species value(s) (no matching 'abb' in {csv_path}): "
                      f"{sorted(unknown)}")
        rows = [row for row in rows if row["abb"] in wanted]

    missing_bioproject = [row["abb"] for row in rows if not row["bioproject"]]
    if missing_bioproject:
        print(f"Skipping {len(missing_bioproject)} row(s) with no 'bioproject' "
              f"set: {missing_bioproject}")
    rows = [row for row in rows if row["bioproject"]]

    if not rows:
        sys.exit("No rows with a 'bioproject' value to download.")

    print(f"Selected {len(rows)} species/complex row(s) for download.")

    if not summary_path.exists():
        print(f"Downloading NCBI bulk assembly summary from {SUMMARY_URL} ...")
    download_verified(SUMMARY_URL, summary_path)

    print(f"Scanning {summary_path} for matching assemblies...")
    matches, pubmed_ids = find_matches(rows, summary_path)

    out_dir.mkdir(parents=True, exist_ok=True)
    manifests = []
    for row in rows:
        manifest = download_species(
            row, matches[row["id"]], pubmed_ids[row["id"]], out_dir, args.include,
        )
        manifests.append(manifest)

    summary_path_out = out_dir / "download_summary.json"
    with open(summary_path_out, "w") as f:
        json.dump(manifests, f, indent=2)
    print(f"Wrote {summary_path_out}")


if __name__ == "__main__":
    main()
