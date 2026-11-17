# Scheme index maintenance

The app's list of published typing schemes (`app/logic/data/scheme_index.csv`) is
generated from `curation.csv` by `scheme_index.py`. The app itself never contacts a
scheme provider; this tool does, weekly, from GitHub Actions
(`.github/workflows/scheme_index.yml`), and only reads scheme names, locus counts
and page addresses.

## Sources

Which providers are read is set in `config.json`; each entry says why.

| Source | Status | Why |
| --- | --- | --- |
| cgMLST.org | on | The list page `/ncs/` is open to automated access; `robots.txt` blocks only `/ncs/schema/` and `/ncs/strain/`. |
| Chewie-NS | on | The API invites programmatic use. |
| PubMLST | off | `rest.pubmlst.org/robots.txt` disallows all automated clients. Needs written permission from the University of Oxford. |
| Institut Pasteur | off | Its policy covers software tools. Needs written permission. |

The tool checks `robots.txt` before every host and stops a source whose path is
disallowed, so switching a source on cannot override a provider's robots rules.

## Weekly pull request

The workflow opens (or updates) one pull request, `Scheme index: provider listing
changes`, when a listing changed:

- **New schemes** are added with `status = pending` and a suggested species and
  type. They do not reach the app until you review them.
- **Changed names or locus counts** of known schemes are updated in place.
- **Schemes a provider no longer lists** get `listed = no` and drop out of the
  app's index.

## Reviewing a pull request

For each pending row in `curation.csv`:

1. Check the species (`Genus epithet`, `Genus epithet1/epithet2` for a group,
   `Genus spp` for a genus-wide scheme) and the type (`cgMLST`, `wgMLST`, `MLST`).
2. Set `status` to `include` or `exclude`, and give the reason in `note`.
   Exclude schemes that copy another provider's scheme or carry restrictive
   licences (e.g. rMLST).
3. Run `python dev/scheme_index/scheme_index.py build` and commit both files.

Rows with `source = manual` (classical MLST schemes checked by hand) are never
touched by the tool.

## Commands

```bash
python dev/scheme_index/scheme_index.py update --report report.md   # read listings, rebuild
python dev/scheme_index/scheme_index.py update --offline            # rebuild without network
python dev/scheme_index/scheme_index.py build                       # rebuild from curation.csv
python -m unittest discover -s dev/scheme_index -p 'test_*.py'      # offline tests
```

## Repository setting

The workflow opens pull requests with the built-in `GITHUB_TOKEN`. Enable
*Settings → Actions → General → Workflow permissions → Allow GitHub Actions to
create and approve pull requests*.
