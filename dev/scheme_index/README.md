# Scheme index maintenance

The app's list of published typing schemes (`app/logic/data/scheme_index.csv`) is
generated from `curation.csv` by `scheme_index.py`. The app itself never contacts a
scheme provider; this tool does, weekly, from GitHub Actions
(`.github/workflows/scheme_index.yml`), and only reads scheme names, locus counts
and page addresses.

## Sources

`providers.json` is the register of providers: what each publishes, its data
terms (with the date they were reviewed), whether its scheme lists may be read
automatically, and where a permission request stands. Keep it up to date when a
provider replies or its terms change. The tool reads the allowed link hosts from
it; which providers are actually read is switched on in `config.json`, and a
test makes sure only providers marked `automated` there can be.

| Source | Status | Why |
| --- | --- | --- |
| cgMLST.org | read | The list page `/ncs/` is open to automated access; `robots.txt` blocks only `/ncs/schema/` and `/ncs/strain/`. |
| Chewie-NS | read | The API invites programmatic use. |
| PubMLST | read | Written permission from the University of Oxford (2026-11-19). Its `robots.txt` targets crawlers, not scripted API use; requests run one after another without pauses, as they asked. rMLST is never listed. |
| Institut Pasteur | read | Written confirmation from Institut Pasteur (2026-11-19) for metadata listing, sequential with pauses and a contact address. |
| EnteroBase | curated by hand | Its terms exclude business use of the service; reading it for a commercial product needs a licence from the University of Warwick (not requested). Two of its databases are hosted by the DSMZ. |

From PubMLST and Institut Pasteur only scheme metadata is read: database and
scheme names, locus counts and page links - never alleles, profiles or isolate
data. EnteroBase's schemes are entered by hand in `curation.csv`
(`source = manual`) and checked like any other row.

The tool checks `robots.txt` before every host and stops a source whose path is
disallowed, so switching a source on cannot override a provider's robots rules.
The one exception is a source whose entry in `config.json` records a written
permission (`robots_permission`), currently only PubMLST; a test makes sure no
other source has one. That permission covers this tool's API calls only: the
weekly link check (`tests/testthat/test-external_links.R`) also honours
robots.txt and so never opens the scheme pages of cgMLST.org (`/ncs/schema/`) or
PubMLST (`/bigsdb`) that the index links to; their addresses are kept current by
this tool's listing instead.

## Weekly pull request

The workflow opens (or updates) one pull request, `Scheme index: provider listing
changes`, when a listing changed:

- **New schemes** are added with `status = pending` and a suggested species and
  type. They do not reach the app until you review them.
- **Changed locus counts** of known schemes are updated in place.
- **A changed name or page address** of a scheme already marked `include` is
  written to the row, but the row goes back to `status = pending` and leaves the
  app's index until you have checked the new values and set `include` again.
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

Rows with `source = manual` (EnteroBase) are never touched by the tool. An
`exclude` row of an automated source is the tool's memory of your decision:
delete it and the scheme returns as `pending` on the next run.

## Validation

Only included rows that pass validation reach the app's index; `build` and
`update` list the others as problems and exit with status 1. The app shows each
row's `url` as a link, so it must be a plain `https` address on the provider's
own host (`PROVIDER_HOSTS` in `scheme_index.py`). Scheme names and addresses in
the pull request text are shown as code, so a provider's text cannot add links,
mentions or formatting.

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
