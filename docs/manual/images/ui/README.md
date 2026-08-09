# UI screenshots for the user manual

Every file in this folder is currently a **placeholder**. Replace each one with a
real screenshot **under the same filename** and the manual picks it up with no
edit to any `.qmd` — the filenames are the contract.

## Before you start

- **One database, one session.** Load a database with enough isolates to make the
  plots look real (30–80 works well; a five-isolate MST looks like a bug) and
  take everything in one pass.
- **Light mode**, unless you intend to redo the whole set in dark. The manual's
  own theme is light.
- **Browser zoom at 100 %**, window maximised, and hide the browser chrome if you
  can — a clean region capture beats a full-window one.
- **PNG**, and roughly the width given below. Exact pixel sizes do not matter
  (the manual scales them); the *aspect* does, so crop to the region named rather
  than padding it out.
- **No real patient data.** Isolate names and metadata are visible in most of
  these. Use a demo database or rename first.
- If a capture would be unreadably wide, crop to the part the caption talks
  about rather than shrinking the whole panel.

## The list

Ordered so you can walk the app once, top to bottom, without backtracking.

### Start screen — before a database is loaded

| # | File | What to capture |
|---|---|---|
| 1 | `navbar-start.png` | The **navigation bar only**, full width, at startup. Must show the *Load Database* and *Scheme Browser* tabs on the left and the version number, moon icon and power icon on the right. Crop to a thin band ~1600 × 260. |
| 2 | `landing.png` | The whole **Load Database** panel **with a database already selected**, so the file-details table and the enabled *Load Database* button are both visible. |
| 3 | `scheme-browser.png` | The whole **Scheme Browser** with a species selected: *Select Scheme*, *Scheme Metadata*, *Initiate New Database* and the *Details* card with its photo. Ideally with a target folder already chosen so *Target location* shows a path. |

### After loading

| # | File | What to capture |
|---|---|---|
| 4 | `navbar-workspace.png` | The **navigation bar only** with a database open: the four workspace tabs, the return arrow, and the database filename on the right. Same thin band as #1. |

### Add Isolates

| # | File | What to capture |
|---|---|---|
| 5 | `typing-sidebar.png` | The **left sidebar only**, with all four numbered steps open, plus *Start Analysis* and *Terminate* below them. Tall crop (~660 × 1100). |
| 6 | `typing-results.png` | The **main area during or just after a run**: the progress bar, the status line, and the *Results* accordion open showing several completed rows. Try to include at least one amber or red *Completeness* badge — that is what the text talks about. Scroll the table right if needed so the *MLST* and *AMR* columns are in frame. |

### Database Browser

| # | File | What to capture |
|---|---|---|
| 7 | `browse-entries.png` | **Browse Entries**, whole panel: the metadata grid plus the right sidebar (*Column Selection*, *Edit*, *Remove isolates*). Ideally with one cell being edited so *Save Changes* and *Discard* are enabled. |
| 8 | `custom-variables.png` | **Custom Variables**, whole panel, on the **Variables** tab, with two or three variables of different types defined. |
| 9 | `custom-variable-modal.png` | The **New custom variable** dialogue, with type set to **Category** so the *Allowed values* field is visible. Crop to the dialogue only. |
| 10 | `cgmlst-scheme-info.png` | **cgMLST Overview > Scheme Info**, whole panel: *Scheme Metadata* table, species photo and *Details*. |
| 11 | `cgmlst-loci-info.png` | **cgMLST Overview > Loci Info**, whole panel, with a locus selected so the colour-coded sequence and the *Sequence / Index / Locus* buttons are showing. |
| 12 | `import.png` | **Import** with source *PhyloTrace database (.db)* and a peer database selected, so the compatibility check grid is filled in and the sidebar pickers are populated. |
| 13 | `import-collisions.png` | The **per-isolate clash rows** from that same screen — the dropdown offering import/skip/rename and the pre-filled `_ext` name. Crop to just those rows plus their heading. If you cannot produce a real clash, import a database into itself after renaming one isolate. |
| 14 | `export.png` | **Export**, whole panel, with *Export type* = **PhyloTrace database (.db)**, some isolates selected, and the live summary showing counts in the main area. |

### Visualization

| # | File | What to capture |
|---|---|---|
| 15 | `viz-new-plot.png` | The **New** tab: the five type tiles, the description card with its two badges, and the *Plot name* / *Initiate plot* form. Have **MST** selected. |
| 16 | `viz-tab-layout.png` | A **generated MST plot tab, whole width** — left setup sidebar, figure, right control panel, all three visible at once. This is the "here is the anatomy of a plot tab" figure, so do not crop the sidebars off. |
| 17 | `viz-selection-modal.png` | The **isolate selection dialogue** with some rows ticked, the count showing, and the date-column filter visible in the footer. |
| 18 | `viz-export-modal.png` | The **Export plot** dialogue with the **Advanced** section expanded, so *Width (cm)* and *Resolution* (or *Target width (px)*) and the hint line are all visible. |

### MST

| # | File | What to capture |
|---|---|---|
| 19 | `mst-plot.png` | The **MST canvas only** (no sidebars), showing: clusters shaded, a variable mapped to node fill with at least one pie node, edge distance labels, and the legend. This is the flagship figure of the chapter. |
| 20 | `mst-options-tab.png` | The **Options tab** of the MST panel with *Clustering*, *Cluster Options* and *Collapsing* all open. **Set a collapse threshold that actually bites**, so the "*n* branches collapsed, leaving *m* nodes" note is visible — the chapter explicitly describes that line. |
| 21 | `mst-nodes-edges.png` | The **Nodes** and **Edges** tabs, side by side if you can compose them, otherwise two captures pasted into one image. Edges should show *Length*, *Spread*, *Shorten long branches* and *Cap at (× median)*. |

### Tree

| # | File | What to capture |
|---|---|---|
| 22 | `tree-plot.png` | A **rectangular tree** with tip points, a colour mapping, and one **Heatmaps** panel (genes or drug classes) drawn beside the tips. Canvas only. |
| 23 | `tree-controls.png` | The tree's **right control panel** with the *Options* tab showing (Missing values, Algorithm, Tree Rooting, Layout), and the *Full / Zoomed* toggle at the bottom in frame. |

### Epi curve

| # | File | What to capture |
|---|---|---|
| 24 | `epi-plot.png` | A **stratified curve** in *Stacked* mode with a moving-average line on and at least one annotation (a milestone line or shaded period) added. Canvas only. |
| 25 | `epi-time-tab.png` | The **Time tab**: the date-range slider, the *Interval* buttons, the step arrows and *Play*, and the help text underneath. |

### Map

| # | File | What to capture |
|---|---|---|
| 26 | `map-plot.png` | The map in **Markers** mode with clustered markers visible, a legend, and the geocoding status line. Include the *Map mode* picker at the bottom of the control column if it fits. |
| 27 | `map-mode-picker.png` | The map's **control column**: the tab strip, one tab's contents (*Markers > Clustering* is the most useful), the *Map mode* picker and *Reset view* below it. |

### AMR heatmap

| # | File | What to capture |
|---|---|---|
| 28 | `amr-heatmap.png` | The **Gene heatmap** with the drug-class strip above the columns and a metadata annotation strip beside the rows, plus its legends. Canvas only. |
| 29 | `amr-data-tab.png` | The **Data tab**: *View* at the top, then *Elements* open showing element types, the gene picker and both minimum-percentage sliders. |

### Analysis Dashboard

| # | File | What to capture |
|---|---|---|
| 30 | `dashboard.png` | The dashboard with **at least two Analyses**, one holding three or more plot tiles with real thumbnails, and the *Add Plot* placeholder at the end of the row. If you can arrange a stale plot, its orange "Isolates changed" flag is worth having in frame. |
| 31 | `analysis-wizard.png` | **Step one** of the *New Analysis* dialogue — name and description. Dialogue only. |

## After replacing them

```bash
cd docs/manual
quarto render --to html      # or: quarto preview
```

Nothing else needs changing. If you decide a figure is not worth keeping, delete
its `![...](images/ui/…)` line from the chapter rather than leaving a placeholder
in place.
