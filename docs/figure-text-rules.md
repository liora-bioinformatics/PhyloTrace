# Figure type rules

Status: **proposal, 2026-09-14; partly implemented and decided since** (dated
where decided). Nothing here is implemented beyond what "Where each engine
stands" marks; "Decision log" records what changed and why. Applies to every
figure engine: Tree, Epi curve, AMR heatmap, AMR prevalence, MST and Map. Rule
IDs (T1, G1, ...) are meant to be cited in code comments and tests.

## Can type simply be larger everywhere?

Yes, no rule forbids it. But "larger" does different things to two kinds of
label:

- **Page-bound labels** (legend, axis numbers and titles, panel titles,
  captions) have room to spare. A larger design size makes them larger on paper
  and on screen, and the room comes out of the drawing: legends fold and trim
  sooner.
- **Room-bound labels** (tip names, isolate names, gene names, bar names,
  rotated class titles) are already as large as their row or column allows. A
  larger design size changes nothing unless the geometry grows with it (G1).
  Growing the page pays only where it changes the page's shape: the Full view
  scales any page into the stage, so a page grown on both axes alike, or
  widened past the stage's shape, shows every label at the same size as
  before.

Worked example: AMR heatmap, 990 isolates x 96 genes, in an example stage of
1400 x 900 CSS px.

| Text size | Canvas (in) | Gene names | Full view px/in | On screen | Printed 183 mm wide |
|---|---|---|---|---|---|
| 100% | 20.9 x 30.3 | 7 pt | 29.7 | 2.9 px | 2.4 pt |
| 200% | 41.8 x 35.1 | 14 pt | 25.6 | 5.0 px | 2.4 pt |

The on-screen gain at 200% comes from the page's shape: its lower aspect fills
a wide stage better. The label is no larger relative to the page. Printed at a
journal's full width, 96 gene columns are under the floor at any Text size;
only fewer columns or a larger print format fixes that.

So what limits a larger global size is geometry, not a rule. The places where
it does collide with a rule are the ceilings (G2), the aspect rule (G3), and
the floor being measured at canvas width rather than at print width (S1).

## Vocabulary

- **Canvas**: the page in inches the figure is drawn on. Preview, export,
  thumbnail and saved Analysis are one drawing of it.
- **Stage**: the on-screen box. The Full view scales the canvas into it
  (`object-fit: contain`); the Zoom view draws it at 192 CSS px per inch.
- **Role**: what a piece of text is for (row label, group title, legend key...).
  Every piece of text has exactly one.
- **Design size**: the role's size at Text size 100%.
- **k**: Text size as a multiplier, 0.6 to 2.
- **Want**: design size x k.
- **Slot / room**: the space a label is drawn in, and the largest type it holds.
- **Floor**: `viz_fit$MIN_PRINT_PT`, 5 pt.
- **Drawn**: the reader's switch is on and the fitted size is at least the floor.

## Rules

### Type

- **T1 One control.** Each figure has one Text size (60-200%, step 5) that
  multiplies every role and nothing else. No per-role size sliders: a second
  control only asks for a size the slot cannot hold. Generate, Auto-fit and
  reset return it to 100%.
- **T2 One ramp.** Design sizes come from one shared role table in
  `viz_fit.R`, not from engine constants. A new kind of text maps to an
  existing role; an engine does not invent a size.
- **T3 As big as possible.** `size = min(want, room)`. Raising k fills gaps and
  then stops; it cannot make two labels collide, and it never sets another
  label smaller. Where labels share one dimension (AMR: the body, the isolate
  names and the legend share the width), each keeps at least what it had at
  100% on the page as it was at 100%, and only the canvas's growth is shared
  out. On a page that does not grow, a room-bound label stays at its 100%
  size.
- **T4 As small as necessary.** A label shrinks only to the floor. Where the room
  is under the floor the label is not drawn. It is never set below the floor.
- **T5 Room is measured where the label is drawn.** The drawn row or column
  pitch (not the nominal one), the lane, the band, the legend column. A rotated
  label's room is its line height across the pitch. Fill factors: 0.72 for row
  and column labels, 0.85 for titles between neighbours, 0.9 for bar names.
- **T6 The hierarchy survives fitting.** After fitting, panel title >= group
  title >= data label, and legend title >= legend key, unless a higher role's
  own room forces it smaller.
- **T7 Sacrifice order, cheapest first.** (1) shrink towards the floor;
  (2) wrap or fold (labels into lanes); (3) trim (legend keys, least frequent
  first, with a gap key and "n of N shown"); (4) drop the label; (5) show its
  substitute. Legends are the exception: wrapping into width the legend column
  already owns takes nothing from the drawing, so it comes first (L1).
- **T8 Every dropped label has a substitute or a switch.** Class titles become
  the class strip and its legend keys; the element-type row becomes the legend
  heading; isolate and gene names have none, so the switch stays available and
  the fit hint says why they are off.
- **T9 Reserve exactly what is drawn.** The space budgeted and the label drawn
  use the same predicate (switch and legible). Reserved but not drawn is a white
  band; drawn but not reserved is an overlap.

### Geometry

- **G1 Geometry follows text, by shape only.** When a role is room-bound and
  its room is geometry the engine owns, the canvas may grow along that axis so
  the slot holds the want (column pitch x k grows the width), but only while
  that moves the page towards the stage's shape (`viz_fit$STAGE_ASPECT`, 0.7,
  height over width): a page is widened only up to `height / STAGE_ASPECT`,
  and its height stays what the rows need. Conditions: k > 1 (smaller text
  never shrinks the page, which the Full view would scale straight back up);
  the aspect is the fit's, not the reader's (G4, since a held ratio would grow
  the height with the width); the labels will be drawn and are legible at the
  floor within the ceiling. Decided 2026-11-20 from renders of the test
  databases: growing both axes alike changed nothing on screen, and widening an
  already-flat page only shrank its other labels.
- **G2 Ceilings.** Width <= base x `CANVAS_MAX_FACTOR` (2.6), x k for G1
  growth above 100%;
  aspect <= `ASPECT_MAX` (8); each side <= `PLOT_MAX_PX` (12,000 px, 62.5 in at
  192 px/in). Past a ceiling, rows or columns share what there is and T3-T8
  decide.
- **G3 A fitted aspect follows the data's shape.** It re-fits when counts change
  (rows, columns, bars, levels). Style inputs (Text size, colours, filters that
  keep the counts) resize inside the ratio in force. Each engine lists its shape
  triggers.
- **G4 The reader's hand wins.** A value the reader set (aspect, a label switch)
  stays until Generate, Auto-fit or reset. The fit reports through a hint
  rather than overriding.

### Screen and print

- **S1 One type, three reading sizes.**
  - Print: pt at canvas width, what the export writes.
  - Full view: pt / 72 x min(stage width / canvas width, stage height / canvas
    height) CSS px.
  - Zoom view: pt / 72 x 192 CSS px, so 1 pt is about 2.7 px.

  The floor protects print at canvas width only.
- **S2 Say when a size is lost.** The fit hint for a dropped role exists today.
  Proposed: a Full-view hint when the smallest drawn role falls under about 6
  CSS px ("Zoom to read gene names"), and an export note giving the smallest
  type at a journal width (89 or 183 mm) as well as at canvas width.

### Legends and measurement

- **L1 Legends wrap, then shrink, then trim.** A legend first wraps into
  further columns within the room beside the drawing (AMR: up to
  `AMR_LEGEND_WRAP_MAX`, 3; MST: `MST_LEGEND_WRAP_MAX`, 3), then its type goes
  to the floor, and only then is any key cut. Decided 2026-11-20: on the test
  databases the order mattered only for short pages with long legends (9 pt
  wrapped against 6.8 pt in one column). Its room is measured where it is
  drawn (T5):
  - Height: from where the column starts to the page foot. Labels under the
    drawing that sit to the legend's left (AMR gene names, element row) do not
    take from it.
  - Width: what the drawing leaves (M3), not a fixed share.
  - Rows: a guide folded into two key columns costs the rows it is drawn in.
    `viz_legend` charges one column per guide on purpose; an engine whose
    legend width is checked separately (AMR) credits the fold.
  - Widths are measured from the keys shown and the title as drawn ("9 of 81
    shown"), and the gutter between key columns adds the metric slack, or a
    long key runs into the next column's swatch.
- **L2 Vocabularies stay whole.** Confidence tiers and drug classes are never
  capped; a mapped variable caps at `LEGEND_FULL_MAX` (18) keys.
- **M1 Measure what is known.** Known strings are measured with `string_em()`
  (Helvetica advances; unknown glyphs count one em). `MEAN_CHAR_EM` (0.6) is
  only for reserving room for labels not yet known.
- **M2 One solve.** View, builder, export and saved Analysis read the same
  layout result. The browser's width never drives layout.
- **M3 Measure the split, not a share.** Where a drawing library lays parts
  out side by side and gives the main panel what is left (ComplexHeatmap gives
  the AMR body what the strips, isolate names, legend and padding leave), the
  fit predicts that remainder from the parts it plans and solves the sizes
  again against it until the two agree. The AMR fit used a fixed 62% share: on
  the test databases the drawn body was 28% narrower (S. capitis, isolate names
  on) to 36% wider (Klebsiella) than planned, so gene names were sized for the
  wrong columns and the legend was planned into 0.9 in while drawn 2.2 in wide.
  Decided 2026-11-20.

### Redraws

- **R1 One draw per change.** Everything a figure is built from passes one
  settled value before it is drawn: a `plot_source` reactive, debounced by
  `PLOT_SETTLE_MS` (450 ms), copied into a `reactiveVal` by an observer at
  priority 100 only when not `identical()` to the last. Preview, export and
  thumbnail read that value only. Browser echoes of `update*Input()` and
  `renderUI()` inputs mounting (NULL, then a value) each arrive in their own
  flush, and without the barrier each one drew the figure again (AMR: 5-8
  draws per Generate; Epi: 4; Map: up to 4). Generate is part of the source
  (a generation counter) so it always draws. Playback may settle at 0 ms.

## Proposed role ramp

Starting values, to be tuned on renders from the S. aureus test database.
Page-bound roles come out about 10% larger than today.

| Role | Examples | Today | Proposed at 100% | Room |
|---|---|---|---|---|
| Panel title | "Resistance" under a heatmap panel | AMR up to 12 | 12 | panel width, flat or rotated |
| Group title | drug-class titles, MST cluster names | AMR gene +3 (up to 16), MST 8 | 11 | gap to nearest neighbour x 0.85 |
| Axis title | count axis, date axis | Epi 10, prevalence 10 | 10 | page |
| Legend title | guide headings | about 9 | 10 | legend column |
| Row label | tip, isolate and bar names | Tree fitted, prevalence 10, AMR up to 13 | 10 | row pitch x 0.72 (bars 0.9) |
| Column label | gene names | AMR 7 (column-bound) | 9 | column pitch x 0.72 |
| Tick label | axis numbers, dates | Epi 9, prevalence 9, Tree 8.2 | 9 | page |
| Legend key | key labels | Tree 8.2, AMR 9, Epi 9 | 9 | legend column |
| Annotation | Epi periods, clade captions | Epi 8.5 | 9 | lane or caption column |
| Secondary label | MST distances and node labels | MST 6.5 and 7 | 8 | edge length, node spacing |

Under G2, a 96-gene heatmap already meets the width ceiling at 100%: gene names
would come out at 7.8 pt, not 9.

## Where each engine stands

| Rule | Tree | Epi | AMR heatmap | AMR prevalence | MST | Map |
|---|---|---|---|---|---|---|
| T1 one Text size | yes | yes | yes | yes | no: node, edge and cluster sliders | no |
| T3-T4 fit and floor | yes, tip rows held above 100% | yes | yes, with 100% floors above 100% | yes | partial: fitted at Generate, 7 in print anchor | no |
| T2 shared ramp | no | no | no | no | no | no |
| G1 geometry follows text | partial: annotation columns by shape, tip rows no | partial: annotation lanes | partial: columns by shape, rows no | no: ratio locked on purpose | n/a | n/a |
| L1 wrap, shrink, trim | yes: folds up to 3 key columns | yes: page grows to the stage's shape first | yes, room measured to the page foot | n/a: wraps, never trims | yes: room-based, up to 3 columns | n/a: plain Leaflet legend |
| M2 one solve, fixed canvas | yes | yes | yes | yes | no: live vis.js widget | no: Leaflet |
| M3 measured width split | n/a | n/a | yes | n/a | n/a | n/a |
| R1 one draw per change | yes | yes | yes | yes | yes | yes |
| S2 Full-view size hint | no | no | no | no | no | no |

## Open decisions

1. Adopt the shared ramp, and how much larger (the table is about +10% on
   page-bound roles)?
2. **G3 conflict:** the AMR heatmap lets Text size re-fit the aspect through G1
   (1.45 to 0.84 at 200%), but the bar chart may not. One rule for both: either
   style may change the shape wherever geometry grows, or G1 grows both axes
   together and keeps the ratio.
3. Apply G1 to the Tree's tip rows, so its height grows with k?
4. ~~Keep the canvas ceiling scaling with k?~~ Decided 2026-11-20: yes, but
   only for G1 growth, which now changes the page's shape only (see G1).
5. Add journal-width sizes to the export note (S2)?
6. Fold the MST's three label sliders into one Text size (T1)?
7. Bring the Map into the system at all?

## Order of work, once decided

1. Role table in `viz_fit.R` with tests; engines read it, values unchanged (no
   visual change).
2. Set the values; one render pass per engine on the S. aureus database.
3. G1 per engine (Tree tip rows; prevalence per decision 2).
4. ~~L1 on the Tree legend.~~ Done 2026-11-20.
5. S2 hints.
6. MST text size (decision 6), then Map.

## Decision log

**2026-11-20, AMR heatmap and every engine's legend and redraws.** Measured on
the test databases in `PhyloTrace_Test_Data` (read-only), rendered through the
app's own builders.

- *Text size had no visible effect on the AMR heatmap.* The canvas width was
  multiplied by k, and the Full view scaled the page straight back. Now the
  canvas grows with k by shape only (G1, `viz_fit$STAGE_ASPECT`), and only
  while the aspect is the fit's.
- *Mapped-variable strip names were tiny.* Strips were a fixed 0.12 in; now
  `0.16 in x k` with a floor (`.strip_in()`), the canvas growing by exactly
  that gain. The Tree's heatmap and tile columns follow k the same way
  (`tree_column_gain()`), by shape.
- *Element-type names did not grow.* Their room is now the gap to the
  neighbouring labels, leaning into the empty band under the strips, not their
  own panel's width (T5).
- *Short pages got a 5 pt, trimmed legend beside empty space.* Three causes,
  all fixed: the body was planned as 62% of the width (M3); the legend's
  height stopped at the body's foot (L1); folded guides were charged one row
  per key (L1). S. capitis: legend 5 pt with 28 of 37 keys became 6.9 pt with
  all 37, isolate names 6 to 8.9 pt, gene names 13 to 9.4 pt (they had been
  the largest type on the page, against T6). Providencia (51 genes, 51 keys,
  36-character names on 12 x 7.7 in) stays near the floor: the page is full,
  and the reader can turn isolate names off or set the aspect by hand.
- *Bigger text set other labels smaller* (seen while fixing the above):
  above 100% the AMR body and legend keep their 100% widths and only the
  canvas's growth is shared out: isolate names, then the gene columns up to
  their size x k, then the legend (T3). On the Tree, larger column headers buy
  their own height under a fitted aspect (`tree_band_extra_in()`), and under a
  hand-set one stay at their 100% depth (`tree_header_text()`), so the tip
  rows never tighten.
- *Long keys ran into the next key column.* The gutter between folded key
  columns adds the export device's metric slack (`.key_gap_in()`).
- *Figures were drawn several times per Generate.* Every engine now has the
  R1 barrier (AMR 5-8 draws, Epi 4, Map up to 4, down to 1).
- *Legends trimmed before shrinking.* The Tree (`tree_legend_size()` via
  `legend_fit`, folds up to `LEGEND_FOLD_MAX` 3) and the MST
  (`mst_legend_layout()`, room-based key budget instead of a fixed 24) now
  follow L1. The Epi legend's room is at least what keeps the page as flat as
  the stage (`0.7 x width - plot height`) before its type shrinks.
- `viz_legend$legend_key_budget()` takes an optional `guide_rows` cost; its
  default, which the Tree relies on, is unchanged.
