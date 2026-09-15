# Figure type rules

Status: **proposal, 2026-09-14.** Nothing here is implemented beyond what
"Where each engine stands" marks. Applies to every figure engine: Tree, Epi
curve, AMR heatmap, AMR prevalence, MST and Map. Rule IDs (T1, G1, ...) are
meant to be cited in code comments and tests.

## Can type simply be larger everywhere?

Yes, no rule forbids it. But "larger" does different things to two kinds of
label:

- **Page-bound labels** (legend, axis numbers and titles, panel titles,
  captions) have room to spare. A larger design size makes them larger on paper
  and on screen, and the room comes out of the drawing: legends fold and trim
  sooner.
- **Room-bound labels** (tip names, isolate names, gene names, bar names,
  rotated class titles) are already as large as their row or column allows. A
  larger design size changes nothing unless the geometry grows with it (G1, as
  the AMR heatmap's gene columns now do). When it does, the page grows in the
  same proportion, so the label's share of the page stays the same.

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
  then stops; it cannot make two labels collide.
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
  (2) wrap or fold (a legend into a second column, labels into lanes);
  (3) trim (legend keys, least frequent first, with a gap key and "n of N
  shown"); (4) drop the label; (5) show its substitute.
- **T8 Every dropped label has a substitute or a switch.** Class titles become
  the class strip and its legend keys; the element-type row becomes the legend
  heading; isolate and gene names have none, so the switch stays available and
  the fit hint says why they are off.
- **T9 Reserve exactly what is drawn.** The space budgeted and the label drawn
  use the same predicate (switch and legible). Reserved but not drawn is a white
  band; drawn but not reserved is an overlap.

### Geometry

- **G1 Geometry follows text.** When a role is room-bound and its room is
  geometry the engine owns, the canvas grows along that axis so the slot holds
  the want: column pitch x k grows the width, row pitch x k grows the height.
  Only for labels that will be drawn, and only when they are legible at the
  floor within the ceiling.
- **G2 Ceilings.** Width <= base x `CANVAS_MAX_FACTOR` (2.6) x max(k, 1);
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

- **L1 Legends shrink before they trim.** Type goes to the floor before any key
  is cut (the AMR policy).
- **L2 Vocabularies stay whole.** Confidence tiers and drug classes are never
  capped; a mapped variable caps at `LEGEND_FULL_MAX` (18) keys.
- **M1 Measure what is known.** Known strings are measured with `string_em()`
  (Helvetica advances; unknown glyphs count one em). `MEAN_CHAR_EM` (0.6) is
  only for reserving room for labels not yet known.
- **M2 One solve.** View, builder, export and saved Analysis read the same
  layout result. The browser's width never drives layout.

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
| T3-T4 fit and floor | yes | yes | yes | yes | partial: fitted at Generate, 7 in print anchor | no |
| T2 shared ramp | no | no | no | no | no | no |
| G1 geometry follows text | no: tip rows ignore k | partial: annotation lanes | partial: columns yes (trial), rows no | no: ratio locked on purpose | n/a | n/a |
| L1 shrink before trim | no: trims first at 8.2 pt | partial: "+ N more" | yes | n/a | n/a | n/a |
| M2 one solve, fixed canvas | yes | yes | yes | yes | no: live vis.js widget | no: Leaflet |
| S2 Full-view size hint | no | no | no | no | no | no |

## Open decisions

1. Adopt the shared ramp, and how much larger (the table is about +10% on
   page-bound roles)?
2. **G3 conflict:** the AMR heatmap lets Text size re-fit the aspect through G1
   (1.45 to 0.84 at 200%), but the bar chart may not. One rule for both: either
   style may change the shape wherever geometry grows, or G1 grows both axes
   together and keeps the ratio.
3. Apply G1 to the Tree's tip rows, so its height grows with k?
4. Keep the canvas ceiling scaling with k (currently max(k, 1) x 2.6)?
5. Add journal-width sizes to the export note (S2)?
6. Fold the MST's three label sliders into one Text size (T1)?
7. Bring the Map into the system at all?

## Order of work, once decided

1. Role table in `viz_fit.R` with tests; engines read it, values unchanged (no
   visual change).
2. Set the values; one render pass per engine on the S. aureus database.
3. G1 per engine (Tree rows; prevalence per decision 2).
4. L1 on the Tree legend.
5. S2 hints.
6. MST, then Map.
