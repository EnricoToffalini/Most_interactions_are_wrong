# Companion Shiny app

This app is an explanatory companion to *The Link Function Problem in
Psychological Interaction Testing* (`paper/paper-v2.qmd`). It illustrates, with
deterministic examples, that the link function maps the expected outcome onto
the linear predictor and thereby sets the scale on which the model is additive,
so a zero product term means no interaction on the link scale, not necessarily
on the response scale. It also displays compact, precomputed simulation-atlas
summaries when they are installed. The app itself does no model fitting and no
Monte Carlo simulation.

The app uses the paper's vocabulary throughout, and deliberately uses no
synonyms of its own: *outcome family*, *link function*, *target metric*,
*product term*, *link scale* paired with *response scale*, *pseudo-interaction*
for an interaction contrast that is zero on the known generating scale and
nonzero on another fitted scale, *link-sensitive* for the same situation in
empirical data where the generating scale is unknown, and *nominal rejection
rate* versus *pseudo-interaction detection rate* for matched and mismatched
fitted scales. Scenario and model labels reach the screen exactly as the atlas
stores them, which is exactly how the manuscript scripts write them.

## How to run

From the repository root:

```r
shiny::runApp("shiny-app")
```

## Required packages

- shiny
- ggplot2

(Only base R and stats are used otherwise. The app checks for these two
packages at startup and stops with a clear message if either is missing.)

## Relation to the paper scripts

Monte Carlo simulations remain in `scripts/`. The app never runs them. A
"Precomputed paper outputs" tab that displayed existing figures and tables
from the paper pipeline is currently hidden (its code is commented out in
`app.R` and can be restored by uncommenting).

The additional **Simulation atlas** tab explores precomputed sensitivity
slices around the manuscript anchors. Full atlas files, when available, are
read from the canonical `../simulation-atlas/data/` directory; smoke-test files
are visibly labeled and are never presented as complete results. The original
explorer remains usable when atlas summaries are absent. See
[`../simulation-atlas/README.md`](../simulation-atlas/README.md) for the
separate offline build, simulation, summarization, and archive workflow. A
standalone deployment must explicitly include `simulation-atlas/data/` beside
the app at the same repository-relative path.

## Tabs

1. **Overview.** Distinguishes outcome family, link function, and target
   metric, and states what the app does and does not do.
2. **Link scale explorer.** Plots any of nine inverse links (identity, logit,
   probit, cloglog, log-log, cauchit, log, inverse, chance-corrected logit)
   and shows how equal eta intervals map to unequal mu intervals.
3. **Four-cell interaction calculator.** A 2x2 design with user-chosen
   coefficients; reports eta and mu per cell and the difference-in-differences
   on both the link scale (exactly beta_xz) and the response scale.
4. **Reverse four-cell calculator.** The inverse direction: the four expected
   cell values are set directly, and a table (plus a bar chart) shows the
   model coefficients — with the product term beta_xz highlighted — that each
   link function implies for exactly those cells.
5. **Forced-choice chance floor.** Compares standard logit and
   chance-corrected logit predictions for two groups across age, holding the
   generating eta fixed.
6. **Within-family link choice.** Overlays the logit curve and a scaled probit
   curve, plots their pointwise difference, and compares response-scale
   difference-in-differences under both links when beta_xz = 0.
7. **Simulation atlas.** Organized around the manuscript rather than around the
   storage format, so that it can be read with the paper open. See below.

## The Simulation atlas tab

The tab is deliberately shallow: **one** control at the top selects the paper
case (Simulation 1, 2 or 3, each labeled with its figure number), and a panel
then states where to read that case in the paper, which figure the numbers
belong to, and which supplement section documents them. Three wide, full-width
pages follow, with no sidebars:

1. **What the paper prints.** The manuscript anchors only, recomputed by the
   atlas: one grouped bar chart plus a table, with the model fitted on the
   generating scale outlined and flagged. Nothing to configure.
2. **Beyond the paper's scenarios.** One small-multiples figure covers *every*
   one-dimensional sensitivity slice at once (one panel per varied parameter),
   followed by the main-effect surface. A single radio button switches both
   figures between the Monte Carlo pseudo-interaction detection rate and the
   deterministic induced product term on the fitted link scale (the quantity
   Supplement S5 tabulates). One grouped dropdown, with the manuscript anchors
   listed first, opens any individual scenario in detail.
3. **Do routine checks catch it?** The pseudo-interaction rate and every
   computed diagnostic in one figure per varied parameter, plus the AIC
   magnitude table. No controls; the page names the corresponding scenario of
   the paper's diagnostic table.

Three controls in total (case, quantity, scenario). The crosswalk to the paper
lives in `ATLAS_PAPER_GUIDE` in `R/atlas_helpers.R`: if a figure or supplement
section is renumbered in the manuscript, or a diagnostic case is renamed, that
list is the only place to update. `smoke-test.R` checks that every family
reachable from the case selector has a complete entry, and that each
`diagnostic_case` names a case that actually appears in the paper's diagnostic
table (Table 3), not a label invented by the app.

## Files

- `app.R`: the app (UI and server).
- `R/app_helpers.R`: inverse-link, forward-link, and difference-in-differences
  helpers.
- `R/atlas_helpers.R`: safe summary loading, the manuscript crosswalk
  (`ATLAS_PAPER_GUIDE`), labelling, and the cheap deterministic atlas plots.
- `../simulation-atlas/`: self-contained offline atlas code, grid, compact
  data, and archive instructions; it is not part of the Shiny runtime.
- `smoke-test.R`: lightweight parse and helper checks; run with
  `Rscript shiny-app/smoke-test.R` from the repository root.
