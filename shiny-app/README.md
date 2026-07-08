# Companion Shiny app

This app is an explanatory companion to the paper on how link-function choice
in generalized linear models affects interaction testing in psychology. It
illustrates, with deterministic examples, that the link function defines the
scale on which the linear predictor is additive, so a zero product term means
no interaction on the link scale, not necessarily on the observed response
scale. The app performs deterministic visual calculations only: it evaluates
inverse link functions at user-chosen values. It does no model fitting and no
Monte Carlo simulation.

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

## Tabs

1. **Overview.** Distinguishes outcome family, link function, and target
   metric, and states what the app does and does not do.
2. **Link scale explorer.** Plots any of nine inverse links (identity, logit,
   probit, cloglog, log-log, cauchit, log, inverse, chance-corrected logit)
   and shows how equal eta intervals map to unequal mu intervals.
3. **Four-cell interaction calculator.** A 2x2 design with user-chosen
   coefficients; reports eta and mu per cell and the difference-in-differences
   on both the link scale (exactly beta_xz) and the observed scale.
4. **Reverse four-cell calculator.** The inverse direction: the four expected
   cell values are set directly, and a table (plus a bar chart) shows the
   model coefficients — with the product term beta_xz highlighted — that each
   link function implies for exactly those cells.
5. **Forced-choice chance floor.** Compares standard logit and
   chance-corrected logit predictions for two groups across age, holding the
   generating eta fixed.
6. **Logit vs probit.** Overlays the logit curve and a scaled probit curve,
   plots their pointwise difference, and compares observed-scale
   difference-in-differences under both links when beta_xz = 0.

## Files

- `app.R`: the app (UI and server).
- `R/app_helpers.R`: inverse-link, forward-link, and difference-in-differences
  helpers.
- `smoke-test.R`: lightweight parse and helper checks; run with
  `Rscript shiny-app/smoke-test.R` from the repository root.
