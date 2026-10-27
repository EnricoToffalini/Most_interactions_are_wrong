# Notes for coding agents

## Paper, Supplement A and Supplement B must stay congruent

Supplement B (`simulation-atlas/`) is an extension of the manuscript simulations:
it repeats the same DGPs, fitted models, diagnostics and summaries over more
levels and conditions. It must never use a different definition of anything the
paper and Supplement A report. Its anchor scenarios must reproduce the
manuscript ones.

Whenever you change a manuscript script (`scripts/`), or the methods described in
`paper/paper-v2.qmd` or `paper/Supplement-A-Technical-details.qmd`, apply the same
change to the corresponding atlas code in the same task:

| Manuscript script | Atlas counterpart |
| --- | --- |
| `scripts/03a-simulation-forced-choice.R` | `simulation-atlas/02a-run-forced-choice.R` |
| `scripts/03b-simulation-within-family-links.R` | `simulation-atlas/02b-run-within-family.R` |
| `scripts/03c-simulation-sum-scores.R` | `simulation-atlas/02c-run-sum-scores.R` |
| `scripts/04-diagnostic-worked-example.R` | `simulation-atlas/02d-run-diagnostics.R` |
| scenario parameters in any of the above | `simulation-atlas/01-build-scenario-grid.R` (values are repeated there, not read from `scripts/`) |
| summary rules (fit filters, applicability of checks, rates) | `simulation-atlas/03-summarize-atlas.R` |

Then check `simulation-atlas/README.md` and the text of
`simulation-atlas/Supplement-B-Simulation-atlas.qmd`, and update anything they
say about the changed definitions. When you finish, list any mismatch that remains
and say why it is intended. For example, the atlas draws aggregated binomial
counts in `02b`, which gives an equivalent likelihood.

## Do not run the R code

Being asked to modify scripts does **not** mean being asked to run them. The
simulations take a long time, and the authors run them themselves (locally or
via the `*.slurm` files in the repository root, `run.sh`, `run-atlas.sh`). Do not run the simulation scripts,
smoke runs included, and do not render the Quarto documents unless explicitly
asked. Check changes by reading the code. When you finish, say which scripts and
scenarios need to be rerun, and which outputs (`tables/`,
`simulation-atlas/data/`, rendered supplements) are now out of date.
