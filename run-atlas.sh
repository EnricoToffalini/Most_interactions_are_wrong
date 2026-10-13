#!/bin/sh
# Run from the repository root. Set Atlas options in the environment first.
# FULL defaults to 3000 replications per scenario. Does not install packages.
# Set ATLAS_RUN_DHARMA=TRUE to include DHARMa checks.

set -e

Rscript simulation-atlas/01-build-scenario-grid.R
Rscript simulation-atlas/02a-run-forced-choice.R
Rscript simulation-atlas/02b-run-within-family.R
Rscript simulation-atlas/02c-run-sum-scores.R
Rscript simulation-atlas/02d-run-diagnostics.R
Rscript simulation-atlas/03-summarize-atlas.R
