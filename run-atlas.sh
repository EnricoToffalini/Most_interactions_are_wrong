#!/bin/sh
# Run from the repository root. Set Atlas options in the environment first.
# FULL defaults to 3000 replications per scenario. Does not install packages.
# Defaults match the full SLURM Atlas job; environment overrides are respected.

set -e

: "${ATLAS_MODE:=full}"
: "${ATLAS_RUN_DHARMA:=TRUE}"
: "${ATLAS_OVERWRITE:=TRUE}"
export ATLAS_MODE ATLAS_RUN_DHARMA ATLAS_OVERWRITE

Rscript --vanilla simulation-atlas/01-build-scenario-grid.R
Rscript --vanilla simulation-atlas/02a-run-forced-choice.R
Rscript --vanilla simulation-atlas/02b-run-within-family.R
Rscript --vanilla simulation-atlas/02c-run-sum-scores.R
Rscript --vanilla simulation-atlas/02d-run-diagnostics.R
Rscript --vanilla simulation-atlas/03-summarize-atlas.R
