#!/bin/bash
#SBATCH --job-name="sim_atlas"
#SBATCH --partition="queue1"
#SBATCH --mem-per-cpu=2G
#SBATCH --mail-user=filippo.gambarota@unipd.it
#SBATCH --mail-type=ALL
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=90
#SBATCH --time=00:00:00

set -e

# Disabilita il multi-threading automatico delle librerie algebriche sottostanti (BLAS, OpenMP, MKL)
# per evitare sovrapposizioni e contesa di thread tra i worker R paralleli
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1

# Configurazione Atlas via variabili d'ambiente (default: MODE="full", RUN_DHARMA="FALSE")
export ATLAS_MODE="full"
export ATLAS_RUN_DHARMA="TRUE"

# Full publication replication count (all families).
export N_SIM=3000
export ATLAS_OVERWRITE=TRUE

# Attiva l'ambiente Conda
source ~/miniconda3/etc/profile.d/conda.sh
conda activate r460

# Esecuzione pipeline simulation-atlas
srun Rscript --vanilla simulation-atlas/01-build-scenario-grid.R
# Run each family explicitly; 02-run-atlas.R is only a wrapper around these four.
srun Rscript --vanilla simulation-atlas/02a-run-forced-choice.R
srun Rscript --vanilla simulation-atlas/02b-run-within-family.R
srun Rscript --vanilla simulation-atlas/02c-run-sum-scores.R
srun Rscript --vanilla simulation-atlas/02d-run-diagnostics.R
srun Rscript --vanilla simulation-atlas/03-summarize-atlas.R
