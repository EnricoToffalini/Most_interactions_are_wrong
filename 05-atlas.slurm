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

# Disabilita il multi-threading automatico delle librerie algebriche sottostanti (BLAS, OpenMP, MKL)
# per evitare sovrapposizioni e contesa di thread con clusterApplyLB / worker R
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1

# Configurazione Atlas via variabili d'ambiente (default: MODE="full", RUN_DHARMA="FALSE")
export ATLAS_MODE="full"
export ATLAS_RUN_DHARMA="FALSE"

# Attiva l'ambiente Conda
source ~/miniconda3/etc/profile.d/conda.sh
conda activate r460

# Esecuzione pipeline simulation-atlas
srun Rscript --vanilla simulation-atlas/01-build-scenario-grid.R
srun Rscript --vanilla simulation-atlas/02-run-atlas_par.R
srun Rscript --vanilla simulation-atlas/03-summarize-atlas.R
