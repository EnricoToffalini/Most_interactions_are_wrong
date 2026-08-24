#!/bin/bash
#SBATCH --job-name="sim_within_family"
#SBATCH --partition="queue1"
#SBATCH --mem-per-cpu=2G
#SBATCH --mail-user=filippo.gambarota@unipd.it
#SBATCH --mail-type=ALL
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=90
#SBATCH --time=00:00:00

# Disabilita il multi-threading automatico delle librerie algebriche sottostanti (BLAS, OpenMP, MKL)
# per evitare sovrapposizioni e contesa di thread con mclapply
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1

# Attiva l'ambiente Conda
source ~/miniconda3/etc/profile.d/conda.sh
conda activate r460
srun Rscript --vanilla scripts/03b-simulation-within-family-links.R
