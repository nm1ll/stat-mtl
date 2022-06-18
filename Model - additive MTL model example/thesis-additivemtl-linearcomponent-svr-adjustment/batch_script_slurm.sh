#!/bin/bash
#SBATCH --job-name tune_adjustment
#SBATCH --account=2016227
#SBATCH --mail-user nikmill@unm.edu
#SBATCH --mail-type END,FAIL
#SBATCH --partition normal
#SBATCH --time 48:00:00
#SBATCH --ntasks 8
#SBATCH --cpus-per-task 8
#SBATCH --mem 15G


module purge	

# Load parallel
module load parallel/20210922-cfec
source $(which env_parallel.bash)

# load the conda module and the pre-created R environment
module load miniconda3/4.10.3-an4v
eval "$(conda shell.bash hook)"
conda activate r_mtl

# change to the directory where SLURM was ran from
cd $SLURM_SUBMIT_DIR

# run the parallelizable script
env_parallel -j 2 --sshloginfile $PBS_NODEFILE --joblog jobs.log --retry-failed "Rscript --vanilla $SLURM_SUBMIT_DIR/adjustment_svr_tuning.R" :::: lambdas.txt :::: loop.txt :::: costs.txt :::: gammas.txt
