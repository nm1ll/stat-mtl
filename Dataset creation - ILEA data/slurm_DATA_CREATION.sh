#!/bin/bash
#SBATCH --job-name mtl_lambda_data_creation
#SBATCH --account=xxxxxxx
#SBATCH --mail-user xxxxxxx@unm.edu
#SBATCH --mail-type END,FAIL
#SBATCH --partition normal
#SBATCH --time 48:00:00
#SBATCH --ntasks 100
#SBATCH --cpus-per-task 1
#SBATCH --mem 8G


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
env_parallel --sshloginfile $PBS_NODEFILE --joblog jobs.log --retry-failed "Rscript --vanilla $SLURM_SUBMIT_DIR/DATASET_CREATION.R" ::: {1..100} :::: lambdas.txt

# end the script
conda deactivate