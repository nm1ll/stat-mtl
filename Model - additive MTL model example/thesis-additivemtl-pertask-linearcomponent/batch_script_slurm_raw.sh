#!/bin/bash
#SBATCH --account=2016227
#SBATCH --mail-user nikmill@unm.edu
#SBATCH --mail-type END,FAIL
#SBATCH --partition normal
#SBATCH --time 48:00:00

##SBATCH --ntasks 3
##SBATCH --cpus-per-task 8

#SBATCH --ntasks 8
#SBATCH --cpus-per-task 8

#SBATCH --mem 16G

#SBATCH --job-name lincomp_all_pertask


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
env_parallel -j 2 --sshloginfile $PBS_NODEFILE --joblog jobs.log --retry-failed "Rscript --vanilla $SLURM_SUBMIT_DIR/linearcomponent_pertask_raw.R" :::: tasks.txt
