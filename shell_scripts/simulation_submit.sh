#!/bin/bash
#SBATCH --job-name=sim1
#SBATCH -A project_dir
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --cpus-per-task=16
#SBATCH -t 25:59:00
#SBATCH --mem-per-cpu=10G

module load r/4.4
export OMP_NUM_THREADS=16
R CMD BATCH --no-save --no-restore example_batch_job.R
