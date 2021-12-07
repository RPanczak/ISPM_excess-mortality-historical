#!/bin/bash
#SBATCH --job-name="excess-mortality"
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=8G
#SBATCH --time=96:00:00

module load R nodejs
Rscript analyses/04_joint-model-boot.R
