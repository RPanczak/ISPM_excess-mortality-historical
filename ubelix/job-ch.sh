#!/bin/bash
#SBATCH --job-name="EM-ch"
#SBATCH --partition=epyc2
#SBATCH --qos=job_epyc2
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=8G
#SBATCH --time=36:00:00
#SBATCH --mail-user=radoslaw.panczak@ispm.unibe.ch
#SBATCH --mail-type=all  

# win eol fix
# dos2unix $HOME/ISPM_excess-mortality/R/*.R
# dos2unix $HOME/ISPM_excess-mortality/analyses/*.R
# dos2unix $HOME/ISPM_excess-mortality/stan/*.stan

# module load R/4.0.0-foss-2020a
module load R nodejs

# R CMD BATCH --verbose --no-save --no-restore $HOME/ISPM_excess-mortality/analyses/20_joint-model-all.R
cd $HOME/ISPM_excess-mortality/
Rscript analyses/20_joint-model-stan-ch.R