#!/bin/bash
# Job name:
#SBATCH --job-name=prep-allen
#
# Account:
#SBATCH --account=co_biostat
#
# Partition:
#SBATCH --partition=savio2
#
# Quality of Service:
#SBATCH --qos=biostat_savio2_normal
#
# Processors (1 node = 20 cores):
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#
# Wall clock limit ('0' for unlimited):
#SBATCH --time=336:00:00
#
# Mail type:
#SBATCH --mail-type=all
#
# Mail user:
#SBATCH --mail-user=philippe_boileau@berkeley.edu
#
# Job output:
#SBATCH --output=slurm.out
#SBATCH --error=slurm.out
#
## Command(s) to run:
export R_LIBS_USER='/global/scratch/users/philippe_boileau/R'  # personal package library

module load openblas/0.2.20 r/3.6.3 r-packages/default
cd ~/projects/pub_cvCovEst/analysis/allen-example/
  
R CMD BATCH --no-save --no-restore \
  R/01_process-data.R logs/01_process-data.Rout
