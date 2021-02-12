#!/bin/bash
# Job name:
#SBATCH --job-name=cvCovEst_update
#
# Account:
#SBATCH --account=co_biostat
#
# Quality of Service:
#SBATCH --qos=biostat_savio2_normal
#
# Partition:
#SBATCH --partition=savio2
#
# Processors (1 node = 20 cores):
#SBATCH --nodes=1
#SBATCH --exclusive
#
# Wall clock limit ('0' for unlimited):
#SBATCH --time=12:00:00
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
module load openblas/0.2.20 gcc/6.3.0 r/3.6.3 r-packages/default
export TMPDIR='~/rtmp'  # resolve update issues for compiled packages as per https://github.com/r-lib/devtools/issues/32
cd ~/projects/pub_cvCovEst/analysis/zeisel-example/
  
R CMD BATCH --no-save --no-restore \
  R/00_update-dependencies.R logs/00_update-dependencies.Rout
