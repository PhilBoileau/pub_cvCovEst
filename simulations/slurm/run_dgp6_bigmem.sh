#!/bin/bash
# Job name:
#SBATCH --job-name=cvCovEst_dgp6_bigpn
#
# Working directory:
#SBATCH --chdir=/global/home/users/nhejazi/
#
# Account:
#SBATCH --account=co_biostat
#
# Partition:
#SBATCH --partition=savio3_bigmem
#
# Processors (1 node = 20 cores):
#SBATCH --nodes=1
#SBATCH --exclusive
#
# Wall clock limit ('0' for unlimited):
#SBATCH --time=48:00:00
#
# Mail type:
#SBATCH --mail-type=all
#
# Mail user:
#SBATCH --mail-user=nhejazi@berkeley.edu
#
# Job output:
#SBATCH --output=slurm.out
#SBATCH --error=slurm.out
#
## Command(s) to run:
export TMPDIR='/global/scratch/nhejazi/rtmp'  # resolve update issues for compiled packages as per https://github.com/r-lib/devtools/issues/32
export R_LIBS_USER='/global/scratch/nhejazi/R'  # personal package library
#export MKL_THREADING_LAYER='sequential'  # https://github.com/HenrikBengtsson/future/issues/405
#export MKL_NUM_THREADS='1'  https://docs-research-it.berkeley.edu/services/high-performance-computing/user-guide/using-software/using-r-savio
#export OMP_NUM_THREADS='1'  # https://github.com/wrathematics/coop/issues/13
module load openblas/0.2.20 gcc/6.3.0 r/3.6.3 r-packages/default
cd ~/cv-cov-est-selector/simulations/

R CMD BATCH --no-save --no-restore \
  '--args n_obs=500 pn_ratio=5 sim_type=6 future_workers=20' \
  R/03_run_simulations.R logs/09f_dgp6_bigpn.Rout
