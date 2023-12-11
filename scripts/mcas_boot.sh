#!/bin/sh
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=solomonb@stanford.edu
#SBATCH --time=13-23:05 # Runtime in D-HH:MM
#SBATCH --job-name=mcas_boot
#SBATCH --nodes=1 # Ensure that all cores are reserved on one machine
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48
#SBATCH --mem=50G
#SBATCH --partition=khatrilab # Partition allocated for the lab
#SBATCH --error=./%x.err
#SBATCH --output=./%x.out

module load R/4.0.4

cd /labs/khatrilab/solomonb/mcas/scripts

# Rscript bootstrap_edge_density.R 
Rscript null_permute_precision.R 