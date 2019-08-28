#!/bin/sh
#SBATCH --job-name=NameOfTheJob
#SBATCH --output=logfile.out
#SBATCH --array=1-100
#SBATCH --ntasks=1
#SBATCH --partition=NameOfThePartition
/usr/usc/R/3.5.0/lib64/R/bin/Rscript --vanilla myprogam.R
