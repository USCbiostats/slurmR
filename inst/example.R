#!/bin/sh
#SBATCH --account=lc_ggv
#SBATCH --time=01:00:00
#SBATCH --mem-per-cpu=4G
#SBATCH --job-name=Waiting
Sys.sleep(10)
message("done.")

