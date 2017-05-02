#!/bin/sh

#SBATCH -A p2010042
#SBATCH -p node -N 1
#SBATCH -t 16:00:00
#SBATCH -J benchmark
#SBATCH --output=benchmark.out
#SBATCH --error=benchmark.err

R --vanilla -f benchmark.r

