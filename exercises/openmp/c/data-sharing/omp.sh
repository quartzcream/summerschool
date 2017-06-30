#!/bin/bash
#SBATCH -J omp-hello
#SBATCH -p test
#SBATCH -t 00:10:00
#SBATCH -N 1

export OMP_NUM_THREADS=4

aprun -n 1 -d 4 a
