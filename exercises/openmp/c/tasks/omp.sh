#!/bin/bash
#SBATCH -J mandelbrot
#SBATCH -p test
#SBATCH -t 00:10:00
#SBATCH -N 1

export OMP_NUM_THREADS=48

aprun -n 1 -d 24 mandelbrot
