#!/bin/bash
#SBATCH -J mpi-oliver
#SBATCH -p test
#SBATCH -t 00:05:00
#SBATCH -N 2

aprun -n 40 heat_mpi
