#!/bin/bash
#SBATCH -J mpi-oliver
#SBATCH -p test
#SBATCH -t 00:05:00
#SBATCH -N 1


aprun -n 9 heat_serial
