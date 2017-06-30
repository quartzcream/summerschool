#!/bin/bash
#SBATCH -J oliver_test
#SBATCH -o %J.out
#SBATCH -e %J.err
#SBATCH -N 1
#SBATCH -ptest
#SBATCH -t 5
aprun -n 24 prog
