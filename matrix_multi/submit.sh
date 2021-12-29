#!/bin/bash -l
#PBS -N GPU_matrix_mult 
#PBS -A NTDD0004
#PBS -l select=1:ncpus=36:mpiprocs=36:mem=300GB:ngpus=1
#PBS -l gpu_type=v100
#PBS -l walltime=01:59:00
#PBS -q casper 
#PBS -j oe
#PBS -k eod

# Run everything in the HPE container
CRAYENV_GPU_SUPPORT=1 crayenv << EOF
# Load CUDA module
module load craype-accel-nvidia70
module unload cray-libsci_acc
module load cudatoolkit
 
# Move to the correct directory and run the executable
echo -e "\nBeginning code output:\n-------------\n"
 
export OMP_NUM_THREADS=1

./matrix_mult.exe 1024 1024 1024 1024
./matrix_mult.exe 2048 2048 2048 2048
./matrix_mult.exe 4096 4096 4096 4096
./matrix_mult.exe 8192 8192 8192 8192

EOF
