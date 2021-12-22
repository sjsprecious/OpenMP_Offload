#!/bin/bash -l
#PBS -N GPU_Jacobi_iteration
#PBS -A NTDD0004
#PBS -l select=1:ncpus=36:mpiprocs=36:mem=300GB:ngpus=1
#PBS -l gpu_type=v100
#PBS -l walltime=00:30:00
#PBS -q gpudev 
#PBS -j oe
#PBS -k eod

# Run everything in the HPE container 
CRAYENV_GPU_SUPPORT=1 crayenv << EOF
# Load CUDA module
module load craype-accel-nvidia70
module unload cray-libsci_acc
module load cudatoolkit

nvidia-smi

# Move to the correct directory and run the executable
echo -e "\nBeginning code output:\n-------------\n"

export OMP_NUM_THREADS=1

for i in 128 256 512 1024
do
  ./jacobi_iteration.exe $i $i
done
EOF
