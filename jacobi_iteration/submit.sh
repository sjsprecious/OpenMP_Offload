#!/bin/bash -l
#PBS -N GPU_Jacobi_iteration
#PBS -A NTDD0004
#PBS -l select=1:ncpus=36:mpiprocs=36:mem=300GB:ngpus=1
#PBS -l gpu_type=v100
#PBS -l walltime=04:00:00
#PBS -q casper 
#PBS -j oe
#PBS -k eod

# Load the necessary modules (software)
module purge
module load ncarenv/1.3
module load nvhpc/21.9
module load cuda/11.4.0
module load ncarcompilers/0.5.0
module list

# Update LD_LIBRARY_PATH so that cuda libraries can be found
export LD_LIBRARY_PATH=${NCAR_ROOT_CUDA}/lib64:${LD_LIBRARY_PATH}
echo ${LD_LIBRARY_PATH}
nvidia-smi

# Move to the correct directory and run the executable
echo -e "\nBeginning code output:\n-------------\n"

export OMP_NUM_THREADS=1

for i in 128 256 512 1024 2048 4096
do
  ./jacobi_iteration.exe $i $i
done
