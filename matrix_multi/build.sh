#!/bin/bash

# Load the necessary modules (software)
module purge
module load ncarenv/1.3
module load nvhpc/21.9
module load cuda/11.4.0
module load ncarcompilers/0.5.0
module list

# Export variables for use in the Makefile
export CUDA_ROOT_PATH="${NCAR_ROOT_CUDA}"
export NVHPC_ROOT_PATH="${NCAR_ROOT_NVHPC}/Linux_x86_64/21.9/compilers"
# Remove any previous build attempts
make clean
# Do a build
make _OPENACC=true
