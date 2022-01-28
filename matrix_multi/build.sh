#!/bin/bash

FCOMPILER=${1^^}         # make input capitalized
USE_OPENMP=${2:-false}   # assign default value
USE_OPENACC=${3:-false}  # assign default value
RUN_CPU=${4:-true}       # assign default value

if [ "$FCOMPILER" == "NVHPC" ]; then

   echo "Using $FCOMPILER compiler..."

   # Load the necessary modules (software)
   module purge
   module load ncarenv/1.3
   module load nvhpc/21.11
   module load cuda/11.4.0
   module load ncarcompilers/0.5.0
   module list

   # Export variables for use in the Makefile
   export CUDA_ROOT_PATH="${NCAR_ROOT_CUDA}"
   export NVHPC_ROOT_PATH="${NCAR_ROOT_NVHPC}/Linux_x86_64/21.11/compilers"

   # Remove any previous build attempts
   make clean

   # Do a build
   make _OPENMP=$USE_OPENMP _OPENACC=$USE_OPENACC _RUNCPU=$RUN_CPU _COMPILER=$FCOMPILER

elif [ "$FCOMPILER" == "CRAY" ]; then

   echo "Using $FCOMPILER compiler..."

   # Load CUDA module in CRAY env
   module load craype-accel-nvidia70
   module unload cray-libsci_acc
   module load cudatoolkit
   module li

   # Remove any previous build attempts
   make clean

   # Do a build
   make _OPENMP=$USE_OPENMP _OPENACC=$USE_OPENACC _RUNCPU=$RUN_CPU _COMPILER=$FCOMPILER

elif [ "$FCOMPILER" == "-H" ]; then
   
   echo "Usage: `basename $0` COMPILER (NVHPC|CRAY) [USE_OPENMP (true|false)] [USE_OPENACC (true|false)] [RUN_CPU (true|false)]"

else

   echo "Unsupported compiler: $FCOMPILER"
   exit 1

fi
