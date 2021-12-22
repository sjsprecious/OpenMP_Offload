#!/bin/bash

# Check module list 
module li

# Load CUDA module
module load craype-accel-nvidia70
module unload cray-libsci_acc
module load cudatoolkit

# Remove any previous build attempts
make clean

# Do a build
make _OPENMP=true _OPENACC=false
