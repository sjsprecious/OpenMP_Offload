#!/bin/bash

#PBS -N MG3_KERNEL 
#PBS -A NTDD0004
#PBS -l select=1:ncpus=36:mpiprocs=36:mem=124GB:ompthreads=1
#PBS -l walltime=11:59:00
#PBS -q economy
#PBS -j oe
#PBS -k eod

ulimit -s unlimited

# unload any modules currently loaded
module purge

# load modules
module load ncarenv/1.3
module load intel/19.1.1
module load mpt/2.22
module load ncarcompilers/0.5.0

cd $PBS_O_WORKDIR
let pcol=16    # pcols value for input data
let ntask=36   # ntasks value for input data

# add a loop to compile the code with different number of mpi ranks
for n in 36 
do
    # add a loop to compile the code with different dfact
    for i in 1 2 4 8 16 32 64 128 256
    do
        # compile the code
        make clean
        make ntasks=$ntask pcols=$pcol dfact=$i
    
        # run the code
        mpiexec_mpt ./kernel.exe >& cheyenne_intel_mpiranks${n}_pcols${pcol}_dfact${i}_log
    
        # clean the files
        make clean
    done # loop for i
done # loop for n
