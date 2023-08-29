#!/bin/bash
#PBS -N MG3_KERNEL 
#PBS -A NTDD0004
#PBS -l select=1:ncpus=128:mpiprocs=128:mem=230GB
#PBS -l walltime=10:30:00
#PBS -q main 
#PBS -j oe
#PBS -k eod

ulimit -s unlimited

# unload any modules currently loaded
module purge

# load modules
module load ncarenv/23.06
module load intel/2023.0.0
module load cray-mpich/8.1.25
module load ncarcompilers/1.0.0

cd $PBS_O_WORKDIR
let pcol=16      # pcols value for input data
let ntask=36     # ntasks value for input data
let ppn=128      # number of MPI ranks per node for job execution

# add a loop to compile the code with different number of mpi ranks
for n in 128
do
    # add a loop to compile the code with different dfact
    for i in 1 2 4 8 16 32 64 128 256
    do
        # compile the code
        make clean
        make ntasks=$ntask pcols=$pcol dfact=$i

        # run the code
        mpiexec -n $n -ppn $ppn ./kernel.exe >& derecho_intel_mpiranks${n}_pcols${pcol}_dfact${i}_log

        # clean the files
        make clean
    done # loop for i
done # loop for n
