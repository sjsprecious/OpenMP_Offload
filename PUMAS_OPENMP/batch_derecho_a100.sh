#!/bin/bash
#PBS -N MG3_KERNEL 
#PBS -A NTDD0004
#PBS -l select=1:ncpus=1:mpiprocs=1:mem=230GB:ngpus=1:mps=1
#PBS -l walltime=00:30:00
#PBS -q main 
#PBS -j oe
#PBS -k eod

ulimit -s unlimited

# unload any modules currently loaded
module purge

# load modules
module load ncarenv/23.06
module load nvhpc/23.5
module load cray-mpich/8.1.25
module load cuda/11.7.1
module load ncarcompilers/1.0.0

cd $PBS_O_WORKDIR
let pcol=16      # pcols value for input data
let ntask=36     # ntasks value for input data
let ppn=1        # number of MPI ranks per node for job execution

# add a loop to compile the code with different number of mpi ranks
for n in 1
do
    # add a loop to compile the code with different dfact
    for i in 1 2 4 8 16 32 64 128 256
    do
        # compile the code
        make clean
        make ntasks=$ntask pcols=$pcol dfact=$i

        # run the code
        mpiexec -n $n -ppn $ppn get_local_rank ./kernel.exe >& derecho_nvhpc_openacc_a100_mpiranks${n}_pcols${pcol}_dfact${i}_log

        # clean the files
        make clean
    done # loop for i
done # loop for n
