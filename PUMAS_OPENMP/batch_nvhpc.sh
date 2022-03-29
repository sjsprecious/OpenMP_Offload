#!/bin/bash

#PBS -N MG3_KERNEL 
#PBS -A NTDD0004
#PBS -l select=1:ncpus=36:mpiprocs=36:mem=300GB:ngpus=1
#PBS -l gpu_type=v100
#PBS -l walltime=23:30:00
#PBS -q casper 
#PBS -j oe
#PBS -k eod

ulimit -s unlimited

# unload any modules currently loaded
module purge

# load modules
module load ncarenv/1.3
module load nvhpc/22.2
module load openmpi/4.1.1
module load ncarcompilers/0.5.0
module load cuda/11.4.0

cd $PBS_O_WORKDIR
let pcol=16    # pcols value for input data
let ntask=36   # ntasks value for input data

nvidia-cuda-mps-control -d && echo "MPS control daemon started"

# add a loop to compile the code with different number of mpi ranks
for n in 1
do
    # add a loop to compile the code with different dfact
    for i in 4608 # 36 72 144 288 576 1152 2304 4608 9216 
    do
        # compile the code
        make clean
        make ntasks=$ntask pcols=$pcol dfact=$i
    
        # run the code
        mpirun -n $n ./kernel.exe >& casper_nvhpc_openacc_mpiranks${n}_pcols${pcol}_dfact${i}_log
    
        # clean the files
        make clean
    
    done # loop for i

done # loop for n
