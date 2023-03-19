#!/bin/bash

#PBS -N MG3_KERNEL 
#PBS -A NTDD0004
#PBS -l select=1:ncpus=1:mpiprocs=1:mem=300GB:ngpus=1:mps=1
#PBS -l gpu_type=v100
#PBS -l walltime=00:30:00
#PBS -q casper 
#PBS -j oe
#PBS -k eod

ulimit -s unlimited

cd $PBS_O_WORKDIR

#####################
# Step 1: Env setup #
#####################

# unload any modules currently loaded
module purge

# load modules
module load ncarenv/1.3
module load nvhpc/22.2
module load openmpi/4.1.1
module load ncarcompilers/0.5.0
module load cuda/11.4.0

########################################################
# Step 2: Set wrapper script for MPI+GPU configuration #
########################################################

let ngpus=1    # number of GPUs used in this kernel run

echo '#!/bin/bash'                       > set_device_rank.sh
echo 'unset CUDA_VISIBLE_DEVICES'        >> set_device_rank.sh

let tmp=$ngpus-1
for k in `seq 0 $tmp`
do
    echo "unset CUDA_VISIBLE_DEVICES$k" >> set_device_rank.sh
done

echo "let ngpus=$ngpus"                               >> set_device_rank.sh
echo 'let dev_id=$OMPI_COMM_WORLD_LOCAL_RANK%$ngpus'  >> set_device_rank.sh
echo 'export ACC_DEVICE_NUM=$dev_id'                  >> set_device_rank.sh
echo 'export CUDA_VISIBLE_DEVICES=$dev_id'            >> set_device_rank.sh
echo 'exec $*'                                        >> set_device_rank.sh

chmod +x set_device_rank.sh

####################################################################
# Step 3: Build and run kernels with user-specified configurations # 
####################################################################

let pcol=16    # pcols value for input data
let ntask=36   # ntasks value for input data

# add a loop to compile the code with different number of mpi ranks
for n in 1 
do
    # add a loop to compile the code with different dfact
    for i in 36 # 4608
    do
        # compile the code
        make clean
        make ntasks=$ntask pcols=$pcol dfact=$i
   
        # set wrapper script for Nsys
        echo '#!/bin/bash' > run_ncu.sh
        echo "ncu --target-processes all --set full --kernel-id ::regex:\"(micro_mg3_0_sedimentation_4347_gpu)\": -f -o openacc_mpiranks${n}_dfact${i}_rank%q{OMPI_COMM_WORLD_RANK}" '$*' >> run_ncu.sh 
        chmod +x run_ncu.sh
exit
        # run the code
        mpirun -n $n ./set_device_rank.sh ./run_ncu.sh ./kernel.exe

        # clean the files
        make clean
        rm -f ./run_ncu.sh
    done # loop for i
done # loop for n

rm -f ./set_device_rank.sh
