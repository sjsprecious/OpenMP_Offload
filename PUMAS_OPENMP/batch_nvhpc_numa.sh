#!/bin/bash

#PBS -N MG3_KERNEL 
#PBS -A NTDD0004
#PBS -l select=1:ncpus=36:mpiprocs=36:mem=300GB:ngpus=8:mps=1
#PBS -l gpu_type=v100
#PBS -l walltime=00:10:00
#PBS -q casper 
#PBS -j oe
#PBS -k eod

#########################################
# Note: To set the NUMA node correctly, #
#       reserve a full node with 8 GPUs #
#       regardless of how many GPUs are #
#       actually used below             #
#########################################

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

let ngpus=8     # number of GPUs used in this kernel run
let nsockets=2  # number of socket per node

echo '#!/bin/bash'                       > set_device_rank.sh
echo 'unset CUDA_VISIBLE_DEVICES'        >> set_device_rank.sh

let tmp=$ngpus-1
for k in `seq 0 $tmp`
do
    echo "unset CUDA_VISIBLE_DEVICES$k"  >> set_device_rank.sh
done

echo "let ngpus=$ngpus"                               >> set_device_rank.sh

echo 'if (( $OMPI_COMM_WORLD_LOCAL_RANK < 18 )); then' >> set_device_rank.sh
echo "let lgpus=$ngpus/$nsockets"                      >> set_device_rank.sh  # gpus per socket
echo 'let dev_id=$OMPI_COMM_WORLD_LOCAL_RANK%$lgpus'   >> set_device_rank.sh
echo 'else'                                            >> set_device_rank.sh
echo "let lgpus=$ngpus-$ngpus/$nsockets"               >> set_device_rank.sh
echo 'let dev_id=4+$OMPI_COMM_WORLD_LOCAL_RANK%$lgpus' >> set_device_rank.sh
echo 'fi'                                              >> set_device_rank.sh
echo 'export ACC_DEVICE_NUM=$dev_id'                   >> set_device_rank.sh
echo 'export CUDA_VISIBLE_DEVICES=$dev_id'             >> set_device_rank.sh
echo 'echo "MPI = $OMPI_COMM_WORLD_LOCAL_RANK", "$dev_id"' >> set_device_rank.sh
echo 'nvidia-smi topo -c $OMPI_COMM_WORLD_LOCAL_RANK' >> set_device_rank.sh
echo 'exec $*'                                         >> set_device_rank.sh

chmod +x set_device_rank.sh

####################################################################
# Step 3: Build and run kernels with user-specified configurations # 
####################################################################

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
#        mpirun -n $n --map-by core --bind-to core --report-bindings ./set_device_rank.sh ./kernel.exe >& casper_nvhpc_openacc_v10_${ngpus}gpus_mpiranks${n}_pcols${pcol}_dfact${i}_log
#        mpirun -n $n --map-by numa --bind-to numa --report-bindings ./set_device_rank.sh ./kernel.exe >& casper_nvhpc_openacc_v10_${ngpus}gpus_mpiranks${n}_pcols${pcol}_dfact${i}_log
        mpirun -n $n --report-bindings ./set_device_rank.sh ./kernel.exe >& casper_nvhpc_openacc_v10_${ngpus}gpus_mpiranks${n}_pcols${pcol}_dfact${i}_log
    
        # clean the files
        make clean
    done # loop for i
done # loop for n

rm -f ./set_device_rank.sh
