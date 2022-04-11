#!/bin/bash

for i in 1
do
    for j in 36 72 144 288 576 1152 2304 4608
    do
        let k=j/i
        mv casper_nvhpc_openacc_v10_mps_mpiranks${i}_pcols16_dfact${k}_log casper_nvhpc_openacc_v10_mpiranks${i}_pcols16_dfact${k}_log
    done
done
