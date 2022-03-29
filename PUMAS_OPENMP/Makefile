# Makefile for KGEN-generated kernel

include ./Macros

ntasks ?= 36
pcols ?= 16
dfact ?= 1

ALL_OBJS := micro_mg_cam.o micro_mg3_0.o micro_pumas_utils.o wv_sat_methods.o shr_spfn_mod.o shr_kind_mod.o shr_const_mod.o kernel_driver.o kgen_utils.o

prefix := ./input_casper_intel19/mpirank${ntasks}_pcols${pcols}

build: ${ALL_OBJS}
	${FC} ${FC_FLAGS} -o kernel.exe $^  
	sed "s/PCOLS/${pcols}/g" kgen_statefile.template >& log
	sed "s/NTASKS/${ntasks}/g" log >& kgen_statefile.lst
	rm -f log
	mv kgen_statefile.lst ${prefix}/ 

run: build
	./kernel.exe

micro_mg_cam.o: micro_mg_cam.F90 micro_mg3_0.o micro_pumas_utils.o wv_sat_methods.o shr_spfn_mod.o shr_kind_mod.o shr_const_mod.o kgen_utils.o
	${FC} ${FC_FLAGS} -DDFACT=${dfact} -c -o $@ $<

micro_mg3_0.o: micro_mg3_0.F90 kgen_utils.o micro_pumas_utils.o wv_sat_methods.o shr_spfn_mod.o
	${FC} ${FC_FLAGS} -DDFACT=${dfact} -c -o $@ $<

micro_pumas_utils.o: micro_pumas_utils.F90 kgen_utils.o shr_spfn_mod.o
	${FC} ${FC_FLAGS} -DDFACT=${dfact} -c -o $@ $<

wv_sat_methods.o: wv_sat_methods.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -DDFACT=${dfact} -c -o $@ $<

shr_spfn_mod.o: shr_spfn_mod.F90 kgen_utils.o shr_kind_mod.o shr_const_mod.o
	${FC} ${FC_FLAGS} -DDFACT=${dfact} -c -o $@ $<

shr_kind_mod.o: shr_kind_mod.F90 kgen_utils.o
	${FC} ${FC_FLAGS} -c -o $@ $<

shr_const_mod.o: shr_const_mod.F90 kgen_utils.o shr_kind_mod.o
	${FC} ${FC_FLAGS} -c -o $@ $<

kernel_driver.o: kernel_driver.F90 micro_mg_cam.o micro_mg3_0.o micro_pumas_utils.o wv_sat_methods.o shr_spfn_mod.o shr_kind_mod.o shr_const_mod.o kgen_utils.o
	${FC} ${FC_FLAGS} -DDFACT=${dfact} -DPCOLS=${pcols} -DSTATEFILE='"${prefix}/kgen_statefile.lst"' -c -o $@ $<

kgen_utils.o: kgen_utils.f90
	${FC} ${FC_FLAGS} -c -o $@ $<

clean:
	rm -f kernel.exe *.mod ${ALL_OBJS} *.optrpt *cub *ptx
