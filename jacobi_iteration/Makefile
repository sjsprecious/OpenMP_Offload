ifeq ($(_COMPILER),CRAY)
F90=ftn
FFLAGS= -O3
DM=

ifeq ($(_OPENMP),true)
ifeq ($(_OPENACC),true)
FCFLAGS= -target-accel=nvidia70 -h cpu=x86-skylake,acc,omp,fp3,aggress,vector3,msgs
else
FCFLAGS= -target-accel=nvidia70 -h cpu=x86-skylake,omp,fp3,aggress,vector3,msgs
endif
else ifeq ($(_OPENACC),true)
FCFLAGS= -target-accel=nvidia70 -h cpu=x86-skylake,acc,omp,fp3,aggress,vector3,msgs
else
FCFLAGS= -h cpu=x86-skylake,omp,fp3,aggress,vector3,msgs
endif

else ifeq ($(_COMPILER),NVHPC)
F90=nvfortran
FFLAGS= -O3 -Wall -Mfree
DM=

ifeq ($(_OPENMP),true)
ifeq ($(_OPENACC),true)
FCFLAGS= -tp=skylake -acc -target=gpu -gpu=cc70 -mp=gpu -Minfo=all -I${NVHPC_ROOT_PATH}/include
else
FCFLAGS= -tp=skylake -target=gpu -gpu=cc70 -mp=gpu -Minfo=all -I${NVHPC_ROOT_PATH}/include
endif
else ifeq ($(_OPENACC),true)
FCFLAGS= -acc -gpu=cc70 -Minfo=all -I${NVHPC_ROOT_PATH}/include
else
FCFLAGS=
endif

endif

ifeq ($(_RUNCPU),true)
DM += -D_CPU
endif
ifeq ($(_OPENACC),true)
DM += -D_OPENACC
endif
ifeq ($(_OPENMP),true)
DM += -D_OPENMP
endif

.PHONY: clean

# Description of necessary files and binaries
# List of object files separated by spaces (e.g. main.o device.o)
OBJECTS= main.o

# The binary to be built by this Makefile
BINARY= jacobi_iteration.exe

all: $(BINARY) 

$(BINARY): $(OBJECTS)
	$(F90) -o $(BINARY) $(OBJECTS) $(FFLAGS) $(FCFLAGS) 

%.o: %.f90 Makefile
	$(F90) -c $(FFLAGS) $(FCFLAGS) $<

%.f90: %.F90 Makefile
	cpp ${DM} $< > $@

clean:
	rm *.o $(BINARY) *cub *ptx
