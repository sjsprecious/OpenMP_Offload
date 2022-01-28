ifeq ($(_COMPILER),CRAY)
F90=ftn
DM=
FFLAGS= -O3
ifeq ($(_OPENMP),true)
ifeq ($(_OPENACC),true)
$(error Do not set _OPENMP and _OPENACC to be both true for this example...)
else
FCFLAGS= -target-accel=nvidia70 -h cpu=x86-skylake,omp,fp3,aggress,vector3,msgs 
OBJECTS= matrix_mult.o
endif
else ifeq ($(_OPENACC),true)
FCFLAGS= -target-accel=nvidia70 -h cpu=x86-skylake,acc,fp3,aggress,vector3,msgs
OBJECTS= matrix_mult.o
else
FCFLAGS= -h cpu=x86-skylake,omp,fp3,aggress,vector3,msgs
OBJECTS= matrix_mult.o
endif

else ifeq ($(_COMPILER),NVHPC)
F90=nvfortran
DM=
FFLAGS= -O3 -Wall -Mfree
ifeq ($(_OPENMP),true)
ifeq ($(_OPENACC),true)
$(error Do not set _OPENMP and _OPENACC to be both true for this example...)
else
FCFLAGS= -tp=skylake -target=gpu -gpu=cc70 -mp=gpu -Minfo=all -I${NVHPC_ROOT_PATH}/include
OBJECTS= matrix_mult.o
endif
else ifeq ($(_OPENACC),true)
FCFLAGS= -acc -gpu=cc70 -Minfo=all -I${NVHPC_ROOT_PATH}/include
OBJECTS= matrix_mult.o
else
FCFLAGS=
OBJECTS= matrix_mult.o
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

# The binary to be built by this Makefile
BINARY= matrix_mult.exe

all: $(BINARY)

$(BINARY): $(OBJECTS)
	$(F90) -o $(BINARY) $(OBJECTS) $(FFLAGS) $(FCFLAGS) 

%.o: %.f90 Makefile
	$(F90) -c $(FFLAGS) $(FCFLAGS) $<

%.f90: %.F90 Makefile
	cpp ${DM} $< > $@

clean:
	rm *.o $(BINARY) *ptx *cub
