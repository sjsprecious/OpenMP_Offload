# Compilers and linkers
F90=nvfortran

# Compilation flags
FFLAGS= -O3 -Wall -Mfree

ifeq ($(_OPENMP),true)
ifeq ($(_OPENACC),true)
#$(error _OPENMP and _OPENACC can not be both true.)
FCFLAGS= -tp=skylake -acc -target=gpu -gpu=cc70 -mp=gpu -Minfo=all -I${NVHPC_ROOT_PATH}/include
DM= -D_OPENMP -D_OPENACC
else
FCFLAGS= -tp=skylake -target=gpu -gpu=cc70 -mp=gpu -Minfo=all -I${NVHPC_ROOT_PATH}/include
DM= -D_OPENMP 
endif
else ifeq ($(_OPENACC),true)
FCFLAGS= -acc -gpu=cc70 -Minfo=all -I${NVHPC_ROOT_PATH}/include
DM= -D_OPENACC
else
FCFLAGS=
DM=
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
	rm *.o $(BINARY)
