# Compilers and linkers
F90=nvfortran

# Compilation flags
FFLAGS= -O3 -Wall -Mfree

ifeq ($(_OPENACC),true)
FCFLAGS= -acc -gpu=cc70 -Minfo=all -I${NVHPC_ROOT_PATH}/include
# Description of necessary files and binaries
# List of object files separated by spaces (e.g. main.o device.o)
OBJECTS= matrix_mult.o
else ifeq ($(_OPENMP),true)
FCFLAGS= -tp=skylake -target=gpu -gpu=cc70 -mp=gpu -Minfo=all -I${NVHPC_ROOT_PATH}/include
OBJECTS= matrix_mult_omp.o
else
FCFLAGS=
OBJECTS= matrix_mult.o
endif

# The binary to be built by this Makefile
BINARY= matrix_mult.exe

all: $(BINARY) 

$(BINARY): $(OBJECTS)
	$(F90) -o $(BINARY) $(OBJECTS) $(FCFLAGS) 

%.o: %.f90 Makefile
	$(F90) -c $(FFLAGS) $(FCFLAGS) $<

clean:
	rm *.o $(BINARY)

