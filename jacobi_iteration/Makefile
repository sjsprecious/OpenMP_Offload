# Compilers and linkers
F90=ftn

# Compilation flags
FFLAGS= -O3

ifeq ($(_OPENMP),true)
ifeq ($(_OPENACC),true)
#$(error _OPENMP and _OPENACC can not be both true.)
FCFLAGS= -target-accel=nvidia70 -h cpu=x86-skylake,acc,omp,fp3,aggress,vector3,msgs
DM= -D_OPENMP -D_OPENACC
else
FCFLAGS= -target-accel=nvidia70 -h cpu=x86-skylake,omp,fp3,aggress,vector3,msgs
DM= -D_OPENMP 
endif
else ifeq ($(_OPENACC),true)
FCFLAGS= -target-accel=nvidia70 -h cpu=x86-skylake,acc,omp,fp3,aggress,vector3,msgs
DM= -D_OPENACC
else
FCFLAGS= -h cpu=x86-skylake,omp,fp3,aggress,vector3,msgs
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
