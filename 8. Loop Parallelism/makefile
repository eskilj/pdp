SRC = openmp-loop.F90
FFLAGS=-fopenmp
LFLAGS=

FTN=gfortran

all: 
	$(FTN) -o openmp-loop $(FFLAGS) $(SRC) $(LFLAGS)

clean:
	rm openmp-loop *.mod
