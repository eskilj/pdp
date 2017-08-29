SRC = mpi-generic.F90  jacobi.F90 gauss-seidel.F90 diffusion.F90
LFLAGS=

FTN=mpif90

all: 
	$(FTN) -o mpi-generic -g $(SRC) $(LFLAGS)

clean:
	rm mpi-generic *.mod
