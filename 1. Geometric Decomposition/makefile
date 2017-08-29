SRC = mpi-halos.F90
ASYNC_SRC = mpi-async.F90
LFLAGS=

FTN=mpif90

all: 
	$(FTN) -o mpi-halos $(SRC) $(LFLAGS)
	$(FTN) -o mpi-async $(ASYNC_SRC) $(LFLAGS)

clean:
	rm -f mpi-async mpi-halos *.mod
