LFLAGS=-lm
CC=mpicc
CFLAGS=-O3

all: 
	$(CC) $(CFLAGS) -o mpi-2d-simple mpi-2d-simple.c $(LFLAGS)
	$(CC) $(CFLAGS) -o mpi-2d-cache mpi-2d-cache.c $(LFLAGS)
	$(CC) $(CFLAGS) -o mpi-2d-comm mpi-2d-comm.c $(LFLAGS)
	$(CC) $(CFLAGS) -o mpi-2d-overlap mpi-2d-overlap.c $(LFLAGS)

clean:
	rm -f mpi-2d-simple mpi-2d-cache mpi-2d-comm mpi-2d-overlap
