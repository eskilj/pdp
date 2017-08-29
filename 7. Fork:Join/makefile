LFLAGS=-lm -fopenmp
CC=gfortran

all: 
	$(CC) -o mergesort ran2.F90 qsort.F90 mergesort.F90 $(LFLAGS)
	$(CC) -o mergesort-tasks ran2.F90 qsort.F90 mergesort-tasks.F90 $(LFLAGS)

clean:
	rm -f mergesort mergesort-tasks *.mod
