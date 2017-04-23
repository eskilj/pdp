

-- Squirrel Simulation --


To compile the exmple simulation:

	$ make

The program can then be executed with:

	$ mpirun -n 220 ./build/main


To submit job to PBS queue:

	$ qsub run.pbs

	or

	$ make run


Simulation output will be written to the /output directory. 