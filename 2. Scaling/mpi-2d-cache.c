#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "mpi.h"

// Boundary value at the LHS of the bar
#define LEFT_VALUE 1.0
// Boundary value at the RHS of the bar
#define RIGHT_VALUE 10.0
// The maximum number of iterations
#define MAX_ITERATIONS 100000
// How often to report the norm
#define REPORT_NORM_PERIOD 1000

void initialise(double*, double*, int, int, int*, int*);

int main(int argc, char * argv[]) {
	int size, myrank, dims[]={0,0}, period[2], coords[2], reorder, left_neighbour, right_neighbour, up_neighbour, down_neighbour, nx, ny;
	MPI_Comm cart_communicator;
	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	MPI_Dims_create(size, 2, dims);
	period[0]=period[1]=0;
	reorder=1;
	MPI_Cart_create(MPI_COMM_WORLD, 2, dims, period, 1, &cart_communicator);
	MPI_Cart_shift(cart_communicator, 0, 1, &left_neighbour, &right_neighbour);
	MPI_Cart_shift(cart_communicator, 1, 1, &down_neighbour, &up_neighbour);
	MPI_Comm_rank(cart_communicator, &myrank);
	MPI_Cart_coords(cart_communicator, myrank, 2, coords);

	if (argc != 4) {
		fprintf(stderr, "You must provide three command line arguments, the global size in X, the global size in Y and convergence accuracy\n");
		return -1;
	}
	nx=atoi(argv[1]);
	ny=atoi(argv[2]);
	double convergence_accuracy=atof(argv[3]);

	if (myrank==0) {
		printf("Number Processes in X=%d, Number Processes in Y=%d\n", dims[0], dims[1]);
		printf("Global size in X=%d, Global size in Y=%d\n\n", nx, ny);
	}

	int local_nx=nx/dims[0], local_ny=ny/dims[1];
	if (local_nx * dims[0] < nx) {
		if (coords[0] < nx - local_nx * dims[0]) local_nx++;
	}
	if (local_ny * dims[1] < ny) {
		if (coords[1] < ny - local_ny * dims[1]) local_ny++;
	}

	int mem_size_x=local_nx+2, mem_size_y=local_ny+2;

	double * u_k = malloc(sizeof(double) * mem_size_x * mem_size_y);
	double * u_kp1 = malloc(sizeof(double) * mem_size_x * mem_size_y);
	double * temp;
	double * send_buffer_left=malloc(sizeof(double) * local_ny);
	double * send_buffer_right=malloc(sizeof(double) * local_ny);
	double * recv_buffer_left=malloc(sizeof(double) * local_ny);
	double * recv_buffer_right=malloc(sizeof(double) * local_ny);
	double start_time;

	initialise(u_k, u_kp1, local_nx, local_ny, coords, dims);

	double rnorm=0.0, bnorm=0.0, norm, tmpnorm=0.0;
	MPI_Request requests[2];

	int i,j,k;
	for (j=1;j<=local_ny;j++) {
		for (i=1;i<=local_nx;i++) {
			tmpnorm=tmpnorm+pow(u_k[i+(j*mem_size_x)]*4-u_k[(i-1)+(j*mem_size_x)]-
					u_k[(i+1)+(j*mem_size_x)]-u_k[i+((j-1)*mem_size_x)]-u_k[i+((j+1)*mem_size_x)], 2);
		}
	}
	MPI_Allreduce(&tmpnorm, &bnorm, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
	bnorm=sqrt(bnorm);

	start_time=MPI_Wtime();
	for (k=0;k<MAX_ITERATIONS;k++) {
		if (coords[1] > 0) {
			MPI_Isend(&u_k[1+mem_size_x], local_nx, MPI_DOUBLE, down_neighbour, 0, MPI_COMM_WORLD, &requests[0]);
			MPI_Irecv(&u_k[1], local_nx, MPI_DOUBLE, down_neighbour, 0, MPI_COMM_WORLD, &requests[1]);
			MPI_Waitall(2, requests, MPI_STATUSES_IGNORE);
		}
		if (coords[1] < dims[1]-1) {
			MPI_Isend(&u_k[(local_ny*mem_size_x)+1], local_nx, MPI_DOUBLE, up_neighbour, 0, MPI_COMM_WORLD, &requests[0]);
			MPI_Irecv(&u_k[((local_ny+1)*mem_size_x)+1], local_nx, MPI_DOUBLE, up_neighbour, 0, MPI_COMM_WORLD, &requests[1]);
			MPI_Waitall(2, requests, MPI_STATUSES_IGNORE);
		}
		
		if (coords[0] > 0) {
			for (i=1;i<=local_ny;i++) {
				send_buffer_left[i-1]=u_k[1+(i*mem_size_x)];
			}
			MPI_Isend(send_buffer_left, local_ny, MPI_DOUBLE, left_neighbour, 0, MPI_COMM_WORLD, &requests[0]);
			MPI_Irecv(recv_buffer_left, local_ny, MPI_DOUBLE, left_neighbour, 0, MPI_COMM_WORLD, &requests[1]);
			MPI_Waitall(2, requests, MPI_STATUSES_IGNORE);
			for (i=1;i<=local_ny;i++) {
				u_k[i*mem_size_x]=recv_buffer_left[i-1];
			}
		}

		if (coords[0] < dims[0]-1) {
			for (i=1;i<=local_ny;i++) {
				send_buffer_right[i-1]=u_k[local_nx+(i*mem_size_x)];
			}
			MPI_Isend(send_buffer_right, local_ny, MPI_DOUBLE, right_neighbour, 0, MPI_COMM_WORLD, &requests[0]);
			MPI_Irecv(recv_buffer_right, local_ny, MPI_DOUBLE, right_neighbour, 0, MPI_COMM_WORLD, &requests[1]);
			MPI_Waitall(2, requests, MPI_STATUSES_IGNORE);
			for (i=1;i<=local_ny;i++) {
				u_k[local_nx+1+(i*mem_size_x)]=recv_buffer_right[i-1];
			}
		}

		tmpnorm=0.0;
		for (j=1;j<=local_ny;j++) {
			for (i=1;i<=local_nx;i++) {
				tmpnorm=tmpnorm+pow(u_k[i+(j*mem_size_x)]*4-u_k[(i-1)+(j*mem_size_x)]-
						u_k[(i+1)+(j*mem_size_x)]-u_k[i+((j-1)*mem_size_x)]-u_k[i+((j+1)*mem_size_x)], 2);
			}
		}
		MPI_Allreduce(&tmpnorm, &rnorm, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
		norm=sqrt(rnorm)/bnorm;
		if (norm < convergence_accuracy) break;

		for (j=1;j<=local_ny;j++) {
			for (i=1;i<=local_nx;i++) {
				u_kp1[i+(j*mem_size_x)]=0.25 * (u_k[(i-1)+(j*mem_size_x)]+u_k[(i+1)+(j*mem_size_x)]+
						u_k[i+((j-1)*mem_size_x)]+u_k[i+((j+1)*mem_size_x)]);
			}
		}
		temp=u_kp1; u_kp1=u_k; u_k=temp;

		if (k % REPORT_NORM_PERIOD == 0 && myrank==0) printf("Iteration= %d Relative Norm=%e\n", k, norm);
	}
	if (myrank==0) printf("\nTerminated on %d iterations, Relative Norm=%e, Total time=%e seconds\n", k, norm,
			MPI_Wtime() - start_time);
	free(u_k);
	free(u_kp1);
	MPI_Finalize();
	return 0;
}

/**
 * Initialises the arrays, such that u_k contains the boundary conditions at the start and end points and all other
 * points are zero. u_kp1 is set to equal u_k
 */
void initialise(double * u_k, double * u_kp1, int local_nx, int local_ny, int * coords, int * dims) {
	int i,j;
	for (i=0;i<local_ny+1;i++) {
		u_k[i*(local_nx+2)]=coords[0]==0 ? LEFT_VALUE : 0;
		u_k[(local_nx+1)+(i*(local_nx+2))]=coords[0]==dims[0]-1 ? RIGHT_VALUE : 0;
	}
	for (j=0;j<=local_ny+1;j++) {
		for (i=1;i<=local_nx;i++) {
			u_k[i+(j*(local_nx+2))]=0.0;
		}
	}
	for (j=0;j<=local_ny+1;j++) {
		for (i=0;i<=local_nx+1;i++) {
			u_kp1[i+(j*(local_nx+2))]=u_k[i+(j*(local_nx+2))];
		}
	}
}
