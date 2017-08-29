module mpi_generic_mod
  use mpi
  implicit none

  interface
     function init(myrank, comm_size, local_nx)      
       integer, intent(in) :: myrank, comm_size, local_nx
       real(kind=8), dimension(:), pointer :: init
     end function init
     subroutine solver(local_nx)       
       integer, intent(in) :: local_nx       
     end subroutine solver
     subroutine finalise()
     end subroutine finalise
  end interface

  ! Number of points to solve in the pipe
  integer, parameter :: NX = 100
  ! Boundary value at the LHS of the pipe
  real(kind=8), parameter :: LEFT_VALUE = 1.0
  ! Boundary value at the RHS of the pipe
  real(kind=8), parameter :: RIGHT_VALUE = 10.0
  ! The maximum number of iterations
  integer, parameter :: MAX_ITERATIONS = 100000
  ! The accuracy of solution to solve to
  real(kind=8), parameter :: CONVERGENCE_ACCURACY = 1e-6
  ! How often to report the norm
  integer, parameter :: REPORT_NORM_PERIOD = 1000

contains

  subroutine perform_halo_swap(myrank, comm_size, local_nx, data)
    real(kind=8), dimension(:), pointer, intent(inout) :: data
    integer, intent(in) :: myrank, comm_size, local_nx

    integer :: ierr, requests(4)

    requests=MPI_REQUEST_NULL

    if (myrank .gt. 0) then
      call mpi_isend(data(1), 1, MPI_DOUBLE, myrank-1, 0, MPI_COMM_WORLD, requests(1), ierr)
      call mpi_irecv(data(0), 1, MPI_DOUBLE, myrank-1, 0, MPI_COMM_WORLD, requests(2), ierr)
    end if
    if (myrank .lt. comm_size-1) then
      call mpi_isend(data(local_nx), 1, MPI_DOUBLE, myrank+1, 0, MPI_COMM_WORLD, requests(3), ierr)
      call mpi_irecv(data(local_nx+1), 1, MPI_DOUBLE, myrank+1, 0, MPI_COMM_WORLD, requests(4), ierr)
    end if
    call mpi_waitall(4, requests, MPI_STATUSES_IGNORE, ierr)
  end subroutine perform_halo_swap

  subroutine run_solver(myrank, comm_size, local_nx, init_proc, solver_proc, finalise_proc)
    integer, intent(in) :: myrank, comm_size, local_nx
    procedure(init) :: init_proc
    procedure(solver) :: solver_proc
    procedure(finalise) :: finalise_proc

    real(kind=8), dimension(:), pointer :: u_k
    real(kind=8) :: bnorm, rnorm, norm
    integer :: k

    u_k=>init_proc(myrank, comm_size, local_nx)

    bnorm=compute_global_residual(u_k, local_nx)
    do k=0, MAX_ITERATIONS
      call perform_halo_swap(myrank, comm_size, local_nx, u_k)
      rnorm=compute_global_residual(u_k, local_nx)
      call solver_proc(local_nx)
      norm=rnorm/bnorm
      if (norm .gt. 1.0) exit
      if (norm .lt. CONVERGENCE_ACCURACY) exit
      if (mod(k, REPORT_NORM_PERIOD)==0 .and. myrank==0) print *, "Iteration=",k," Relative Norm=",norm
    end do
    if (myrank==0) then
      print *, "Terminated on ",k," iterations, Relative Norm=", norm
    end if
    call finalise_proc()
  end subroutine run_solver

  !  Initialises the arrays, such that u_k contains the boundary conditions at the start and end points and all other
  ! points are zero. u_kp1 is set to equal u_k
  subroutine fill_boundary_conditions(u_k, local_nx, my_rank, comm_size)
    real(kind=8), intent(inout) :: u_k(0:local_nx+1)
    integer, intent(in) :: local_nx, my_rank, comm_size

    u_k(0)=merge(LEFT_VALUE, 0.0_8, my_rank==0)
    u_k(local_nx+1)=merge(RIGHT_VALUE, 0.0_8, my_rank==comm_size-1)
    u_k(1:local_nx)=0.0
  end subroutine fill_boundary_conditions

  real(kind=8) function compute_global_residual(data, local_nx)
    real(kind=8), intent(in) :: data(0:local_nx+1)
    integer, intent(in) :: local_nx

    real(kind=8) :: tmpnorm, global_norm
    integer :: i, ierr

    tmpnorm=0.0
    do i=1, local_nx
      tmpnorm=tmpnorm+((data(i)*2-data(i-1)-data(i+1)) ** 2)
    end do
    call mpi_allreduce(tmpnorm, global_norm, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD, ierr)
    compute_global_residual=sqrt(global_norm)
  end function compute_global_residual
end module mpi_generic_mod

