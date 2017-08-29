module problem_mod
  use mpi
  implicit none

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

  subroutine run_solver()
    real(kind=8), dimension(:), allocatable :: u_k, u_kp1, temp
    real(kind=8) :: bnorm, rnorm, norm, tmpnorm
    integer :: i, k, ierr, size, myrank, local_nx

    call mpi_init(ierr)
    call mpi_comm_size(MPI_COMM_WORLD, size, ierr)
    call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierr)

    local_nx=NX/size
    if (local_nx * size .lt. NX) then
      if (myrank .lt. NX - local_nx * size) local_nx=local_nx+1
    end if

    allocate(u_k(0:local_nx+1), u_kp1(0:local_nx+1), temp(0:local_nx+1))

    bnorm=0.0
    tmpnorm=0.0
    rnorm=0.0
    
    call initialise_values(u_k, u_kp1, local_nx, myrank, size)

    do i=1, local_nx
      tmpnorm=tmpnorm+((u_k(i)*2-u_k(i-1)-u_k(i+1)) ** 2)
    end do
    call mpi_allreduce(tmpnorm, bnorm, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD, ierr)
    bnorm=sqrt(bnorm)

    do k=0, MAX_ITERATIONS
      if (myrank .gt. 0) then
        call mpi_sendrecv(u_k(1), 1, MPI_DOUBLE, myrank-1, 0, u_k(0), 1, &
          MPI_DOUBLE, myrank-1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
      end if
      if (myrank .lt. size-1) then
        call mpi_sendrecv(u_k(local_nx), 1, MPI_DOUBLE, myrank+1, 0, u_k(local_nx+1), 1, &
          MPI_DOUBLE, myrank+1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
      end if
      tmpnorm=0.0
      do i=1, local_nx
        tmpnorm=tmpnorm+((u_k(i)*2-u_k(i-1)-u_k(i+1)) ** 2)
      end do
      call mpi_allreduce(tmpnorm, rnorm, 1, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD, ierr)
      norm=sqrt(rnorm)/bnorm
      if (norm .lt. CONVERGENCE_ACCURACY) exit

      do i=1, local_nx
        u_kp1(i)=0.5 * (u_k(i-1) + u_k(i+1))
      end do
      temp=u_kp1
      u_kp1=u_k
      u_k=temp

      if (mod(k, REPORT_NORM_PERIOD)==0 .and. myrank==0) print *, "Iteration=",k," Relative Norm=",norm
    end do
    if (myrank==0) print *, "Terminated on ",k," iterations, Relative Norm=", norm
    deallocate(u_k, u_kp1, temp)
    call mpi_finalize(ierr)
  end subroutine run_solver

  !  Initialises the arrays, such that u_k contains the boundary conditions at the start and end points and all other
  ! points are zero. u_kp1 is set to equal u_k
  subroutine initialise_values(u_k, u_kp1, local_nx, my_rank, size)
    real(kind=8), intent(inout) :: u_k(0:local_nx+1), u_kp1(0:local_nx+1)
    integer, intent(in) :: local_nx, my_rank, size
    integer :: i

    u_k(0)=merge(LEFT_VALUE, 0.0_8, my_rank==0)
    u_k(local_nx+1)=merge(RIGHT_VALUE, 0.0_8, my_rank==size-1)
    u_k(1:local_nx)=0.0
    u_kp1=u_k
  end subroutine initialise_values  
end module problem_mod

program diffusion
  use problem_mod
  implicit none

  call run_solver()
end program diffusion
