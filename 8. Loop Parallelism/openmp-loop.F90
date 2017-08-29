module problem_mod
  use omp_lib
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
  ! Whether to display the computed pipe points
  logical, parameter :: DISPLAY_COMPUTED_VALUE = .false.

contains

  subroutine run_solver()
    real(kind=8), dimension(:), allocatable :: u_k, u_kp1, temp
    real(kind=8) :: bnorm, rnorm, norm
    integer :: i, k

    allocate(u_k(0:NX+1), u_kp1(0:NX+1), temp(0:NX+1))

    bnorm=0.0
    rnorm=0.0
    
    call initialise_values(u_k, u_kp1)

    !$OMP PARALLEL PRIVATE(i,k)
    !$OMP MASTER
    print *, "Number of threads: ", OMP_GET_NUM_THREADS()
    !$OMP END MASTER
    !$OMP DO SCHEDULE(STATIC) REDUCTION(+:bnorm)
    do i=1, NX
      bnorm=bnorm+((u_k(i)*2-u_k(i-1)-u_k(i+1)) ** 2)
    end do
    !$OMP END DO
    !$OMP SINGLE
    bnorm=sqrt(bnorm)
    !$OMP END SINGLE
    do k=0, MAX_ITERATIONS
      !$OMP DO SCHEDULE(STATIC) REDUCTION(+:rnorm)
      do i=1, NX
        rnorm=rnorm+((u_k(i)*2-u_k(i-1)-u_k(i+1)) ** 2)
      end do
      !$OMP END DO
      !$OMP DO SCHEDULE(STATIC) 
      do i=1, NX
        u_kp1(i)=0.5 * (u_k(i-1) + u_k(i+1))
      end do
      !$OMP END DO
      !$OMP SINGLE
      temp=u_kp1
      u_kp1=u_k
      u_k=temp
      norm=sqrt(rnorm)/bnorm
      rnorm=0.0
      !$OMP END SINGLE      
      if (norm .lt. CONVERGENCE_ACCURACY) exit
      !$OMP MASTER
      if (mod(k, REPORT_NORM_PERIOD)==0) print *, "Iteration=",k," Relative Norm=",norm
      !$OMP END MASTER
    end do
    !$OMP MASTER
    print *, "Terminated on ",k," iterations, Relative Norm=", norm
    !$OMP END MASTER 
    !$OMP END PARALLEL

    if (DISPLAY_COMPUTED_VALUE) call display(u_k)
    deallocate(u_k, u_kp1, temp)
  end subroutine run_solver

  ! Displays each point in the pipe
  subroutine display(u_k)
    real(kind=8), intent(in) :: u_k(0:NX+1)
    integer :: i

    print *, "-------- Display of values --------"
    do i=0,NX+1
      print *, i, u_k(i)
    end do
  end subroutine display  

  !  Initialises the arrays, such that u_k contains the boundary conditions at the start and end points and all other
  ! points are zero. u_kp1 is set to equal u_k
  subroutine initialise_values(u_k, u_kp1)
    real(kind=8), intent(inout) :: u_k(0:NX+1), u_kp1(0:NX+1)
    integer :: i

    u_k(0)=LEFT_VALUE
    u_k(NX+1)=RIGHT_VALUE
    u_k(1:NX)=0.0
    u_kp1=u_k
  end subroutine initialise_values  
end module problem_mod

program diffusion
  use problem_mod
  implicit none

  call run_solver()
end program diffusion
