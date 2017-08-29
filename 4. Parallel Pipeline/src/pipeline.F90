module problem_mod
  use mpi
  implicit none

  ! Number of points to solve in the pipe
  integer, parameter :: NX = 100
  ! The maximum number of iterations
  integer, parameter :: MAX_ITERATIONS = 100000
  real(kind=8), parameter :: THRESHOLD_VALUE = 18.75
  ! The accuracy of solution to solve to
  real(kind=8), parameter :: CONVERGENCE_ACCURACY = 1e-4
  ! How often to report the norm
  integer, parameter :: REPORT_NORM_PERIOD = 1000
  ! Whether to display the solver progress
  logical, parameter :: DISPLAY_SOLVER_PROGRESS = .false.

contains

  subroutine run_pipeline()
    integer :: myrank, ierr
    
    call mpi_init(ierr)
    call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierr)
    
    if (myrank == 0) then
      call read_data_points()
    else if (myrank == 1) then
      call average_sample_values()
    else if (myrank == 2) then
      call run_solver()
    else if (myrank == 3) then
      call data_analysis()
    else if (myrank == 4) then
      call write_values()
    end if    
    call mpi_finalize(ierr)
  end subroutine run_pipeline 

  subroutine read_data_points()
    integer :: i, ierr
    character(len=65) :: file_name
    real(kind=8) :: values(60)
    
    do i=1, command_argument_count()
      call get_command_argument(i, file_name)
      open (unit=2, file=file_name, action='read', status='old', iostat=ierr)
      if (ierr == 0) then
        read (2,*) values
        close(2)
        call mpi_send(values, 60, MPI_DOUBLE, 1, 0, MPI_COMM_WORLD, ierr)
      else
        print *, "Warning: Can not find file ", file_name
      end if
    end do
    call mpi_send(0, 0, MPI_DOUBLE, 1, 0, MPI_COMM_WORLD, ierr)
  end subroutine read_data_points

  subroutine average_sample_values()
    integer :: ierr, num_elements, status(MPI_STATUS_SIZE), i
    real(kind=8) :: data_values(60), averaged_values(2)

    do while (1==1)
      call mpi_recv(data_values, 60, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD, status, ierr)
      call mpi_get_count(status, MPI_DOUBLE, num_elements, ierr)
      if (num_elements == 0) exit

      averaged_values(1)=sum(data_values(:30))/30
      averaged_values(2)=sum(data_values(31:))/30
      call mpi_send(averaged_values, 2, MPI_DOUBLE, 2, 0, MPI_COMM_WORLD, ierr)
     end do
     call mpi_send(0, 0, MPI_DOUBLE, 2, 0, MPI_COMM_WORLD, ierr)
  end subroutine average_sample_values  

  subroutine data_analysis()
    real(kind=8), dimension(:), allocatable :: u_k
    integer :: ierr, num_elements, status(MPI_STATUS_SIZE), i, data_values(2)
    logical :: past_threshold

    allocate(u_k(0:NX+1))

    do while (1==1)
      call mpi_recv(u_k, NX+2, MPI_DOUBLE, 2, 0, MPI_COMM_WORLD, status, ierr)
      call mpi_get_count(status, MPI_DOUBLE, num_elements, ierr)
      if (num_elements == 0) exit

      past_threshold=.false.
      data_values=0
      do i=0, NX+1
        if (u_k(i) .ge. THRESHOLD_VALUE) then
          if (.not. past_threshold) then
            past_threshold=.true.
            data_values(1)=i
          end if
          data_values(2)=data_values(2)+1
        end if
      end do
      call mpi_send(data_values, 2, MPI_INT, 4, 0, MPI_COMM_WORLD, ierr)
    end do
    call mpi_send(0, 0, MPI_INT, 4, 0, MPI_COMM_WORLD, ierr)
    deallocate(u_k)
  end subroutine data_analysis

  subroutine write_values()
    integer :: ierr, num_elements, status(MPI_STATUS_SIZE), i, data_values(2), file_number
    character(len=32) :: str_conv

    file_number=0
    do while (1==1)
      call mpi_recv(data_values, 2, MPI_INT, 3, 0, MPI_COMM_WORLD, status, ierr)
      call mpi_get_count(status, MPI_INT, num_elements, ierr)
      if (num_elements == 0) exit

      if (file_number .lt. 10) then
        write(str_conv,'(I1)') file_number
      else
        write(str_conv,'(I2)') file_number
      end if      
      open (unit=3, file="results_"//str_conv)
      write (3,*) "Sample number ", file_number
      write (3,*) "Pollution starts at grid point ", data_values(1)
      write (3,*) "Number of grid points over threshold that need cleaning ", data_values(2)
      close(3)
      print *, "Result file is: results_"//str_conv
      file_number=file_number+1
    end do    
  end subroutine write_values  

  subroutine run_solver()
    real(kind=8), dimension(:), allocatable :: u_k, u_kp1, temp
    real(kind=8) :: bnorm, rnorm, norm, boundary_values(2)
    integer :: i, k, ierr, status(MPI_STATUS_SIZE), num_elements

    allocate(u_k(0:NX+1), u_kp1(0:NX+1), temp(0:NX+1))

    do while (1==1)
      call mpi_recv(boundary_values, 2, MPI_DOUBLE, 1, 0, MPI_COMM_WORLD, status, ierr)
      call mpi_get_count(status, MPI_DOUBLE, num_elements, ierr)
      if (num_elements == 0) exit
      bnorm=0.0
      rnorm=0.0

      call initialise_values(u_k, u_kp1, boundary_values(1), boundary_values(2))

      do i=1, NX
        bnorm=bnorm+((u_k(i)*2-u_k(i-1)-u_k(i+1)) ** 2)
      end do
      bnorm=sqrt(bnorm)

      do k=0, MAX_ITERATIONS
        do i=1, NX
          rnorm=rnorm+((u_k(i)*2-u_k(i-1)-u_k(i+1)) ** 2)
        end do
        norm=sqrt(rnorm)/bnorm
        if (norm .lt. CONVERGENCE_ACCURACY) exit
        do i=1, NX
          u_kp1(i)=0.5 * (u_k(i-1) + u_k(i+1))
        end do
        temp=u_kp1
        u_kp1=u_k
        u_k=temp      
        if (DISPLAY_SOLVER_PROGRESS .and. mod(k, REPORT_NORM_PERIOD)==0) print *, "Iteration=",k," Relative Norm=",norm
        rnorm=0.0
      end do
      call mpi_send(u_k, NX+2, MPI_DOUBLE, 3, 0, MPI_COMM_WORLD, ierr)
    end do
    call mpi_send(0, 0, MPI_DOUBLE, 3, 0, MPI_COMM_WORLD, ierr)
    deallocate(u_k, u_kp1, temp)
  end subroutine run_solver

  !  Initialises the arrays, such that u_k contains the boundary conditions at the start and end points and all other
  ! points are zero. u_kp1 is set to equal u_k
  subroutine initialise_values(u_k, u_kp1, left_boundary, right_boundary)
    real(kind=8), intent(inout) :: u_k(0:NX+1), u_kp1(0:NX+1)
    real(kind=8), intent(in) :: left_boundary, right_boundary
    integer :: i

    u_k(0)=left_boundary
    u_k(NX+1)=right_boundary
    u_k(1:NX)=0.0
    u_kp1=u_k
  end subroutine initialise_values  
end module problem_mod

program diffusion
  use problem_mod
  implicit none

  call run_pipeline()
end program diffusion
