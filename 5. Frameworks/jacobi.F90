module jacobi_mod
  use mpi_generic_mod
  implicit none

  ! W is the amount to overrelax in the SOR, it should be larger than 1 and smaller than 2 (but might diverge before this)
  real, parameter :: W = 1.2

  real(kind=8), dimension(:), allocatable, target :: u_k, u_kp1, temp  
contains

  function init_jacobi(myrank, comm_size, local_nx)
    integer, intent(in) :: myrank, comm_size, local_nx
    real(kind=8), dimension(:), pointer :: init_jacobi

    allocate(u_k(0:local_nx+1), u_kp1(0:local_nx+1), temp(0:local_nx+1))

    call fill_boundary_conditions(u_k, local_nx, myrank, comm_size)
    u_kp1=u_k
    init_jacobi=>u_k
  end function init_jacobi

  subroutine jacobi_solver(local_nx)
    integer, intent(in) :: local_nx

    integer :: i
    
    do i=1, local_nx
      u_kp1(i)=0.5 * (u_k(i-1) + u_k(i+1))
    end do
    temp=u_kp1
    u_kp1=u_k
    u_k=temp
  end subroutine jacobi_solver

  subroutine jacobi_sor_solver(local_nx)
    integer, intent(in) :: local_nx

    integer :: i
    
    do i=1, local_nx
      u_kp1(i)=((1-W) * u_kp1(i)) + 0.5 * W * (u_k(i-1)+u_k(i+1));
    end do
    temp=u_kp1
    u_kp1=u_k
    u_k=temp
  end subroutine jacobi_sor_solver

  subroutine finalise_jacobi()
    deallocate(u_k, u_kp1, temp)
  end subroutine finalise_jacobi
end module jacobi_mod
