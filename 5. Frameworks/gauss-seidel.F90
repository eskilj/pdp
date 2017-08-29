module gauss_seidel_mod
  use mpi_generic_mod
  implicit none

  ! W is the amount to overrelax in the SOR, it should be larger than 1 and smaller than 2 (but might diverge before this)
  real, parameter :: W = 1.2

  real(kind=8), dimension(:), allocatable, target :: u_k
contains

  function init_gauss_seidel(myrank, comm_size, local_nx)
    integer, intent(in) :: myrank, comm_size, local_nx
    real(kind=8), dimension(:), pointer :: init_gauss_seidel

    allocate(u_k(0:local_nx+1))

    call fill_boundary_conditions(u_k, local_nx, myrank, comm_size)
    init_gauss_seidel=>u_k
  end function init_gauss_seidel

  subroutine gauss_seidel_solver(local_nx)
    integer, intent(in) :: local_nx

    integer :: i
    
    do i=1, local_nx
      u_k(i)=0.5 * (u_k(i-1) + u_k(i+1))
    end do
  end subroutine gauss_seidel_solver

  subroutine gauss_seidel_sor_solver(local_nx)
    integer, intent(in) :: local_nx

    integer :: i
    
    do i=1, local_nx
      u_k(i)=((1-W) * u_k(i)) + 0.5 * W * (u_k(i-1)+u_k(i+1));
    end do
  end subroutine gauss_seidel_sor_solver

  subroutine finalise_gauss_seidel()
    deallocate(u_k)
  end subroutine finalise_gauss_seidel
end module gauss_seidel_mod
