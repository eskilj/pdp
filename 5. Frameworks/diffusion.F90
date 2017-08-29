program diffusion
  use mpi_generic_mod
  use jacobi_mod
  use gauss_seidel_mod
  implicit none

  ! Solver type, 0=jacobi, 1=gauss seidel, 2=jacobi SOR, 3=gauss seidel SOR
  integer, parameter :: solver_type = 0

  integer :: ierr, local_nx, comm_size, myrank

  call mpi_init(ierr)
  call mpi_comm_size(MPI_COMM_WORLD, comm_size, ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierr)

  local_nx=NX/comm_size
  if (local_nx * comm_size .lt. NX) then
    if (myrank .lt. NX - local_nx * comm_size) local_nx=local_nx+1
  end if

  if (solver_type == 0) then
    call run_solver(myrank, comm_size, local_nx, init_jacobi, jacobi_solver, finalise_jacobi)
  else if (solver_type == 1) then
    call run_solver(myrank, comm_size, local_nx, init_gauss_seidel, gauss_seidel_solver, finalise_gauss_seidel)
  else if (solver_type == 2) then
    call run_solver(myrank, comm_size, local_nx, init_jacobi, jacobi_sor_solver, finalise_jacobi)
  else if (solver_type == 3) then
    call run_solver(myrank, comm_size, local_nx, init_gauss_seidel, gauss_seidel_sor_solver, finalise_gauss_seidel)
  end if
  call mpi_finalize(ierr)
end program diffusion

