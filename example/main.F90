program main
  use simulation
  use director_mod
  implicit none

  ! call run()
  call entry()
  contains

  subroutine entry()
    type(director) :: sim_director

    ! Initialize MPI via director
    sim_director = director()


    ! Create simulation type factory



    ! Initialize processes





    ! Run simulation




    ! Close
    call sim_director%director_finalize()

  end subroutine entry

end program main
