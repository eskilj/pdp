! Main program - Launches simulation

program main
  use actor_mod
  use simulation
  implicit none

  call entry()

  contains

  subroutine entry()
    class(actor), pointer :: sim_director
    integer :: actor_type

    ! Initialize MPI via director
    call initialize(actor_type)

    if (actor_type == 2) then

      call get_actor_type(actor_type, sim_director)
      call sim_director%work()

    else if (actor_type == 1) then

      call run_actor()
    end if

    ! Close
    call finalize()

  end subroutine entry

end program main
