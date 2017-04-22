program main
  use actor_mod
  use simulation

  implicit none

  ! call run()
  call entry()
  contains

  subroutine entry()
    type(simulator) :: sim_director
    class(actor), pointer :: new_actor
    integer :: actor_type
    ! Initialize MPI via director
    call init_sim(actor_type)

    if (actor_type == 2) then
      
      call get_actor_type(actor_type, new_actor)
      call new_actor%work()

    else if (actor_type == 1) then
      call run_actor()
    end if

    ! Close
    call sim_director%director_finalize()

  end subroutine entry

end program main
