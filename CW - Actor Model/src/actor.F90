module actor_mod
use actor_comm
use pool
implicit none
private
include "mpif.h"

  ! Derived actor type - Base type for other actors
  ! Communication methods implemeted in actor_comm module
  type, public :: actor
    integer :: id
    integer :: state
    integer :: status
    integer :: parent
  contains
    procedure :: work
    procedure :: init
    procedure :: spawn_actor
    procedure :: actor_comms
  end type actor

  public :: free_type, recv_message, has_messages, get_message, commit_type, PP_message, send_comm, should_kill_actor

contains

  ! Basic init routine
  subroutine init(this)
    class(actor) :: this
    call this%actor_comms()
  end subroutine init

  ! Main work routine other actors MUST override
  subroutine work(this)
    class(actor) :: this
    print *, "Actor work - MUST BE IMPLEMENTED BY INHERITING TYPES."
  end subroutine work

  ! Reqest new actor on process
  subroutine spawn_actor(this, process_id)
    class(actor) :: this
    integer, intent(out) :: process_id
    process_id = startWorkerProcess()

  end subroutine spawn_actor

  ! Get actor rank and parent
  subroutine actor_comms(this)
    class(actor) :: this
    
    this%id = PP_myRank
    this%parent = getCommandData()

  end subroutine actor_comms

  ! Helper method from pool - returns kill status
  logical function should_kill_actor()
    should_kill_actor = shouldWorkerStop()
  end function should_kill_actor
  

end module actor_mod