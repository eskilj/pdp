module actor_mod
use actor_comm
use pool
implicit none
private
include "mpif.h"

  type, public :: actor
    integer :: id
    integer :: state
    integer :: status
    integer :: parent
    integer :: rank
  contains
    final :: finalize
    procedure :: work
    procedure :: init
    procedure :: spawn_actor
    procedure :: actor_comms
  end type actor

  public :: free_type, recv_message, has_messages, get_message, create_comm, PP_message, send_comm

contains

  subroutine init(this)
    class(actor) :: this
    call this%actor_comms()
    
  end subroutine init

  subroutine work(this)
    class(actor) :: this
    print *, "Actor work"
  end subroutine work

  subroutine finalize(this)
    type(actor) :: this
    ! print *, "Shutting down actor."
  end subroutine

  subroutine spawn_actor(this, process_id)
    class(actor) :: this
    integer, intent(out) :: process_id
    process_id = startWorkerProcess()

  end subroutine spawn_actor

  subroutine actor_comms(this)
    class(actor) :: this
    
    this%id = get_rank()
    this%parent = getCommandData()

  end subroutine actor_comms
  

end module actor_mod