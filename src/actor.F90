module actor_mod
use actor_comm
use pool
implicit none

  integer, parameter :: JUMP_TAG = 11
  integer, parameter :: NEW_MONTH_TAG = 22
  integer, parameter :: SHUTDOWN_TAG = 33

  type actor
    integer :: id
    integer :: state
    integer :: status
    integer :: parent
  contains
    final :: finalize
    procedure :: work
    procedure :: init
    procedure :: create_msg_type
    procedure :: spawn_actor
  end type actor

contains

  subroutine create_msg_type(this)
    class(actor) :: this
    call create_type()
  end subroutine create_msg_type

  subroutine init(this, p_rank, p_parent)
    class(actor) :: this
    integer :: p_rank, p_parent
    this%id = p_rank
    this%parent = p_parent
    ! print *, "Actor init"
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


end module actor_mod