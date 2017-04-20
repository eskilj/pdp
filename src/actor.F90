module actor_mod
use actor_comm
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
    ! procedure :: send
    ! procedure :: recv
    procedure :: init
    procedure :: create_msg_type
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

  ! subroutine send(this, recipient, data)
  !   class(actor), intent(in) :: this
  !   integer, intent(in) :: recipient, data

  !   call send_message(this%id, recipient, data)

  ! end subroutine send

  ! subroutine recv(self)
  !   class(actor), intent(in) :: self
  !   call recv_message(self%id)
  ! end subroutine recv

  subroutine create_new_actor()
    print *, "new actor"
  end subroutine create_new_actor

end module actor_mod