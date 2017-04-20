module actor_comm
  implicit none
  private
  include "mpif.h"

  integer :: ierr, actor_message_type

  type, public :: actor_msg
    integer :: tag
    integer, dimension(2) :: data = 0
    real, dimension(2) :: real_data = 0.0
  end type actor_msg

  public :: create_type, send_message, recv_message, has_messages

  contains

  subroutine create_type()

    ! introduce new variables related to derived datatypes
    type(actor_msg) :: passon
    integer :: blocklen(3), type(3)
    integer (MPI_ADDRESS_KIND), dimension(4) :: disp

    ! Create all necessary info for the derived datatype
    call MPI_GET_ADDRESS(passon%tag, disp(1), ierr)
    call MPI_GET_ADDRESS(passon%data, disp(2), ierr)
    call MPI_GET_ADDRESS(passon%real_data, disp(3), ierr)

    
    disp(3) = disp(3) - disp(2)
    disp(2) = disp(2) - disp(1)
    disp(1) = 0

    blocklen(1) = 1
    blocklen(2) = 2
    blocklen(3) = 2

    type(1) = MPI_INTEGER
    type(2) = MPI_INTEGER
    type(3) = MPI_REAL

    ! Create the new datatype, called 'newtype' and commit it
    call MPI_TYPE_CREATE_STRUCT(3, blocklen, disp, type, actor_message_type, ierr)
    call MPI_TYPE_COMMIT(actor_message_type, ierr)

  end subroutine create_type

  subroutine send_message(recipient, msg)
    type(actor_msg) :: msg
    integer :: recipient

    call MPI_BSEND(msg, 1, actor_message_type, recipient, msg%tag, MPI_COMM_WORLD, ierr)

  end subroutine send_message


  subroutine recv_message(msg)
    type(actor_msg), intent(inout) :: msg
    integer :: status(MPI_STATUS_SIZE), request
    logical :: recv

    ! print *, "actor recv: ", id

    call MPI_IPROBE(MPI_ANY_SOURCE, 1, MPI_COMM_WORLD, recv, status, ierr)

    if (recv) then
      
      call MPI_Irecv(msg, 1, actor_message_type, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, request, ierr)

    end if

  end subroutine recv_message

  logical function has_messages(source, tag, comm)

    integer, intent(in) :: source, tag, comm
    integer :: status(MPI_STATUS_SIZE)
    logical :: recv

    call MPI_IPROBE(source, tag, comm, recv, status, ierr)
    has_messages = recv

  end function has_messages


end module actor_comm