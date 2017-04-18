module actor_comm
  implicit none
  private
  include "mpif.h"

  type actor_msg
    integer :: sender
    integer :: recipient
    integer :: data
  end type actor_msg

  public :: create_type, send_message, recv_message

  contains

  subroutine create_type()

    ! introduce new variables related to derived datatypes
    type(actor_msg) :: passon
    integer :: blocklen(3), type(3)
    integer (MPI_ADDRESS_KIND), dimension(3) :: disp
    integer (MPI_ADDRESS_KIND) :: base
    integer :: newtype, ierr

    ! Create all necessary info for the derived datatype
    call MPI_GET_ADDRESS(passon%sender, disp(1), ierr)
    call MPI_GET_ADDRESS(passon%recipient, disp(2), ierr)
    call MPI_GET_ADDRESS(passon%data, disp(3), ierr)

    base = disp(1)
    disp(1) = disp(1) - base
    disp(2) = disp(2) - base
    disp(3) = disp(3) - base

    blocklen(1) = 1
    blocklen(2) = 1
    blocklen(3) = 1

    type(1) = MPI_INTEGER
    type(2) = MPI_INTEGER
    type(3) = MPI_INTEGER

    ! Create the new datatype, called 'newtype' and commit it
    call MPI_TYPE_CREATE_STRUCT(3,blocklen,disp,type,newtype,ierr)
    call MPI_TYPE_COMMIT(newtype, ierr)

  end subroutine create_type

  subroutine send_message(sender, recipient, data)
    integer, intent(in) :: sender, recipient, data
    type(actor_msg) :: msg
    integer :: ierr, newtype, comm

    comm = MPI_COMM_WORLD

    msg = actor_msg(sender=sender, recipient=recipient, data=data)
    call MPI_BSEND(msg, 1, newtype, recipient, 1, comm, ierr)

  end subroutine send_message


  subroutine recv_message(id)
    integer, intent(in) :: id
    type(actor_msg) :: msg
    integer :: ierr, newtype
    integer :: status(MPI_STATUS_SIZE)
    logical :: recv

    ! print *, "actor recv: ", id

    call MPI_IPROBE(MPI_ANY_SOURCE, 1, MPI_COMM_WORLD, recv, status, ierr)

    if (recv) then
      print *, "got mail"
    end if

  end subroutine recv_message

end module actor_comm