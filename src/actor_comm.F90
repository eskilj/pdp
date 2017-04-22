module actor_comm
  implicit none
  private
  include "mpif.h"

  integer :: actor_message_type, newtype
  integer:: ierr
  integer :: comm = MPI_COMM_WORLD

  type, public :: PP_message
    integer :: tag = -1
    integer, dimension(2) :: data = 0
    real, dimension(2) :: real_data = 0.0
  end type

  public :: free_type, recv_message, has_messages, get_message, create_comm, send_comm

  contains

  subroutine create_comm()

  type(PP_message) :: passon
  integer :: blocklen(3), type(3)
  integer (MPI_ADDRESS_KIND), dimension(3) :: disp
  integer (MPI_ADDRESS_KIND) :: base
  integer :: ierr

  ! Create all necessary info for the derived datatype
  call MPI_GET_ADDRESS(passon%tag,disp(1),ierr)
  call MPI_GET_ADDRESS(passon%data,disp(2),ierr)
  call MPI_GET_ADDRESS(passon%real_data,disp(3),ierr)

  base = disp(1)
  disp(1) = disp(1) - base
  disp(2) = disp(2) - base
  disp(3) = disp(3) - base

  blocklen(1) = 1
  blocklen(2) = 2
  blocklen(2) = 2

  type(1) = MPI_INTEGER
  type(2) = MPI_INTEGER
  type(3) = MPI_REAL

  ! Create the new datatype, called 'newtype' and commit it
  call MPI_TYPE_CREATE_STRUCT(3,blocklen,disp,type,newtype,ierr)
  call MPI_TYPE_COMMIT(newtype,ierr)

end subroutine create_comm

subroutine send_comm(passon, recipient)

  type(PP_message) :: passon
  integer :: recipient

  call MPI_BSEND(passon, 1, newtype, recipient, passon%tag, MPI_COMM_WORLD, ierr)

  ! print *, "DATA: ", passon%data, "REAL_DATA: ", passon%real_data, "TAG: ", passon%tag

end subroutine send_comm

  subroutine free_type()
    call MPI_Type_Free(newtype, ierr)
  end subroutine free_type

  subroutine recv_message(msg, source, tag)

    type(PP_message), intent(out) :: msg
    integer, intent(in) :: source, tag
    integer :: status(MPI_STATUS_SIZE), request
    logical :: recv

    call MPI_IPROBE(source, tag, MPI_COMM_WORLD, recv, status, ierr)

    if (recv) then
      
      call MPI_Irecv(msg, 1, newtype, status(MPI_SOURCE), status(MPI_TAG), MPI_COMM_WORLD, request, ierr)

    end if

  end subroutine recv_message

  subroutine has_messages(source, tag, recv, status)

    integer, intent(in) :: source, tag
    integer, intent(out) :: status(MPI_STATUS_SIZE)
    logical, intent(out) :: recv

    call MPI_IPROBE(source, tag, comm, recv, status, ierr)

  end subroutine has_messages

  subroutine get_message(msg, source, tag)
    type(PP_message), intent(out) :: msg
    integer, intent(in) :: source, tag
    call MPI_RECV(msg, 1, newtype, source, tag, comm, MPI_STATUS_IGNORE, ierr)
    
  end subroutine get_message


end module actor_comm