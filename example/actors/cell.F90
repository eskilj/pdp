module cell_mod
  use actor_mod
  use pool
  implicit none

  private
  include "mpif.h"

  integer :: ierr

  type, public, extends(actor) :: cell
  integer :: pop_influx = 0
  integer :: infection_level = 0
  integer :: month
  contains
    procedure :: work => cell_work
  end type cell

contains

  subroutine cell_work(this)
    class(cell) :: this
    integer, dimension(2) :: frog_data, inf_history = 0
    integer, dimension(3) :: pop_history = 0
    integer :: frog_inf
    integer :: status(MPI_STATUS_SIZE), request
    logical :: recv

    do

      if (shouldWorkerStop()) then
        EXIT
      end if

      call MPI_IPROBE(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, recv, status, ierr)

      if (recv) then

        select case(status(MPI_TAG))
          
          case (JUMP_TAG) ! Handle message from squirrel

            call MPI_Irecv(frog_inf, 1, MPI_INTEGER, MPI_ANY_SOURCE, JUMP_TAG, MPI_COMM_WORLD, request, ierr)

            pop_history(1) = pop_history(1) + 1
            inf_history(1) = inf_history(1) + frog_inf

            frog_data(1) = sum(pop_history)
            frog_data(2) = sum(inf_history)

            call MPI_BSEND(frog_data, 2, MPI_INTEGER, status(MPI_SOURCE), JUMP_TAG, MPI_COMM_WORLD, ierr)

          case (NEW_MONTH_TAG) ! Handle new month

            print *, "NEW month"

            pop_history = cshift(pop_history, shift=-1)
            pop_history(1) = 0

            inf_history = cshift(inf_history, shift=-1)
            inf_history(1) = 0

            this%month = this%month + 1

          case (SHUTDOWN_TAG) ! Handle shut-down

            print *, "SHUTDOWN TAG"
            ! EXIT

          case default ! Handle default (not known)
            PRINT *, "ERROR CELL"
          end select

      end if      

    end do

    ! print *, "SHUTDOWN CELL: ", this%id

  end subroutine cell_work

end module cell_mod