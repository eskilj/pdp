module cell_mod
  use actor_mod
  use pool
  implicit none

  private
  include "mpif.h"

  integer :: ierr
  type(actor_msg) :: msg

  ! Cell Comm. tags
  integer, parameter :: START_TAG = 100
  integer, parameter :: STEP_TAG = 101
  integer, parameter :: MONTH_TAG = 104
  integer, parameter :: KILL_TAG = 102



  type, public, extends(actor) :: cell
    integer :: pop_influx = 0
    integer :: infection_level = 0
    integer :: month = 0
    integer, dimension(2) :: inf_history = 0
    integer, dimension(3) :: pop_history = 0
  contains
    procedure :: work => cell_work
  end type cell

  contains

  subroutine cell_work(this)
    class(cell) :: this
    integer :: status(MPI_STATUS_SIZE), request
    logical :: recv

    do

      ! Handle incoming messages
      call recv_message(msg)

      select case (msg%tag)

        case (STEP_TAG)

          ! print *, "SQIRR STEP TO CELL"

          this%pop_history(1) = this%pop_history(1) + 1
          this%inf_history(1) = this%inf_history(1) + msg%data(1)
          request = msg%data(2)

          msg%data(1) = sum(this%pop_history)
          msg%data(2) = sum(this%inf_history)

          call send_message(request, msg)

        case (MONTH_TAG)
          if (this%id == 1) print *, "MONTH UPDATE"

          this%pop_history = cshift(this%pop_history, shift=-1)
          this%pop_history(1) = 0

          this%inf_history = cshift(this%inf_history, shift=-1)
          this%inf_history(1) = 0

          this%month = this%month + 1

        case default ! Unknown tag

        end select

      if (shouldWorkerStop()) then
        EXIT
      end if    

    end do

  end subroutine cell_work

end module cell_mod