module cell_mod
  use actor_mod
  use param_mod
  implicit none

  private
  include "mpif.h"

  ! Cell type, inherits from actor
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

  ! Override actor work function
  subroutine cell_work(this)
    class(cell) :: this
    type(PP_message) :: msg
    integer :: status(MPI_STATUS_SIZE)
    logical :: recv = .FALSE.

    do

      call has_messages(MPI_ANY_SOURCE, MPI_ANY_TAG, recv, status)
    
      if (should_kill_actor()) EXIT ! Exit loop if shut-down is triggered by root

      ! Handle incoming messages
      if (recv) then

        call get_message(msg, status(MPI_SOURCE), status(MPI_TAG))

        select case(msg%tag)

        case (STEP_TAG) ! SQIRR STEP TO CELL

          this%pop_history(1) = this%pop_history(1) + 1
          this%inf_history(1) = this%inf_history(1) + msg%data(1)

          msg%tag = CELL_TAG
          msg%data(1) = sum(this%pop_history)
          msg%data(2) = sum(this%inf_history)

          call send_comm(msg, status(MPI_SOURCE)) ! SEND UPDATED CELL DATA BACK TO SQUIRREL

        case (MONTH_TAG) ! NEW MONTH ALERT FROM ROOT - Update population influx history and infeciton levels

          PRINT *, "Cell :", this%id, ", Pop. Influx :", sum(this%pop_history), ", Infection Lvl :", sum(this%inf_history)

          ! write(*,"(A,I0,A,I0,A,I0,A)") "Cell :", this%id, ", Pop. Influx :", sum(this%pop_history), &
          ! ", Infection Lvl :", sum(this%inf_history), "."

          this%pop_history = cshift(this%pop_history, shift=-1)
          this%pop_history(1) = 0

          this%inf_history = cshift(this%inf_history, shift=-1)
          this%inf_history(1) = 0

          this%month = this%month + 1

        case default
          print *, "UNKNOWN TAG :", msg%tag
        end select

      end if

    end do

  end subroutine cell_work

end module cell_mod