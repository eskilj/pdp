module squirrel_mod
  use actor_mod
  use squirrel_functions
  use pool
  implicit none

  private
  include "mpif.h"

  integer :: ierr
  type(actor_msg) :: msg

  ! Squirrel life-cycle parameters
  integer, parameter :: BIRTH_INTERVAL = 50
  integer, parameter :: INFECTION_INTERVAL = 50
  integer, parameter :: MIN_INFECTED_STEPS = 50

  ! Squirrel state
  integer, parameter :: NEW_STATE = 0
  integer, parameter :: READY = 1
  integer, parameter :: WAITING = 2

  integer, parameter :: START_TAG = 100
  integer, parameter :: CELL_TAG = 101
  integer, parameter :: KILL_TAG = 102

  ! Squirrel type, inherits from actor
  type, public,  extends(actor) :: squirrel
    integer :: cell = 0
    integer :: infected_steps = 0, step = 0
    integer :: infected = 0
    integer :: pop_sum = 0
    integer, dimension(INFECTION_INTERVAL) :: inf_history = 0
    real :: x = 0.0
    real :: y = 0.0
  contains
     ! Override actor type procedures
    procedure :: work => squirrel_work
    procedure :: init => squirrel_init
  end type squirrel

  contains

  subroutine squirrel_init(this, p_rank, p_parent)
    class(squirrel) :: this
    integer :: p_rank, p_parent
    integer :: status(MPI_STATUS_SIZE)

    this%id = p_rank
    this%state = -1 - p_rank
    this%parent = p_parent
  

    call initialiseRNG(this%state)
    call squirrelStep(0.0, 0.0, this%x, this%y, this%state)
    this%cell = getCellFromPosition(this%x, this%y)

  end subroutine squirrel_init

  subroutine squirrel_work(this)
    class(squirrel) :: this
    
    do

      
    if (this%status == NEW_STATE) then

      call recv_message(msg)

      select case (msg%tag)

        case (START_TAG)

          this%infected = msg%data(1)

          PRINT *, "INIT SQ"
          this%status = READY

        case default ! Unknown tag

      end select

    end if




      ! Do work when ready
      if (this%status == READY) then

        call sq_step(this)

        call sq_check_birth(this)
        call sq_check_infection(this)

        this%status = WAITING

      end if

      if (this%status == WAITING) then

        call recv_message(msg)

        select case (msg%tag)

        case (CELL_TAG)

        case default ! Unknown tag

        end select

      end if

      if (shouldWorkerStop()) then

        EXIT

      end if

    end do


  end subroutine squirrel_work
  ! END SQUIRREL MAIN



  !SQUIRREL EXTRA FUNCTIONS

  subroutine sq_step(this)
    class(squirrel) :: this

    ! Perform step, update cell
    call squirrelStep(this%x, this%y, this%x, this%y, this%state)
    this%cell = getCellFromPosition(this%x, this%y)
      
    ! Inform current cell the squirrel stepped
    msg%tag = CELL_TAG
    msg%data(1) = this%infected
    msg%data(2) = this%id

    call send_message(this%cell, msg)
    
    this%step = this%step + 1

  end subroutine sq_step

  subroutine sq_check_birth(this)
    class(squirrel) :: this
    integer :: child_id

    if ( MOD( this%step, BIRTH_INTERVAL ) == 0) then

      if ( willGiveBirth(real(this%pop_sum/BIRTH_INTERVAL), this%state) ) then
      
        child_id = startWorkerProcess()

      end if
      this%pop_sum = 0

    end if

  end subroutine sq_check_birth



  subroutine sq_check_infection(this)
    class(squirrel) :: this

    ! Check if will get infected
    if ( this%infected == 0 ) then
      
      if ( willCatchDisease(real(sum(this%inf_history)/INFECTION_INTERVAL), this%state) ) then
        this%infected = 1
      end if

    else !Already infected squirrel
          
      this%infected_steps = this%infected_steps + 1

      if (this%infected_steps .ge. MIN_INFECTED_STEPS) then
        if (willDie(this%state)) then

          print *, "SQRL died - ", this%id

        end if
      end if

    end if

  end subroutine sq_check_infection

end module squirrel_mod




