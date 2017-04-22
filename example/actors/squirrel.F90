! actor model - squirrel module

module squirrel_mod

  use actor_mod
  use squirrel_functions
  use pool
  use param_mod

  implicit none

  private
  include "mpif.h"

  integer :: ierr

  ! Squirrel type, inherits from actor
  type, public, extends(actor) :: squirrel
    integer :: cell = 0
    integer :: step = 0
    integer :: infected = 0
    integer :: pop_sum = 0
    integer :: infected_steps = 0
    real :: x = 0.0
    real :: y = 0.0
    integer, dimension(INFECTION_INTERVAL) :: inf_history = 0
  contains
     ! Override actor type procedures
    procedure :: work => squirrel_work
    procedure :: init => squirrel_init
  end type squirrel


  contains

  ! Override actor init function
  subroutine squirrel_init(this)
    class(squirrel) :: this
    type(PP_message) :: msg
    integer :: status(MPI_STATUS_SIZE)
    logical :: recv

    ! Get parent id and rank from actor init
    call this%actor_comms()

    ! Update squirrel state with seed
    this%state = -1 - this%id  
    call initialiseRNG(this%state)
    
    this%status = NEW_STATE

    do while (this%status == NEW_STATE)
      call recv_message(msg, this%parent, START_TAG)
      if (msg%tag == START_TAG) then

            this%infected = msg%data(1)
            this%x = msg%real_data(1)
            this%y = msg%real_data(2)

            this%status = READY
      end if
    end do

    call squirrelStep(this%x, this%y, this%x, this%y, this%state)
    this%cell = getCellFromPosition(this%x, this%y)

  end subroutine squirrel_init


  ! Squirrel work method
  subroutine squirrel_work(this)
    class(squirrel) :: this
    
    do

      ! Do work when ready
      if (this%status == READY) then

        call sq_step(this)

        call sq_check_birth(this)
        call sq_check_infection(this)

        this%status = WAITING

      end if

      if (this%status == WAITING) then

        ! call recv_message(msg, this%cell, CELL_TAG)

        ! select case (msg%msg_tag)

        ! case (CELL_TAG)

        !   ! Update squirrel's population influx history
        !   this%pop_sum = this%pop_sum + msg%data(1)

        !   if (this%infected == 0) then
        !     this%inf_history = cshift(this%inf_history, shift=-1)
        !     this%inf_history(1) = msg%data(1)
        !   end if

        !   this%status = READY

        ! case default ! Unknown tag

        ! end select

      end if

      ! if (shouldWorkerStop()) then
      !   EXIT
      ! end if

    end do


  end subroutine squirrel_work
  ! END SQUIRREL MAIN



  !SQUIRREL EXTRA FUNCTIONS

  subroutine sq_step(this)
    class(squirrel) :: this
    type(PP_message) :: msg

    ! Perform step, update cell
    call squirrelStep(this%x, this%y, this%x, this%y, this%state)
    this%cell = getCellFromPosition(this%x, this%y)
      
    ! Inform current cell the squirrel stepped
    msg%tag = STEP_TAG
    msg%data(1) = this%infected
    msg%data(2) = this%id

    call send_comm(msg, this%cell)
    
    this%step = this%step + 1

  end subroutine sq_step

  subroutine sq_check_birth(this)
    class(squirrel) :: this
    type(PP_message) :: msg
    integer :: child_id

    if ( MOD( this%step, BIRTH_INTERVAL ) == 0) then

      if ( willGiveBirth(real(this%pop_sum/BIRTH_INTERVAL), this%state) ) then
      
        call this%spawn_actor(child_id)

        msg%tag = START_TAG
        msg%real_data = (/ this%x, this%y /)

        call send_comm(msg, child_id)

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




