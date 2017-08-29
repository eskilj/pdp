! actor model - squirrel module

module squirrel_mod

  use actor_mod
  use squirrel_functions
  use param_mod

  implicit none

  private
  include "mpif.h"

  integer :: ierr

  ! Squirrel type, inherits from actor
  type, public, extends(actor) :: squirrel
    integer :: cell = 0
    integer :: step = 0               ! Squirrel steps taken
    integer :: infected = 0           ! Infected status, 1 or 0
    integer :: pop_sum = 0            ! Populartion sum from grid cells
    integer :: infected_steps = 0     ! Steps taken while infected
    real :: x = 0.0, y = 0.0          ! Position in environment
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

    ! Get parent id and rank from actor init
    call this%actor_comms()

    ! Update squirrel state with seed
    this%state = -1 - this%id  
    call initialiseRNG(this%state)
    
    this%status = NEW_STATE
    call wait_on_parent(this)

    call squirrelStep(this%x, this%y, this%x, this%y, this%state)
    this%cell = getCellFromPosition(this%x, this%y)
    
  end subroutine squirrel_init


  ! Squirrel work method
  subroutine squirrel_work(this)
    class(squirrel) :: this
    
    do while (this%status == READY)

      call sq_step(this)

      call wait_cell_data(this)

      if (this%status == DEAD_STATUS) EXIT

      call sq_check_birth(this)
      call sq_check_infection(this)

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
    real :: avg_pop

    if (this%status == DEAD_STATUS) RETURN ! An alive squirrel should not be here

    ! Check if the squirrel will give birth every BIRTH_INTERVAL step
    if ( MOD( this%step, BIRTH_INTERVAL ) == 0) then

      avg_pop = real(this%pop_sum/BIRTH_INTERVAL) ! Calculate average population

      if ( willGiveBirth( avg_pop, this%state) ) then
      
        ! Create new squirrel, send parent position
        call this%spawn_actor(child_id)

        msg%tag = START_TAG
        msg%real_data = (/ this%x, this%y /)
        call send_comm(msg, child_id)

      end if

      this%pop_sum = 0 ! Reset population influx history

    end if

  end subroutine sq_check_birth


  subroutine sq_check_infection(this)
    class(squirrel) :: this

    ! Check if will get infected
    if ( this%infected == 0 ) then
      
      if ( willCatchDisease(real(sum(this%inf_history)/INFECTION_INTERVAL), this%state) ) then
        this%infected = 1
      end if

    else ! If Already infected squirrel

      ! Check if squirrel will die
      if (this%infected_steps .ge. MIN_INFECTED_STEPS .AND. willDie(this%state)) then

        this%status = DEAD_STATUS
        RETURN ! return if squirrel dies
      end if

      this%infected_steps = this%infected_steps + 1 ! iterate infected_steps

    end if

  end subroutine sq_check_infection

  ! Wait on data from cell
  subroutine wait_cell_data(this)
    class(squirrel) :: this
    type(PP_message) :: msg
    integer :: status(MPI_STATUS_SIZE)
    logical :: recv
    recv = .FALSE.

    do while (recv .eqv. .FALSE.)

      call has_messages(MPI_ANY_SOURCE, MPI_ANY_TAG, recv, status)

      if (should_kill_actor()) then
        this%status = DEAD_STATUS
        RETURN
      end if

    end do
 
    call get_message(msg, this%cell, CELL_TAG)

    ! Update data when recieved from current cell
    this%pop_sum = this%pop_sum + msg%data(1)

    this%inf_history = cshift(this%inf_history, shift=-1)
    this%inf_history(1) = msg%data(2)

  end subroutine wait_cell_data

  ! Wait for data from parent before starting work
  subroutine wait_on_parent(this)
    class(squirrel) :: this
    type(PP_message) :: msg
    integer :: status(MPI_STATUS_SIZE)
    logical :: recv

    do while (this%status == NEW_STATE)

      call has_messages(this%parent, START_TAG, recv, status)

      if (should_kill_actor()) then
        this%status = DEAD_STATUS
        RETURN
      end if

      if (recv) then

        call recv_message(msg, this%parent, START_TAG)

        ! Update squirrel using the data from the parent
        this%infected = msg%data(1)
        this%x = msg%real_data(1)
        this%y = msg%real_data(2)

        this%status = READY ! Squirrel is now ready to work

      end if

    end do

  end subroutine wait_on_parent

end module squirrel_mod




