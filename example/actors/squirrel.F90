module squirrel_mod
  use actor_mod
  use squirrel_functions
  use pool
  implicit none
  private
  include "mpif.h"

  integer :: ierr

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
    integer :: step = 0
    integer :: infected = 0
    integer :: pop_sum = 0
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

    if (p_parent == 0) then
        ! print *, "NEW SQ BORN. PID: ", this%id, "- Parent: ", this%parent
        call MPI_Recv(this%infected, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, status, ierr)
    else 
      ! print *, "NEW SQ BORN. PID: ", this%id, "- Parent: ", this%parent
    end if

  end subroutine squirrel_init

  subroutine squirrel_work(this)
    class(squirrel) :: this
    integer, dimension(2) :: cell_data
    integer, dimension(INFECTION_INTERVAL) :: inf_history = 0
    integer :: status(MPI_STATUS_SIZE), request, child_id, infected_steps = 0
    logical :: recv
    type(actor_msg) :: msg

    do

      ! Handle incoming messages
      do while (has_messages(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD)) 
        
        call recv_message(msg)

        select case (msg%tag)

        case (CELL_TAG)

          this%pop_sum = this%pop_sum + msg%data(1)

          if (this%infected == 0) then
            inf_history = cshift(inf_history, shift=-1)
            inf_history(1) = msg%data(2)
          end if

        case default ! Unknown tag

        end select

      end do

      ! Do work when ready
      if (this%status == READY) then

        call sq_step(this)


      end if


      if (shouldWorkerStop()) then
        EXIT
      end if

      call MPI_IPROBE(this%cell, MPI_ANY_TAG, MPI_COMM_WORLD, recv, status, ierr)
      
      if (recv) then

        call MPI_Irecv(cell_data, 2, MPI_INTEGER, this%cell, JUMP_TAG, MPI_COMM_WORLD, request, ierr)

        this%step = this%step + 1

        ! Check if will give birth

        ! Check if will get infected
        if ( this%infected == 0 ) then
          if ( willCatchDisease(real(sum(inf_history)/INFECTION_INTERVAL), this%state) ) then
            this%infected = 1
          end if
        else !Already infected squirrel
          
          infected_steps = infected_steps + 1
          if (infected_steps .ge. MIN_INFECTED_STEPS) then
            if (willDie(this%state)) then
              print *, "SQRL died - ", this%id
              EXIT
            end if
          end if
        end if
        
      end if

    end do

  end subroutine squirrel_work

  subroutine sq_step(this)
    class(squirrel) :: this

    ! Perform step, update cell
    call squirrelStep(this%x, this%y, this%x, this%y, this%state)
    this%cell = getCellFromPosition(this%x, this%y)
      
    ! Inform current cell the squirrel stepped
    call MPI_BSEND(this%infected, 1, MPI_INTEGER, this%cell, JUMP_TAG, MPI_COMM_WORLD, ierr)

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

end module squirrel_mod




