module squirrel_mod
  use actor_mod
  use squirrel_functions
  implicit none
  private
  include "mpif.h"

  integer :: ierr
  integer, parameter :: BIRTH_INTERVAL = 50
  integer, parameter :: INFECTION_INTERVAL = 50
  integer, parameter :: MIN_INFECTED_STEPS = 50

  type, public,  extends(actor) :: squirrel
    integer :: cell = 0, step = 0, infected = 0
    real :: x = 0.0, y = 0.0
  contains
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
        call MPI_Recv(this%infected, 1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, status, ierr)
    end if

  end subroutine squirrel_init

  subroutine squirrel_work(this)
    class(squirrel) :: this
    integer, dimension(2) :: cell_data
    integer, dimension(INFECTION_INTERVAL) :: inf_history = 0
    integer :: status(MPI_STATUS_SIZE), request, pop_sum , infected_steps = 0
    logical :: recv

    ! print *, "sqrl: ", this%id, "- infected: ", this%infected

    do
    
      call squirrelStep(this%x, this%y, this%x, this%y, this%state)
      this%cell = getCellFromPosition(this%x, this%y)
      
      call MPI_BSEND(this%infected, 1, MPI_INTEGER, this%cell, JUMP_TAG, MPI_COMM_WORLD, ierr)

      call MPI_IPROBE(MPI_ANY_SOURCE, JUMP_TAG, MPI_COMM_WORLD, recv, status, ierr)
      
      if (recv) then
        call MPI_Irecv(cell_data, 2, MPI_INTEGER, this%cell, JUMP_TAG, MPI_COMM_WORLD, request, ierr)

        pop_sum = pop_sum + cell_data(1)

        if (this%infected == 0) then
          inf_history = cshift(inf_history, shift=-1)
          inf_history(1) = cell_data(2)
        end if
        
      end if

      this%step = this%step + 1

      ! Check if will give birth

      if (MOD(this%step, BIRTH_INTERVAL) == 0) then
        if ( willGiveBirth(real(pop_sum/BIRTH_INTERVAL), this%state) ) then
          pop_sum = 0
          call create_new_actor()
        end if
      end if

      ! Check if will get infected
      if ( this%infected == 0 ) then
        if ( willCatchDisease(real(sum(inf_history)/INFECTION_INTERVAL), this%state) ) then
          inf_history(:) = 0
          this%infected = 1
        end if
      else !Already infected squirrel
        
        infected_steps = infected_steps + 1
        if (infected_steps .ge. MIN_INFECTED_STEPS) then
          if (willDie(this%state)) then
            EXIT
          end if
        end if

      end if

    end do

  end subroutine squirrel_work

end module squirrel_mod