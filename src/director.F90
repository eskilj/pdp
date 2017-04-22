module director_mod
use actor_mod
use pool
implicit none
private
include "mpif.h"

  integer :: ierr

  ! Comm buffers
  integer, parameter :: BUFF_SIZE = 10240
  character :: BUFFER (BUFF_SIZE)

  ! Director type
  type, public, extends(actor) :: director
  contains
    procedure :: initialize
    procedure :: director_finalize
    procedure :: run
    procedure :: master_status
  end type director

  public :: init_sim

  contains

  subroutine run(this)
    class(director) :: this
  end subroutine run

subroutine initialize(this, process_pool_status)
  class(director) :: this
  integer :: process_pool_status

  call MPI_Init(ierr)
  call MPI_Buffer_attach(BUFFER, BUFF_SIZE, ierr)
  call processPoolInit(process_pool_status)

end subroutine initialize

subroutine director_finalize(this)
  class(director) :: this

  call processPoolFinalise()
  call free_type()
  call MPI_Finalize(ierr)

end subroutine director_finalize

! Global simulation methods

  subroutine init_sim(process_pool_status)
    integer, intent(out) :: process_pool_status

    call MPI_Init(ierr)
    call MPI_Buffer_attach(BUFFER, BUFF_SIZE, ierr)

    call create_comm()

    call processPoolInit(process_pool_status)

  end subroutine init_sim

  subroutine master_status(this)
    class(director) :: this

    call masterPoll(this%status)

  end subroutine master_status

end module director_mod


