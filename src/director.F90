module director_mod
use actor_mod
use pool
implicit none
private
include "mpif.h"

  integer :: ierr

  integer, parameter :: BUFF_SIZE = 10240
  character :: BUFFER (BUFF_SIZE)






  ! Director type
  type, public, extends(actor) :: director
    integer :: active_processes
  contains
    procedure :: director_finalize
  end type director

  interface director
    procedure :: initialize
  end interface director

  public :: init_comm

  contains






subroutine init_comm(process_pool_status)
  integer, intent(inout) :: process_pool_status
  call MPI_Init(ierr)
  call MPI_Buffer_attach(BUFFER, BUFF_SIZE, ierr)

  call processPoolInit(process_pool_status)

end subroutine init_comm


function initialize() result(this)
  type(director) :: this
  integer :: process_pool_status
  call MPI_Init(ierr)
  call MPI_Buffer_attach(BUFFER, BUFF_SIZE, ierr)
  call processPoolInit(process_pool_status)
  print *, "Creating director."

  ! call MPI_Comm_rank(MPI_COMM_WORLD, this%id, ierr)

end function initialize

subroutine director_finalize(this)
  class(director) :: this
  ! call MPI_Buffer_detach(BUFFER, BUFF_SIZE, ierr)
  call processPoolFinalise() 
  call MPI_Finalize(ierr)
  print *, "closing director."

end subroutine director_finalize

end module director_mod