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

  ! Director type - extends actor
  type, public, extends(actor) :: director
  contains
    procedure :: master_status
  end type director

  public :: initialize, finalize, actor_sleep_status, get_rank, get_num_workers

contains


  ! Director type bound routine
  subroutine master_status(this)
    class(director) :: this
    call masterPoll(this%status)
  end subroutine master_status

  ! Call worker sleep 
  subroutine actor_sleep_status(workerStatus)
    integer :: workerStatus
    call workerSleep(workerStatus)
  end subroutine actor_sleep_status

  ! Get number of active workers from process pool
  integer function get_num_workers(pp_start)
    integer :: pp_start
    get_num_workers = INT(count(PP_active(pp_start:)))
  end function get_num_workers

  ! Get process rank
  integer function get_rank()
    get_rank = PP_myRank
  end function get_rank

  ! Simulation life-cycle methods !

  ! INIT SIMULATION
  subroutine initialize(process_pool_status)
    integer, intent(out) :: process_pool_status

    call MPI_Init(ierr)
    call MPI_Buffer_attach(BUFFER, BUFF_SIZE, ierr)
    call processPoolInit(process_pool_status)
    
    call commit_type()
    
  end subroutine initialize

  ! END SIMULATION
  subroutine finalize()

    call processPoolFinalise()
    call free_type()
    call MPI_Finalize(ierr)

  end subroutine finalize

end module director_mod


