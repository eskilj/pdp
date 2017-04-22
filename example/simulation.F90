

module simulation
use actor_comm
use squirrel_mod
use cell_mod
use director_mod
use actor_mod
use param_mod
use pool
implicit none
private
include "mpif.h"

  ! simulator type, inheriting from the director
  type, public, extends(director) :: simulator
    integer :: month = 0
  contains
    procedure :: work => simulation_work
    procedure :: init => init_simulation
  end type simulator

  public :: init_sim, get_actor_type, run_actor

contains


subroutine init_simulation(this)
  class(simulator) :: this
  integer :: actor_pid, infected, i, ierr
  type(PP_message) :: msg
  ! Create cell actors
  do i=1, NUM_CELLS
    call this%spawn_actor(actor_pid)
  end do

  ! Create squirrels
  do i=1, INIT_SQUIRRELS

    infected = 0
    call this%spawn_actor(actor_pid)

    if (i .le. INIT_INFECTED) infected = 1

    msg%tag = START_TAG
    msg%data = infected

    call send_comm(msg, actor_pid)

  end do

  PRINT *, "MASTER STARTED ", NUM_CELLS, " CELLS and ", INIT_SQUIRRELS, "SQUIRREL ACTORS."
  ! Set master status
  this%status = 1

end subroutine init_simulation

subroutine simulation_work(this)
  class(simulator) :: this

  do while (this%status == 1)

    call look_activity(this)
    call update_month(this)
    call check_environment(this)

  end do

  PRINT *, "WORK - STATUS :", this%status

end subroutine simulation_work

subroutine look_activity(this)
  class(simulator) :: this
  DOUBLE PRECISION :: start_time, end_time
  integer :: status(MPI_STATUS_SIZE)
  logical :: look, recvd

  look = .TRUE.
  call CPU_TIME(start_time)

  do while (look)

    ! call MPI_IPROBE(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, recvd, status, ierr)

    ! Let master wait (blocking) for incoming messages:
    if (recvd) call this%master_status()

    call CPU_TIME(end_time)
    if ( end_time-start_time .gt. MONTH_DURATION ) then
        look = .false.
        PRINT *, "NEW MONTH"
    end if

  end do

end subroutine look_activity

subroutine update_month(this)
  class(simulator) :: this
  type(PP_message) :: msg
  integer :: i

  this%month = this%month + 1

  if (this%month .eq. SIM_DURATION) then

    PRINT *, "END OF simulation"
    this%status = 0
    ! EXIT

  else if (this%month .lt. SIM_DURATION) then

    msg%tag = MONTH_TAG

    do i=1, NUM_CELLS
      
      call send_comm(msg, i)

    end do

  end if

end subroutine update_month

subroutine check_environment(this)
  class(simulator) :: this
  integer :: squirrels_alive
  squirrels_alive = get_num_workers(NUM_CELLS+1)

  if (squirrels_alive .ge. MAX_SQUIRRELS) then
    PRINT *, "ABORT"
    this%status = 0
  end if

  if (squirrels_alive == 0) then
    print *, "ABORT! NO Squirrels."
    this%status = 0
  end if

end subroutine check_environment


integer function squirrels_alive()
  integer :: workers
  workers = get_num_workers(NUM_CELLS+1)
  squirrels_alive = workers
end function squirrels_alive



subroutine get_actor_type(process_pool_status, new_actor)
  class(actor), pointer :: new_actor
  integer :: process_pool_status

  if (process_pool_status == 2) then
    allocate(simulator :: new_actor)
  else if (process_pool_status == 1 .AND. get_rank() .LE. NUM_CELLS) then
    allocate(cell :: new_actor)
  else if (process_pool_status == 1) then
    allocate(squirrel :: new_actor)
  end if

  call new_actor%init()

end subroutine get_actor_type

subroutine run_actor()
  class(actor), allocatable :: worker
  integer :: rank, workerStatus = 1

  do while (workerStatus == 1)

    rank = get_rank()

    if (rank .LE. NUM_CELLS) then
      allocate(cell :: worker)
    else
      allocate(squirrel :: worker)
    end if

    call worker%init()
    call worker%work()
    deallocate(worker)

    call workerSleep(workerStatus)

  end do

end subroutine run_actor


end module simulation