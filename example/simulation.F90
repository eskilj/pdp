! Simulation specific director

module simulation
  use actor_comm
  use squirrel_mod
  use cell_mod
  use director_mod
  use actor_mod
  use param_mod
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

  public :: initialize, finalize, get_actor_type, run_actor

contains



  ! Simulation type procedures

  ! Override actor init routine
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
      call this%spawn_actor(actor_pid)

      ! Set squirrel's infected status - infected the first (INIT_INFECTED) ones
      infected = 0
      if (i .le. INIT_INFECTED) infected = 1

      msg%tag = START_TAG
      msg%data = infected
      call send_comm(msg, actor_pid) ! Send infected status to squirrel

    end do

    PRINT *, "MASTER STARTED ", NUM_CELLS, " CELLS and ", INIT_SQUIRRELS, "SQUIRREL ACTORS."
    this%status = 1
    
  end subroutine init_simulation


  ! Override actor work procedure
  subroutine simulation_work(this)
    class(simulator) :: this
    DOUBLE PRECISION :: start_time, end_time
    integer :: status(MPI_STATUS_SIZE), ierr
    logical :: recvd = .FALSE.

    call CPU_TIME(start_time)

    do while (this%status == 1)

      call MPI_IPROBE(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, recvd, status, ierr)

      ! Let master wait (blocking) for incoming messages:
      if (recvd) then
        call this%master_status()
        call check_environment(this)
      end if


      call CPU_TIME(end_time)
      if ( end_time - start_time .gt. MONTH_DURATION ) then
        call CPU_TIME(start_time)
        call update_month(this)
      end if

    end do

  end subroutine simulation_work

  ! End actor procedures



  ! Update month, alert cells
  subroutine update_month(this)
    class(simulator) :: this
    type(PP_message) :: msg
    integer :: i

    this%month = this%month + 1
    PRINT *, "=== Simulation month ", this%month, " ==="
    PRINT *, "=== Squirrels alive ", get_num_workers(NUM_CELLS+1), " ==="


    if (this%month .eq. SIM_DURATION) then

      PRINT *, "END OF SIMULATION."
      this%status = 0

    else if (this%month .lt. SIM_DURATION) then

      msg%tag = MONTH_TAG

      do i=1, NUM_CELLS
        call send_comm(msg, i)
      end do

    end if

  end subroutine update_month

  ! If no squirrels, or reached limit, shut down simulation
  subroutine check_environment(this)
    class(simulator) :: this
    integer :: squirrels_alive

    squirrels_alive = get_num_workers(NUM_CELLS+1)

    if (squirrels_alive .ge. MAX_SQUIRRELS) then
      PRINT *, "ABORT - MAXIMUM SQUIRREL LIMIT REACHED! SHUTTING DOWN SIMULATION."
      this%status = 0
    end if

    if (squirrels_alive == 0) then
      print *, "NO SQUIRRELS REMAINIGN. SHUTTING DOWN SIMULATION."
      this%status = 0
    end if

  end subroutine check_environment



  ! Get actor type for root process
  subroutine get_actor_type(process_pool_status, new_actor)
    class(actor), pointer :: new_actor
    integer :: process_pool_status

    if (process_pool_status == 2) then
      allocate(simulator :: new_actor)
    end if

    call new_actor%init()

  end subroutine get_actor_type

  ! Allocate and run actors, then sleep if they die. 
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

      call actor_sleep_status(workerStatus)

    end do

  end subroutine run_actor


end module simulation
! End simulation
