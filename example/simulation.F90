module simulation
! use factory_mod
use squirrel_mod
use cell_mod
use director_mod
use actor_mod
use pool
implicit none
private
include "mpif.h"

  integer :: ierr
  integer, parameter :: BUFF_SIZE = 10240
  character :: BUFFER (BUFF_SIZE)

  ! Simulation specific parameters
  integer, parameter :: NUM_CELLS=16      ! Cells in environment
  integer, parameter :: MAX_SQUIRRELS=200  ! Max squirrels
  integer, parameter :: INIT_SQUIRRELS=34 ! Number of squirrels initially 
  integer, parameter :: INIT_INFECTED=4   ! Number of squirrels initially infected
  integer, parameter :: SIM_DURATION=24   ! Length of simulation, in months

  public :: run

contains

subroutine run()

  ! type(director) :: sim_director
  integer :: process_pool_status

  call MPI_Init(ierr)
  call MPI_Buffer_attach(BUFFER, BUFF_SIZE, ierr)

  call create_type()

  call processPoolInit(process_pool_status)
  
  ! call init_comm(process_pool_status)

  ! sim_director = director()

  if (process_pool_status == 1) then
    call become_worker()
  else if (process_pool_status == 2) then
    call create_actors()
  end if

  call processPoolFinalise() 
  call MPI_Finalize(ierr)

end subroutine run

subroutine create_actors()
  ! class(actor), pointer :: new_actor
  ! type(product) :: sqrl, cell
  ! type(product), dimension(3) :: products
  ! type(Factory) :: sim_factroy
  integer :: i, workerPid, masterStatus, infected, month
  real :: interval, t_start, t_end
  ! allocate(squirrel :: new_actor)
  interval = 0.1
  month = 0
  ! sqrl = product("squirrel", new_actor)
  ! products(:) = sqrl
  ! call sim_factroy%init_factory(products)

  PRINT *, "START SIM!"

  do i=1, NUM_CELLS
    workerPid = startWorkerProcess()
  end do

  do i=1, INIT_SQUIRRELS
    infected = 0
    workerPid = startWorkerProcess()
    if (i .le. INIT_INFECTED) then
      infected = 1
    end if
    call MPI_BSEND(infected, 1, MPI_INTEGER, workerPid, 0, MPI_COMM_WORLD, ierr)
  end do

  
  ! Start timer
  call CPU_TIME(t_start)


  do ! START master LOOP
    ! Let master wait (blocking) for incoming messages:
    call masterPoll(masterStatus)

    ! Initialize shutdown if 'invalid' masterStatus
    if (masterStatus .ne. 1) EXIT 


    ! Monthly interval
    call CPU_TIME(t_end)

    if ( t_end - t_start > interval) then
      print *, "NEW MONTH"

      month = month + 1
      if (month .eq. SIM_DURATION) then

        PRINT *, "END OF simulation"
        EXIT

      else if (month .lt. SIM_DURATION) then

        do i=1, NUM_CELLS
          call MPI_BSEND(1, 1, MPI_INTEGER, i, 22, MPI_COMM_WORLD, ierr)
        end do

      end if

      call CPU_TIME(t_start)

    end if

    if (squirrels_alive() .ge. MAX_SQUIRRELS) then
      PRINT *, "ABORT"
      ! call MPI_ABORT(MPI_COMM_WORLD, 1, ierr)
    end if

    ! Too many squirrels
    if (squirrels_alive() == 0) then
      print *, "ABORT! NO Squirrels - Squirrels alive: ", squirrels_alive()
      print *, "MONTH ", month
      EXIT
    end if

  end do !END MASTER LOOP

  PRINT *, "END MASTER LOOP"

end subroutine create_actors

subroutine become_worker()

    class(actor), pointer :: new_actor
    integer :: workerStatus = 1
    integer :: parentId, myRank
    logical :: stop_sim

    do while (workerStatus == 1)

      if(shouldWorkerStop()) EXIT

      myRank = get_rank()
      parentId = getCommandData()

      if (myRank .LE. NUM_CELLS) then
        allocate(cell :: new_actor)
      else if (myRank .GT. NUM_CELLS .AND. myRank .LT. NUM_CELLS + MAX_SQUIRRELS) then
        allocate(squirrel :: new_actor)
          ! print *, "sq: ", myRank, "parent: ", parentId
      end if

      call new_actor%init(myRank, parentId)

      call new_actor%work()
      deallocate(new_actor)

      if(shouldWorkerStop()) EXIT

      call workerSleep(workerStatus)

    end do

      ! write(*,"(A,I0,A,I0)") "Ended ", myRank

end subroutine become_worker

integer function squirrels_alive()
  integer :: workers
  workers = get_num_workers(NUM_CELLS+1)
  squirrels_alive = workers
end function squirrels_alive

end module simulation