module mergesort_mod
  use pool
  use qsort_c_module
  use mpi
  use ran2_mod
  implicit none  

  integer, parameter :: DATA_LENGTH=100, SERIAL_THRESHOLD=10
contains

  subroutine workerCode()
     integer :: workerStatus, parentId, dataLength, ierr
     real(kind=8), dimension(:), allocatable :: data
     workerStatus=1
     do while (workerStatus == 1)
       parentId = getCommandData()
       call MPI_Recv(dataLength, 1, MPI_INT, parentId, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
       allocate(data(dataLength))
       call MPI_Recv(data, dataLength, MPI_DOUBLE, parentId, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
       call sort(data, dataLength)
       if (parentId == 0) call shutdownPool()
       call MPI_Send(data, dataLength, MPI_DOUBLE, parentId, 0, MPI_COMM_WORLD, ierr)
       deallocate(data)
       call workerSleep(workerStatus)
     end do
  end subroutine workerCode

  recursive subroutine sort(data, dataLength)
    integer, intent(in) :: dataLength
    real(kind=8), dimension(:), intent(inout) :: data

    integer :: pivot, workerPid, ierr

    if (dataLength < SERIAL_THRESHOLD) then
      call QsortC(data)
    else
      pivot=dataLength/2
      workerPid=startWorkerProcess()
      call MPI_Send(pivot, 1, MPI_INT, workerPid, 0, MPI_COMM_WORLD, ierr)
      call MPI_Send(data(:pivot), pivot, MPI_DOUBLE, workerPid, 0, MPI_COMM_WORLD, ierr)
      call sort(data(pivot+1:), dataLength-pivot)
      call MPI_Recv(data, pivot, MPI_DOUBLE, workerPid, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
      call merge(data, pivot, dataLength)
    end if
  end subroutine sort

  subroutine merge(data, pivot, length)
    real(kind=8), dimension(:), intent(inout) :: data
    integer, intent(in) :: pivot, length

    real(kind=8), dimension(length) :: tempData
    integer i, pre_index, post_index

    pre_index=1
    post_index=pivot+1
    do i=1,length
      if (pre_index > pivot) then
        tempData(i)=data(post_index)
        post_index=post_index+1
      else if (post_index > length) then
        tempData(i)=data(pre_index)
        pre_index=pre_index+1
      else if (data(pre_index) .lt. data(post_index)) then
        tempData(i)=data(pre_index)
        pre_index=pre_index+1
      else
        tempData(i)=data(post_index)
        post_index=post_index+1
      end if
    end do
    data=tempData
  end subroutine merge
end module mergesort_mod

program mergesort_prog
  use mergesort_mod
  use pool
  use mpi
  implicit none

  call entryPoint()
contains

  subroutine entryPoint()
    integer :: ierr, process_pool_status

    call MPI_Init(ierr)
    call processPoolInit(process_pool_status)

    if (process_pool_status == 1) then
      call workerCode()
    else if (process_pool_status == 2) then
      call mergesort_master()
    end if

    call processPoolFinalise() 
    call MPI_Finalize(ierr)
  end subroutine entryPoint

  subroutine mergesort_master()
    real(kind=8), dimension(:), allocatable :: data

    integer :: pid, ierr, len, masterStatus

    allocate(data(DATA_LENGTH))
    call generateUnsortedData(data)
    print *, "Unsorted data"
    call displayData(data)
    pid=startMergeSort(data)
    call masterPoll(masterStatus)
    do while (masterStatus == 1) 
      call masterPoll(masterStatus)
    end do
    call MPI_Recv(data, DATA_LENGTH, MPI_DOUBLE, pid, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
    print *, "Sorted data"
    call displayData(data)
    deallocate(data)
  end subroutine mergesort_master

  integer function startMergeSort(data)
    real(kind=8), dimension(:), intent(in) :: data

    integer :: pid, len, ierr

    pid=startWorkerProcess()
    len=DATA_LENGTH
    call MPI_Send(len, 1, MPI_INT, pid, 0, MPI_COMM_WORLD, ierr)
    call MPI_Send(data, len, MPI_DOUBLE, pid, 0, MPI_COMM_WORLD, ierr)
    startMergeSort=pid
  end function startMergeSort  

  subroutine displayData(data)
    real(kind=8), dimension(:), intent(in) :: data

    integer :: i
    do i=1, size(data)
      write (6, "(f10.3)", advance='no'), data(i)
    end do
    print *, ""
  end subroutine displayData

  subroutine generateUnsortedData(data)
    real(kind=8), dimension(:), intent(out) :: data

    integer :: seed, i, clocktick

    call system_clock(clocktick)
    seed=-1-clocktick
    data(1)=ran2(seed)
    do i=2, DATA_LENGTH
      data(i)=ran2(seed)
    end do    
  end subroutine generateUnsortedData
end program mergesort_prog

