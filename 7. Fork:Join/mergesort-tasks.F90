module mergesort_mod
  use qsort_c_module
  use ran2_mod
  implicit none  

  integer, parameter :: DATA_LENGTH=100, SERIAL_THRESHOLD=10
contains

  recursive subroutine sort(data, dataLength)
    integer, intent(in) :: dataLength
    real(kind=8), dimension(:), intent(inout) :: data

    integer :: pivot

    if (dataLength < SERIAL_THRESHOLD) then
      call QsortC(data)
    else
      pivot=dataLength/2
!$OMP task shared(data)
      call sort(data(:pivot), pivot)
!$OMP end task
!$OMP task shared(data)
      call sort(data(pivot+1:), dataLength-pivot) 
!$OMP end task
!$OMP taskwait
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
  implicit none

  call entryPoint()
contains

  subroutine entryPoint()    
    real(kind=8), dimension(:), allocatable :: data

    call omp_set_nested(1)
    allocate(data(DATA_LENGTH))
    call generateUnsortedData(data)
    print *, "Unsorted data"
    call displayData(data)
!$OMP parallel
!$OMP single
    call sort(data, DATA_LENGTH)
!$OMP end single
!$OMP end parallel
    print *, "Sorted data"
    call displayData(data)
    deallocate(data)
  end subroutine entryPoint  

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

