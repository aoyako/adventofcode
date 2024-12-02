module task
  implicit none
contains
  logical function check(arr)
    integer, dimension(:), allocatable, intent(inout) :: arr
    integer, allocatable :: temp(:)
    logical :: res, is_increasing = .false., is_decreasing = .false.
    integer :: i, count_increasing, count_decreasing, count_broken, broken, new_size

    broken = -1
    count_increasing = 0
    count_decreasing = 0
    count_broken = 0
    is_increasing = .false.
    is_decreasing = .false.

    check = .true.

    do i = 2, size(arr)
        if (arr(i-1) < arr(i)) then
            count_increasing = count_increasing + 1
        elseif (arr(i-1) > arr(i)) then
            count_decreasing = count_decreasing + 1
        else
            count_broken = count_broken + 1
        end if
    end do

    if (min(count_increasing, count_decreasing)+count_broken > 1) then
        check = .false.
        return
    end if

    if (max(count_increasing, count_decreasing) == count_increasing) is_increasing = .true.
    if (max(count_increasing, count_decreasing) == count_decreasing) is_decreasing = .true.

    do i = 2, size(arr)
        if ((arr(i-1) > arr(i) .and. is_increasing) .or. &
            (arr(i-1) < arr(i) .and. is_decreasing) .or. &
            (arr(i-1) == arr(i)) .or. &
            abs(arr(i-1) - arr(i)) > 3) then
            broken = i
            exit
        end if
    end do

    if (broken /= -1) then
        allocate(temp(size(arr)-1))
        temp = [arr(:broken-1), arr(broken+1:)]
    else
        allocate(temp(size(arr)))
        temp = arr(:)
    end if

    do i = 2, size(temp)
        if ((temp(i-1) > temp(i) .and. is_increasing) .or. &
            (temp(i-1) < temp(i).and. is_decreasing) .or. &
            (temp(i-1) == temp(i)) .or. &
            abs(temp(i-1) - temp(i)) > 3) then
            check = .false.
            ! return
        end if
    end do

    if (check) return
    check = .true.

    if (broken /= -1) then
        temp = [arr(:broken-2), arr(broken:)]
    end if

    do i = 2, size(temp)
        if ((temp(i-1) > temp(i) .and. is_increasing) .or. &
            (temp(i-1) < temp(i).and. is_decreasing) .or. &
            (temp(i-1) == temp(i)) .or. &
            abs(temp(i-1) - temp(i)) > 3) then
            check = .false.
            return
        end if
    end do

  end function check
end module task

program two_two
    use util
    use task
    implicit none
    integer, allocatable:: arr(:)
    integer :: count

    count = 0
    do
        arr = read_int_list()
        if (size(arr) == 0) exit
        if (check(arr)) count = count + 1
    end do

    print *, count

end program two_two
