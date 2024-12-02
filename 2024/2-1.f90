module task
  implicit none
contains
  logical function check(arr)
    integer, dimension(:), intent(in) :: arr
    logical :: res, is_increasing, is_decreasing
    integer :: i

    check = .true.

    if (arr(1) > arr(2)) then
        is_increasing = .true.
        is_decreasing = .false.
    elseif ( arr(1) < arr(2) ) then
        is_increasing = .false.
        is_decreasing = .true.
    else
        check = .false.
        return
    end if

    do i = 2, size(arr)
        if ((arr(i-1) > arr(i) .and. is_decreasing) .or. &
            (arr(i-1) < arr(i).and. is_increasing) .or. &
            (arr(i-1) == arr(i)) .or. &
            abs(arr(i-1) - arr(i)) > 3) then
            check = .false.
            return
        end if
    end do

  end function check
end module task

program two_one
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

end program two_one
