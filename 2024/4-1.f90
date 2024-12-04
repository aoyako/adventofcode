program four_one
    implicit none
    integer :: ios, i, j, n, m, result
    character(len=256) :: line
    character(len=1) :: map(100000, 256)

    i = 1
    n = 0
    m = 0
    do
        read(*, "(A)", iostat=ios) line
        if (ios /= 0) exit

        do j = 1, len_trim(line)
            map(i, j) = line(j:j)
        end do

        n = n + 1
        m = len(trim(line))
        i = i + 1
    end do

    result = 0

    do i = 1, n
        do j = 1, m
            result = result + is_xmas(map, i, j, n, m)
        end do
    end do

    print *,  result

contains
  integer function is_xmas(arr, i, j, n, m)
    character(1), intent(in) :: arr(:,:)
    integer, intent(in) :: i, j, n, m
    integer :: d, result

    result = 0

    if (i+3 <= n) then
        if (all([arr(i,j), arr(i+1,j), arr(i+2,j), arr(i+3,j)] == ['X', 'M', 'A', 'S'])) result = result + 1
    end if

    if (i-3 >= 1) then
        if (all([arr(i,j), arr(i-1,j), arr(i-2,j), arr(i-3,j)] == ['X', 'M', 'A', 'S'])) result = result + 1
    end if

    if (j+3 <= m) then
        if (all([arr(i,j), arr(i,j+1), arr(i,j+2), arr(i,j+3)] == ['X', 'M', 'A', 'S'])) result = result + 1
    end if

    if (j-3 >= 1) then
        if (all([arr(i,j), arr(i,j-1), arr(i,j-2), arr(i,j-3)] == ['X', 'M', 'A', 'S'])) result = result + 1
    end if
    
    if ((j+3 <= m) .and. (i+3 <= n)) then
        if (all([arr(i,j), arr(i+1,j+1), arr(i+2,j+2), arr(i+3,j+3)] == ['X', 'M', 'A', 'S'])) result = result + 1
    end if

    if ((j-3 >= 1) .and. (i-3 >= 1)) then
        if (all([arr(i,j), arr(i-1,j-1), arr(i-2,j-2), arr(i-3,j-3)] == ['X', 'M', 'A', 'S'])) result = result + 1
    end if 

    if ((j+3 <= m) .and. (i-3 >= 1)) then
        if (all([arr(i,j), arr(i-1,j+1), arr(i-2,j+2), arr(i-3,j+3)] == ['X', 'M', 'A', 'S'])) result = result + 1
    end if

    if ((j-3 >= 1) .and. (i+3 <= n)) then
        if (all([arr(i,j), arr(i+1,j-1), arr(i+2,j-2), arr(i+3,j-3)] == ['X', 'M', 'A', 'S'])) result = result + 1
    end if
    
    is_xmas = result

  end function is_xmas

end program four_one
