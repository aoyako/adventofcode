program four_two
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

    if ((j+1 <= m) .and. (i+1 <= n) .and. (j-1 >= 1) .and. (i-1 >= 1)) then
        if (all([arr(i-1,j-1), arr(i, j), arr(i+1,j+1)] == ['M', 'A', 'S']) .or. &
        all([arr(i-1,j-1), arr(i, j), arr(i+1,j+1)] == ['S', 'A', 'M'])) then
            result = result + 1
        end if
    end if

    if ((j+1 <= m) .and. (i+1 <= n) .and. (j-1 >= 1) .and. (i-1 >= 1)) then
        if (all([arr(i+1,j-1), arr(i, j), arr(i-1,j+1)] == ['M', 'A', 'S']) .or. &
        all([arr(i+1,j-1), arr(i, j), arr(i-1,j+1)] == ['S', 'A', 'M'])) then
            result = result + 1
        end if
    end if
    
    is_xmas = result / 2

  end function is_xmas

end program four_two
