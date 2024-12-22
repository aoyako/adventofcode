program twentytwo_two
    implicit none
    integer, parameter :: int128 = selected_int_kind(38)
    integer :: ios
    integer(int128) :: result, x, y, i, prev, tmp
    integer :: map(2000, 2000), n, a, b, c, d
    integer :: prices(2000, 2000)

    n = 0
    prev = -1
    do
        read(*, *, iostat=ios) x
        if (ios /= 0) exit
        n = n + 1
        prev = x

        do i = 1, 2000
            y = x * 64
            x = xor(x, y)
            x = mod(x, 16777216)

            y = x / 32
            x = xor(x, y)
            x = mod(x, 16777216)

            y = x * 2048
            x = xor(x, y)
            x = mod(x, 16777216)

            tmp = mod(x, 10)
            map(n, i) = tmp - prev
            prices(n, i) = tmp
            prev = tmp
        end do
    end do

    result = 0
    do a = -9, 9
        do b = -9, 9
            do c = -9, 9
                do d = -9, 9
                    result = max(result, check(map, prices, n, a, b, c, d))
                end do
            end do
        end do
        print *,  result
    end do

    print *, result

contains
pure function contains(row, prices, a, b, c, d) result(output)
    integer, intent(in) :: row(:), a, b, c, d, prices(:)
    integer :: output
    integer :: i
    
    output = 0
    do i = 4, 2000
        if (row(i-3) == a .and. row(i-2) == b .and. row(i-1) == c .and. row(i) == d) then
            output = prices(i)
            return
        end if
    end do
end function contains

pure function check(map, prices, n, a, b, c, d) result(output)
    integer, intent(in) :: map(:,:), a, b, c, d, n, prices(:,:)
    integer :: output
    integer :: i
    
    output = 0
    do i = 1, n
        output = output + contains(map(i,:), prices(i,:), a, b, c, d)
    end do
end function check
end program twentytwo_two
