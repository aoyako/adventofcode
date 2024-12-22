program twentytwo_one
    implicit none
    integer, parameter :: int128 = selected_int_kind(38)
    integer :: ios
    integer(int128) :: result, x, y, i

    result = 0
    do
        read(*, *, iostat=ios) x
        if (ios /= 0) exit

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

        end do
        result = result + x
    end do

    print *, result

end program twentytwo_one
