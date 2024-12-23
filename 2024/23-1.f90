program twentythree_one
    implicit none
    integer, parameter :: int128 = selected_int_kind(38)
    integer :: ios, x, y, result, i, a, b
    character(256) :: line
    integer :: map(1000, 1000)
    integer :: visited(1000, 1000, 1000)

    map = 0
    visited = 0
    do
        read(*, "(A)", iostat=ios) line
        if (ios /= 0) exit

        x = (ichar(line(1:1)) - ichar("a"))*30 + (ichar(line(2:2)) - ichar("a")) + 1
        y = (ichar(line(4:4)) - ichar("a"))*30 + (ichar(line(5:5)) - ichar("a")) + 1

        map(x, y) = 1
        map(y, x) = 1
    end do

    result = 0
    do i = 1, 1000
        x = (i-1) / 30
        if (x == ichar("t")-ichar("a")) then
            do a = 1, 1000
                do b = 1, 1000
                    ! if (i == 571 .and. a == 75 .and. b == 95) print *, map(i,a), map(i,b), map(a,b)
                    if (map(i,a) == 1 .and. map(i,b) == 1 .and. map(a,b) == 1) then
                        if (visited(i, a, b) == 0) result = result + 1
                        visited(a, b, i) = 1
                        visited(b, a, i) = 1
                        visited(i, a, b) = 1
                        visited(i, b, a) = 1
                        visited(a, i, b) = 1
                        visited(b, i, a) = 1
                    end if
                end do
            end do
        end if
    end do

    print *, result

end program twentythree_one
