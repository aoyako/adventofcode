program one_two
    implicit none
    integer, parameter :: max_size = 100000
    integer :: arr(max_size), freq(max_size)
    integer :: i, n, ios, res, tmp

    n = 0

    do
        read(*, *, iostat=ios) arr(n + 1), tmp
        if (ios /= 0) exit
        freq(tmp) = freq(tmp) + 1 
        n = n + 1
    end do


    res = 0
    do i = 1, n
        res = res + arr(i) * freq(arr(i))
    end do

    print *, res

end program one_two
