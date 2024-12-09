program nine_one
    use iso_fortran_env, only: int64
    implicit none
    integer(int64) :: ios, i, j, n, m, result, char, cid, needle, count, write_val
    character(len=100000) :: line
    integer :: disk(100000000) = -1
    logical :: order

    read(*, "(A)", iostat=ios) line
    cid = 0
    needle = 1
    order = .true.

    do i = 1, len_trim(line)
        count = ichar(line(i:i)) - ichar("0")
        write_val = -1

        if (order) then
            write_val = cid
            cid = cid + 1
        else
            write_val = -1
        end if

        do j = 1, count
            disk(needle) = write_val
            needle = needle + 1
        end do

        if (needle > 100000000) print *, "out of bounds"

        order = .not. order
    end do

    n = needle-1
    needle = 1

    do i = n, 1, -1
        do
            if (disk(needle) == -1) exit
            needle = needle + 1
        end do

        if (needle >= i) exit

        disk(needle) = disk(i)
        disk(i) = -1
    end do

    result = 0
    do i = 1, n
        if (disk(i) /= -1) result = result + (i-1)*disk(i)
    end do

    print *, result

end program nine_one