program nine_two
    use iso_fortran_env, only: int64
    implicit none
    integer(int64) :: ios, i, j, n, m, result, char, cid, needle, count, write_val, block_size, space_size
    character(len=100000) :: line
    integer :: disk(10000000) = -1
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
    i = n

    do
        block_size = 1
        cid = disk(i)
        do
            i = i - 1
            if (i <= 0) exit
            if (cid == disk(i)) then 
                block_size = block_size + 1
            else
                i = i + 1
                exit
            end if
        end do

        space_size = 0
        do needle = 1, i
            if (disk(needle) == -1) then
                space_size = space_size + 1
            else
                space_size = 0
            end if
            if (space_size == block_size) then
                disk(needle-space_size+1:needle) = disk(i:i+block_size-1)
                disk(i:i+block_size-1) = -1
                exit
            end if
        end do

        do
            i = i - 1
            if (i <= 0) exit
            if (disk(i) /= -1) exit
        end do

        if (i <= 0) exit
    end do

    result = 0
    do i = 1, n
        if (disk(i) /= -1) result = result + (i-1)*disk(i)
    end do

    print *, result

end program nine_two