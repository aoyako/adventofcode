program twentyfour_one
    implicit none
    integer, parameter :: uint128 = selected_int_kind(38)
    character(len=256) :: command
    character(len=256) :: line
    integer :: ecode, ios, n, ncache, i
    integer(uint128) :: x, y, z, op, cache(10000, 4), result
    integer :: map(100000000)
    logical :: updated

    command = "sed -e 's/^$/\\/g' | grep -oE -e '[a-zA-Z0-9]+' -e '\\'  > tmp.txt"
    call execute_command_line(command, exitstat=ecode)

    open(unit=10, file="tmp.txt", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        stop
    end if

    n = 0
    ncache = 0
    map = -1
    do
        read(10, *, iostat=ios) line
        if (ios /= 0) exit
        if (line(1:1) == "\") exit

        x = ichar(line(1:1)) * 256*256 + ichar(line(2:2)) * 256 + ichar(line(3:3))
        read(10, *) y

        map(x) = y
    end do

    do
        read(10, *, iostat=ios) line
        if (ios /= 0) exit

        x = ichar(line(1:1)) * 256*256 + ichar(line(2:2)) * 256 + ichar(line(3:3))

        read(10, *, iostat=ios) line
        if (line(1:1) == "A") op = 2
        if (line(1:1) == "X") op = 1
        if (line(1:1) == "O") op = 0

        read(10, *, iostat=ios) line
        y = ichar(line(1:1)) * 256*256 + ichar(line(2:2)) * 256 + ichar(line(3:3))

        read(10, *, iostat=ios) line
        z = ichar(line(1:1)) * 256*256 + ichar(line(2:2)) * 256 + ichar(line(3:3))

        ncache = ncache + 1
        cache(ncache, 1) = x
        cache(ncache, 2) = y
        cache(ncache, 3) = z
        cache(ncache, 4) = op
    end do

    do
        updated = .false.
        do i = 1, ncache
            x = cache(i, 1)
            y = cache(i, 2)
            z = cache(i, 3)
            op = cache(i, 4)

            if (map(x) /= -1 .and. map(y) /= -1 .and. map(z) == -1) then
                if (op == 2) then
                    map(z) = and(map(x), map(y))
                elseif (op == 1) then
                    map(z) = xor(map(x), map(y))
                else
                    map(z) = or(map(x), map(y))
                end if
                updated = .true.
            end if
        end do

        if (.not. updated) exit
    end do
    
    result = 0
    do i = 99, 0, -1
        x = ichar("z") * 256*256 + (i/10 + ichar("0")) * 256 + (mod(i,10) + ichar("0"))
        if (map(x) /= -1) result = result * 2 + map(x)
    end do

    print *, result

end program twentyfour_one
