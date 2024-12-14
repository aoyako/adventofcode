program forteen_two
    implicit none
    character(len=256) :: errmsg, line, command
    integer :: ax, ay, bx, by, cx, cy, x, y, i, j, A, B, result, ecode, ios, N, M, T
    character(1) :: map(300, 300)
    integer:: tl, tr, bl, br, vx, vy, ncache, p
    integer :: cache(10000, 4)

    ncache = 0
    command = "grep -oE '\-?[0-9]+' > tmp.txt"
    call execute_command_line(command, exitstat=ecode)

    open(unit=10, file="tmp.txt", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        stop
    end if

    N = 101
    M = 103
    T = 3

    result = 0
    do
        read(10, *, iostat=ios) x, y, vx, vy
        if (ios /= 0) exit

        ncache = ncache + 1
        cache(ncache, 1) = x
        cache(ncache, 2) = y
        cache(ncache, 3) = vx
        cache(ncache, 4) = vy 
    end do

    do T = 0, 10000000
        tl = 0
        tr = 0
        br = 0
        bl = 0

        map = "."

        do p = 1, ncache
            x = cache(p, 1)
            y = cache(p, 2)
            vx = cache(p, 3)
            vy = cache(p, 4)

            x = x + vx * T
            y = y + vy * T
            x = mod(mod(x, N)+N, N)
            y = mod(mod(y, M)+M, M)

            map(x+1, y+1) = "#"
        end do

        print *, T
        do i = N/2-20, N/2+20
            do j = M/2-20, M/2+20
                write(*, "(A)", advance="no") map(i,j)
            end do
            print *
        end do
        print *
    end do
end program forteen_two
