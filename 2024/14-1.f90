program forteen_one
    implicit none
    character(len=256) :: errmsg, line, command
    integer :: ax, ay, bx, by, cx, cy, x, y, i, j, A, B, result, ecode, ios, N, M, T
    integer:: tl, tr, bl, br, vx, vy

    command = "grep -oE '\-?[0-9]+' > tmp.txt"
    call execute_command_line(command, exitstat=ecode)

    open(unit=10, file="tmp.txt", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        stop
    end if

    N = 101
    M = 103
    ! N = 11
    ! M = 7
    T = 100

    tl = 0
    tr = 0
    br = 0
    bl = 0
    result = 0
    do
        read(10, *, iostat=ios) x, y, vx, vy
        if (ios /= 0) exit

        x = x + vx * T
        y = y + vy * T

        x = mod(mod(x, N)+N, N)
        y = mod(mod(y, M)+M, M)

        if (x > N/2 .and. y > M/2) tl = tl + 1
        if (x < N/2 .and. y > M/2) tr = tr + 1
        if (x > N/2 .and. y < M/2) bl = bl + 1
        if (x < N/2 .and. y < M/2) br = br + 1

    end do

    result = tl * tr * br * bl
    print *, result
end program forteen_one
