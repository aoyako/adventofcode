program thirteen_one
    implicit none
    character(len=256) :: errmsg, line, command
    integer :: ax, ay, bx, by, cx, cy, x, y, i, j, A, B, result, ecode, ios

    command = "grep -o '[0-9]\+' > tmp.txt"
    call execute_command_line(command, exitstat=ecode)

    open(unit=10, file="tmp.txt", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        stop
    end if

    result = 0
    do
        A = 0
        B = 0
        read(10, *, iostat=ios) ax, ay, bx, by, x, y
        if (ios /= 0) exit

        A = (x*by - y*bx) / (ax * by - ay * bx)
        if (mod((x*by - y*bx), (ax * by - ay * bx)) == 0) then
            B = (y - A*ay) / by

            if (mod((y - A*ay), by) == 0) result = result + 3*A + B
        end if
    end do

    print *, result
end program thirteen_one
