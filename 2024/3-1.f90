program three_one
    implicit none
    integer :: ecode, ios, left, right, result
    character(len=256) :: errmsg, line, command

    command = "grep -oE 'mul\([0-9]{1,3},[0-9]{1,3}\)' | sed -E 's/mul\(([0-9]{1,3}),([0-9]{1,3})\)/\1 \2/' > tmp.txt"
    call execute_command_line(command, exitstat=ecode)

    open(unit=10, file="tmp.txt", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        stop
    end if

    result = 0
    do
        read(10, "(A)", iostat=ios) line
        if (ios /= 0) exit

        read(line, *, iostat=ios) left, right
        
        result = result + left * right
    end do

    print *, result

end program three_one
