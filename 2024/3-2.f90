program three_two
    implicit none
    integer :: ecode, ios, left, right, result
    character(len=256) :: errmsg, line, command
    logical :: enabled

    command = "grep -oE 'mul\([0-9]{1,3},[0-9]{1,3}\)|do\(\)|don'\''t\(\)'" // &
              " | sed -E 's/do\(\)/-1 -1/'" // &
              " | sed -E 's/don'\''t\(\)/-2 -2/'" // &
              " | sed -E 's/mul\(([0-9]{1,3}),([0-9]{1,3})\)/\1 \2/'" // &
              " > tmp.txt"
    call execute_command_line(command, exitstat=ecode)

    open(unit=10, file="tmp.txt", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        stop
    end if

    result = 0
    enabled = .true.
    do
        read(10, "(A)", iostat=ios) line
        if (ios /= 0) exit

        read(line, *, iostat=ios) left, right
        
        if (left == -1) then
            enabled = .true.
        elseif (left == -2) then
            enabled = .false.
        elseif (enabled) then
            result = result + left * right
        end if
    end do

    print *, result

end program three_two
