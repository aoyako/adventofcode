program seventeen_one
    implicit none
    character(len=256) :: command
    integer :: ecode, ios
    integer :: apk(100)
    character(len=1000) :: output
    character(len=20) :: buffer
    integer:: A, B, C, opcode, x, left, right, ipointer, n

    command = "grep -oE '\-?[0-9]+' > tmp.txt"
    call execute_command_line(command, exitstat=ecode)

    open(unit=10, file="tmp.txt", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        stop
    end if


    n = 0
    output = ""
    read(10, *) A, B, C

    do
        read(10, *, iostat=ios) opcode, x
        if (ios /= 0) exit

        n = n + 1
        apk(n) = opcode
        n = n + 1
        apk(n) = x
    end do

    ipointer = 1
    do
        if (ipointer > n) exit
        opcode = apk(ipointer)
        x = apk(ipointer+1)

        select case (opcode)
            case(0)
                left = A
                right = combo(x, A, B, C)
                A = left / (2**right)
            case(1)
                left = B
                right = x
                B = xor(left, right)
            case(2)
                x = combo(x, A, B, C)
                B = mod(x, 8)
            case(3)
                if (A /= 0) then
                    ipointer = x+1 - 2
                end if
            case(4)
                B = xor(B, C)
            case(5)
                x = combo(x, A, B, C)
                x = mod(x, 8)
                write(buffer, "(I0)") x
                if (len_trim(output) /= 0) then
                    output = trim(output) // "," // trim(buffer)
                else
                    output = trim(buffer)
                end if
            case(6)
                left = A
                right = combo(x, A, B, C)
                B = left / (2**right)
            case(7)
                left = A
                right = combo(x, A, B, C)
                C = left / (2**right)
        end select

        ipointer = ipointer + 2
    end do

    print *, trim(output)

contains

function combo(opt, A, B, C) result(output)
    integer, intent(in) :: opt, A, B, C
    integer :: output

    select case (opt)
        case(0)
            output = 0
        case(1)
            output = 1 
        case(2)
            output = 2
        case(3)
            output = 3
        case(4)
            output = A
        case(5)
            output = B
        case(6)
            output = C
        case(7)
            print *, "IMPOSSIBLE COMBO OPERAND"
    end select
    
end function combo
end program seventeen_one
