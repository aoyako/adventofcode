program seventeen_two
    implicit none
    integer, parameter :: int128 = selected_int_kind(38)
    character(len=256) :: command
    integer :: ecode, ios
    integer(kind=int128) :: apk(100)
    integer(kind=int128) :: output, target, opcode, x, i, n

    command = "grep -oE '\-?[0-9]+' > tmp.txt"
    call execute_command_line(command, exitstat=ecode)

    open(unit=10, file="tmp.txt", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        stop
    end if


    n = 0
    target = 0
    output = 0
    read(10, *) x, x, x

    do
        read(10, *, iostat=ios) opcode, x
        if (ios /= 0) exit

        n = n + 1
        apk(n) = opcode
        n = n + 1
        apk(n) = x
    end do

    do i = 1, n
        target = target * 10 + apk(i)
    end do

    x = reverse(apk, target, 0_int128, n)
    print *, x

contains

recursive function reverse(apk, target, res, n) result(ans)
    integer(kind=int128), intent(in) :: apk(:)
    integer(kind=int128), intent(in) :: res, target, n
    integer(kind=int128) :: ans, output, rem, ipointer, A, B, C, right, left, opcode, x

    ans = -1
    if (target == 0) then
        ans = res
        return
    end if

    do rem = 0, 7
        A = lshift(res, 3) + rem
        B = 0
        C = 0
        output = -1

        do ipointer = 1, n-3, 2
            opcode = apk(ipointer)
            x = apk(ipointer+1)

            select case (opcode)
                case(1)
                    left = B
                    right = x
                    B = xor(left, right)
                case(2)
                    x = combo(x, A, B, C)
                    B = and(x, 7_int128)
                case(4)
                    B = xor(B, C)
                case(5)
                    x = combo(x, A, B, C)
                    x = and(x, 7_int128)
                    output = x
                case(6)
                    left = A
                    right = combo(x, A, B, C)
                    B = rshift(left, right)
                case(7)
                    left = A
                    right = combo(x, A, B, C)
                    C = rshift(left, right)
            end select

            if (output == mod(target, 10_int128)) then
                ans = reverse(apk, target/10_int128, A, n)
                if (ans /= -1) return
            end if
        end do
    end do
end function reverse

pure function combo(opt, A, B, C) result(output)
    integer(kind=int128), intent(in) :: opt, A, B, C
    integer(kind=int128) :: output

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
    end select
    
end function combo
end program seventeen_two
