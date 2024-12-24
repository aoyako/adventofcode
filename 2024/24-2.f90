program twentyfour_two
    implicit none
    integer, parameter :: uint128 = selected_int_kind(38)
    character(len=256) :: command
    character(len=256) :: line
    integer :: ecode, ios, n, ncache, i, a1, a2, b1, b2, c1, c2, d1, d2
    integer(uint128) :: x, y, z, op, old_cache(10000, 4), cache(10000, 4), result, X_wired, Y_wired, Z_wired
    integer :: map(100000000), memory(100000000), broken(4), c, tmp1, tmp2, bit
    logical :: updated, all_unique

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

    X_wired = from_bits("x", map)
    Y_wired = from_bits("y", map)

    c = 0
    do i = 1, ncache
        if (cache(i,4) /= 1 .and. cache(i, 3) >= ichar("z")*256*256) then
            c = c + 1
            broken(c) = i
        end if
    end do

    ! Not a huge soluton
    ! Build dependency graph
    ! Since the result is an addition
    ! (x xor y) xor remainder = z

    a1 = 163 - 91
    a2 = 279 - 91
    b1 = 164 - 91
    b2 = 208 - 91
    c1 = 197 - 91
    c2 = 234 - 91
    d1 = 212 - 91
    d2 = 312 - 91
    call swap(cache(163 - 91, 3), cache(279 - 91, 3))
    call swap(cache(164 - 91, 3), cache(208 - 91, 3))
    call swap(cache(197 - 91, 3), cache(234 - 91, 3))
    call swap(cache(212 - 91, 3), cache(312 - 91, 3))

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

    Z_wired = from_bits("z", map)

    if (X_wired + Y_wired == Z_wired) then
        print *, to_str(cache(a1,3))
        print *, to_str(cache(a2,3))
        print *, to_str(cache(b1,3))
        print *, to_str(cache(b2,3))
        print *, to_str(cache(c1,3))
        print *, to_str(cache(c2,3))
        print *, to_str(cache(d1,3))
        print *, to_str(cache(d2,3))
        stop
    end if


contains

function from_bits(char, memory) result(output)
    character(1), intent(in) :: char
    integer, intent(in) :: memory(:)
    integer(uint128) :: output
    
    output = 0
    do i = 99, 0, -1
        x = ichar(char) * 256*256 + (i/10 + ichar("0")) * 256 + (mod(i,10) + ichar("0"))
        if (memory(x) /= -1) output = output * 2 + memory(x)
    end do
    
end function from_bits

subroutine swap(a, b)
    integer(uint128), intent(inout) :: a, b
    integer :: temp

    temp = a
    a = b
    b = temp
end subroutine swap

function to_str(x) result(output)
    integer(uint128), intent(in) :: x
    character(4) :: output
    integer(uint128) :: a, b, c

    a = x / (256*256)
    b = mod(x, 256*256) / 256
    c = mod(x, 256)
    
    output(1:1) = char(a)
    output(2:2) = char(b)
    output(3:3) = char(c)
    
end function to_str

end program twentyfour_two
