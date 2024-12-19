program nineteen_one
    implicit none
    integer, parameter :: uint128 = selected_int_kind(38)
    character(len=256) :: command
    integer :: ecode, ios, n, result, split, i
    character(len=256) :: line
    character(len=1) :: line_arr(300)
    integer(uint128) :: towels(1000, 2), x, y
    integer :: cache(100000000)

    command = "sed -e 's/^$/\\/g' | grep -oE -e '[a-z]+' -e '\\'  > tmp.txt"
    call execute_command_line(command, exitstat=ecode)

    open(unit=10, file="tmp.txt", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        stop
    end if

    n = 0
    cache = 0
    do
        read(10, *, iostat=ios) line
        if (ios /= 0) exit
        if (line(1:1) == "\") exit

        do i = 1, len_trim(line)
            line_arr(i) = line(i:i)
        end do

        n = n + 1
        towels(n, 1) = parse(line_arr, len_trim(line))
        towels(n, 2) = 6_uint128**len_trim(line)
    end do

    result = 0
    do
        read(10, *, iostat=ios) line
        if (ios /= 0) exit

        do i = 1, len_trim(line)
            line_arr(i) = line(i:i)
        end do

        if (len_trim(line) > 30) then
            do split = len_trim(line)/2-5, len_trim(line)/2+5
                x = parse(line_arr(:split), split)
                y = parse(line_arr(split+1:), len_trim(line) - split)
                if (dfs(towels, n, x, cache) .and. dfs(towels, n, y, cache)) then 
                    result = result + 1
                    exit
                end if 
            end do
        else
            x = parse(line_arr, len_trim(line))
            if (dfs(towels, n, x, cache)) then 
                result = result + 1
            end if
        end if
    end do

    print *, result

contains

recursive function dfs(path, n, begin, cache) result(output)
    integer(uint128), intent(in) :: path(:,:), begin
    integer, intent(inout) :: cache(:)
    integer, intent(in) :: n
    integer :: i
    logical :: output

    output = .false.

    if (begin == 0) then
        output = .true.
        return
    end if

    if (begin < 100000000 .and. cache(begin) == -1) then
        output = .false.
        return
    end if

    if (begin < 100000000 .and. cache(begin) == 1) then
        output = .true.
        return
    end if

    do i = 1, n
        if (mod(begin, path(i,2)) == path(i,1)) output = output .or. dfs(path, n, begin/path(i,2), cache)
        if (output) then
            if (begin < 100000000) then
                cache(begin) = 1
            end if
            return
        end if
    end do

    if (begin < 100000000) then
        cache(begin) = -1
    end if
    
end function dfs

function parse(line, n) result(output)
    character(len=1), intent(in) :: line(:)
    integer, intent(in) :: n
    integer(uint128) :: output, i

    output = 0
    do i = 1, n
        select case (line(i))
            case("w")
                output = output*6 + 1
            case("u")
                output = output*6 + 2
            case("b")
                output = output*6 + 3
            case("r")
                output = output*6 + 4
            case("g")
                output = output*6 + 5
        end select
    end do
    
end function parse
end program nineteen_one
