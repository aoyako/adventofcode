program nineteen_two
    implicit none
    integer, parameter :: uint128 = selected_int_kind(38)
    character(len=256) :: command
    integer :: ecode, ios, n, split, i
    character(len=256) :: line
    character(len=1) :: line_arr(300)
    character(len=1) :: towels(1000, 400)
    integer :: lels(1000)
    integer(uint128) :: result

    command = "sed -e 's/^$/\\/g' | grep -oE -e '[a-z]+' -e '\\'  > tmp.txt"
    call execute_command_line(command, exitstat=ecode)

    open(unit=10, file="tmp.txt", status="old", action="read", iostat=ios)
    if (ios /= 0) then
        stop
    end if

    n = 0
    do
        read(10, *, iostat=ios) line
        if (ios /= 0) exit
        if (line(1:1) == "\") exit

        n = n + 1
        lels(n) = len_trim(line)
        do i = 1, len_trim(line)
            towels(n, i) = line(i:i)
        end do
    end do

    result = 0
    do
        read(10, *, iostat=ios) line
        if (ios /= 0) exit

        do i = 1, len_trim(line)
            line_arr(i) = line(i:i)
        end do

        result = result + dfs(towels, n, line_arr, len_trim(line), lels)
    end do

    print *, result

contains

function dfs(path, n, arr, narr, lels) result(output)
    character(len=1), intent(in) :: path(:,:)
    character(len=1), intent(in) :: arr(:)
    integer, intent(in) :: n, narr, lels(:)
    integer :: i, mm, p
    integer(uint128) :: output
    integer(uint128) :: cache(1000)

    cache = 0
    cache(1) = 1
    do i = 1, narr
        do p = 1, n
            if (i >= lels(p)) then
                mm = lels(p)
                if (all(arr(i-mm+1:i) == path(p,:mm))) then
                    cache(i+1) = cache(i+1-mm) + cache(i+1)
                end if
            end if
        end do
    end do

    output = cache(narr+1)
    
end function dfs
    
end program nineteen_two
