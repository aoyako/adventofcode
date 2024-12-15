program fifteen_two
    implicit none
    character(len=10000) :: line
    character(1) :: map(1000, 1000)
    integer :: i, j, n, m, x, y, result, ios, ii, jj

    i = 1
    n = 0
    x = 0
    y = 0
    do
        read(*, "(A)", iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) exit

        do j = 1, len_trim(line)
            if (line(j:j) == "O") then
                map(i, j*2) = "]"
                map(i, j*2-1) = "["
            end if
            if (line(j:j) == "@") then
                map(i, j*2) = "."
                map(i, j*2-1) = "@"
                x = i
                y = 2*j - 1
            end if
            if (line(j:j) == ".") then
                map(i, j*2) = "."
                map(i, j*2-1) = "."
            end if
            if (line(j:j) == "#") then
                map(i, j*2) = "#"
                map(i, j*2-1) = "#"
            end if
        end do

        n = n + 1
        m = 2*len_trim(line)
        i = i + 1
    end do

    do
        read(*, "(A)", iostat=ios) line
        if (ios /= 0) exit

        do j = 1, len_trim(line)
            call simulate(map, n, m, x, y, line(j:j))
        end do
    end do

    do i = 1, n
        do j = 1, m
            if (map(i, j) == "[") result = result + 100*(i-1) + (j-1)
        end do
    end do

    ! do ii = 1, n
    !     do jj = 1, m
    !         write(*, "(A)", advance="no") map(ii,jj)
    !     end do
    !     print *
    ! end do

    print *, result

contains
  subroutine simulate(map, n, m, x, y, dir)
    character(1), intent(inout) :: map(:,:)
    integer, intent(in) :: n, m
    integer, intent(inout) :: x, y
    integer :: dx, dy, i, k
    character(1) :: dir

    dx = 0
    dy = 0
    select case (dir)
        case("^")
            dx = -1
        case(">")
            dy = 1
        case("v")
            dx = 1
        case("<")
            dy = -1
    end select

    if (dy /= 0) then
        do k = 1, 100
            if (map(x, y+k*dy) == "#") exit
            if (map(x, y+k*dy) == ".") then
                do i = y+k*dy, y+dy, -dy
                    map(x, i) = map(x, i-dy)
                end do
                map(x, y) = "."
                y = y + dy
                exit
            end if
        end do
    elseif (can_move(map, n, m, x, y, dx)) then
        call move(map, n, m, x, y, dx)
        x = x + dx
    end if

    end subroutine simulate

    recursive logical function can_move(map, n, m, x, y, dx) result(res)
        character(1), intent(in) :: map(:,:)
        integer, intent(in) :: n, m, x, y, dx

        if (map(x, y) == ".") then
            res = .true.
            return
        end if
        if (map(x, y) == "#") then
            res = .false.
            return
        end if
        if (map(x, y) == "]") then
            res = can_move(map, n, m, x+dx, y, dx) .and. can_move(map, n, m, x+dx, y-1, dx)
            return
        end if
        if (map(x, y) == "[") then
            res = can_move(map, n, m, x+dx, y, dx) .and. can_move(map, n, m, x+dx, y+1, dx)
            return
        end if
        res = can_move(map, n, m, x+dx, y, dx)
        
    end function can_move

    recursive subroutine move(map, n, m, x, y, dx)
        character(1), intent(inout) :: map(:,:)
        integer, intent(in) :: n, m, x, y, dx
        
        if (map(x, y) == "#") return 
        if (map(x, y) == ".") return 
        if (map(x, y) == "]") then
            call move(map, n, m, x+dx, y, dx)
            call move(map, n, m, x+dx, y-1, dx)
            map(x+dx, y) = map(x, y)
            map(x+dx, y-1) = map(x, y-1)
            map(x, y) = "."
            map(x, y-1) = "."
        end if
        if (map(x, y) == "[") then
            call move(map, n, m, x+dx, y+1, dx)
            call move(map, n, m, x+dx, y, dx)
            map(x+dx, y) = map(x, y)
            map(x+dx, y+1) = map(x,y+1)
            map(x, y) = "."
            map(x, y+1) = "."
        end if
        if (map(x, y) == "@") then
            call move(map, n, m, x+dx, y, dx)
            map(x+dx, y) = map(x, y)
            map(x, y) = "."
        end if
        
    end subroutine move
end program fifteen_two
