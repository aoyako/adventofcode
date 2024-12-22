program twentyone_one
    implicit none
    integer :: ios, ncode, x, y, i, result, tmp
    character(256) :: line

    result = 0
    do
        read(*, "(A)", iostat=ios) line
        if (ios /= 0) exit
        x = 0
    
        tmp = res(line(1:1), 1, "A", 3)
        do i = 2, len_trim(line)
            tmp = tmp + res(line(i:i), 1, line(i-1:i-1), 3)
            y = 0
            read(line(i-1:i-1), *) y
            x = x*10 + y
        end do
        result = result + tmp * x
    end do   

    print *, result

contains
recursive function res(end, press, begin, depth) result(output)
    character(1), intent(in) :: end, begin
    integer, intent(in) :: press, depth
    integer :: output
    integer :: cooords_dir(10, 10), empty_x, empty_y, i, j, legs(120, 120, 120), nx, nlayers
    integer :: bx, by, ex, ey, dx, dy, scale, yscale, length, xscale
    character(1) :: end_dir, xdir, ydir

    call set_coords(end, ex, ey)
    call set_coords(begin, bx, by)

    if (depth == 0) then
        output = press
        return
    end if

    dx = bx - ex
    dy = by - ey
    if (dx < 0) then
        xdir = ">"
        xscale = -dx
    else
        xdir = "<"
        xscale = dx
    end if
    if (dy < 0) then
        ydir = "v"
        yscale = -dy
    else
        ydir = "^"
        yscale = dy
    end if
    end_dir = "A"

    length = 0
    if (dx == 0) then
        length = length + res(ydir, yscale, "A", depth-1)
        length = length + res("A", press, ydir, depth-1)
    elseif (dy == 0) then
        length = length + res(xdir, xscale, "A", depth-1)
        length = length + res("A", press, xdir, depth-1)
    elseif (by == 0 .and. ex == 0) then
        length = length + res(ydir, yscale, "A", depth-1)
        length = length + res(xdir, xscale, ydir, depth-1)
        length = length + res("A", press, xdir, depth-1)
    elseif ((bx == 0 .and. ey == 0) .or. dx > 0) then
        length = length + res(xdir, xscale, "A", depth-1)
        length = length + res(ydir, yscale, xdir, depth-1)
        length = length + res("A", press, ydir, depth-1)
    else
        length = length + res(ydir, yscale, "A", depth-1)
        length = length + res(xdir, xscale, ydir, depth-1)
        length = length + res("A", press, xdir, depth-1)
    end if
    
    output = length

end function res

subroutine set_coords(dir, x, y)
    integer, intent(inout) :: x, y
    character(1), intent(in) :: dir
    integer :: i
    select case (dir)
        case("^")
            x = 1
            y = 0
        case("A")
            x = 2
            y = 0
        case("<")
            x = 0
            y = 1
        case("v")
            x = 1
            y = 1
        case(">")
            x = 2
            y = 1

        case("0")
            x = 1
            y = 0
        case("1")
            x = 0
            y = -1
        case("2")
            x = 1
            y = -1
        case("3")
            x = 2 
            y = -1
        case("4")
            x = 0 
            y = -2
        case("5")
            x = 1
            y = -2
        case("6")
            x = 2
            y = -2
        case("7")
            x = 0
            y = -3
        case("8")
            x = 1
            y = -3
        case("9")
            x = 2
            y = -3
    end select
end subroutine set_coords

subroutine append_moves(cache, n, move, nmoves)
    integer, intent(inout) :: cache(:), n
    integer, intent(in) :: move, nmoves
    do i = 1, nmoves
        n = n + 1
        cache(n) = move
    end do
end subroutine append_moves

end program twentyone_one
