program sixteen_one
    use iso_fortran_env, only: int64
    implicit none
    character(len=10000) :: line
    character(1) :: map(300, 300)
    integer :: visited(300, 300)
    integer :: cache(100000000, 4)
    integer :: i, j, n, m, x, y, result, ios, ii, jj, ncache, ex, ey, bx, by, dx, dy, cost, dir
    integer(int64) :: step

    i = 1
    n = 0
    ncache = 0
    step = 0

    bx = 0
    by = 0
    do
        read(*, "(A)", iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) exit

        do j = 1, len_trim(line)
            map(i, j) = line(j:j)

            if (map(i, j) == "S") then
                bx = i
                by = j
            end if
            if (map(i, j) == "E") then
                ex = i
                ey = j
            end if
        end do

        n = n + 1
        m = len_trim(line)
        i = i + 1
    end do

    ncache = 1
    cache(1, 1) = bx
    cache(1, 2) = by
    cache(1, 3) = 0
    cache(1, 4) = 1

    result = 100000000
    visited = 100000000
    do
        if (step == 1000000000) exit
        if (ncache == 0) exit
        x = cache(ncache, 1)
        y = cache(ncache, 2)
        cost = cache(ncache, 3)
        dir = cache(ncache, 4)
        ncache = ncache - 1

        call cdir(dir, dx, dy)

        if (x == ex .and. y == ey) then
            result = min(result, cost)
            cycle
        end if

        if (map(x+dx, y+dy) /= "#" .and. visited(x+dx, y+dy) + 2000 > cost) then
            call gdir(dir, dx, dy)
            visited(x+dx, y+dy) = min(visited(x+dx, y+dy), cost+1)
            call apps(cache, x+dx, y+dy, cost+1, dir, ncache)
        end if

        call swap(dx, dy)
        if (map(x+dx, y+dy) /= "#" .and. visited(x+dx, y+dy) + 2000 > cost) then
            call gdir(dir, dx, dy)
            visited(x+dx, y+dy) = min(visited(x+dx, y+dy), cost+1001)
            call apps(cache, x+dx, y+dy, cost+1001, dir, ncache)
        end if

        dx = -dx
        dy = -dy
        if (map(x+dx, y+dy) /= "#" .and. visited(x+dx, y+dy) + 2000 > cost) then
            call gdir(dir, dx, dy)
            visited(x+dx, y+dy) = min(visited(x+dx, y+dy), cost+1001)
            call apps(cache, x+dx, y+dy, cost+1001, dir, ncache)
        end if

        call swap(dx, dy)
        if (map(x+dx, y+dy) /= "#" .and. visited(x+dx, y+dy) + 2000 > cost) then
            call gdir(dir, dx, dy)
            visited(x+dx, y+dy) = min(visited(x+dx, y+dy), cost+2001)
            call apps(cache, x+dx, y+dy, cost+2001, dir, ncache)
        end if

        step = step + 1
    end do

    print *, result

contains
    subroutine apps(arr, x, y, cost, dir, narr)
        integer, intent(inout) :: arr(:,:)
        integer, intent(inout) :: narr
        integer, intent(in) :: x, y, cost, dir

        narr = narr + 1
        arr(narr, 1) = x
        arr(narr, 2) = y
        arr(narr, 3) = cost
        arr(narr, 4) = dir
        
    end subroutine apps

    subroutine swap(a, b)
        integer, intent(inout) :: a, b
        integer :: temp
        temp = a
        a = b
        b = temp
    end subroutine swap

    subroutine cdir(dir, dx, dy)
        integer, intent(inout) :: dx, dy, dir
        dx = 0
        dy = 0
        if (dir == 1) dy = 1
        if (dir == 2) dx = -1
        if (dir == 3) dy = -1
        if (dir == 4) dx = 1
    end subroutine cdir

    subroutine gdir(dir, dx, dy)
        integer, intent(inout) :: dx, dy, dir
        if (dx == 1 .and. dy == 0) dir = 4
        if (dx == 0 .and. dy == 1) dir = 1
        if (dx == -1 .and. dy == 0) dir = 2
        if (dx == 0 .and. dy == -1) dir = 3
    end subroutine gdir

end program sixteen_one
