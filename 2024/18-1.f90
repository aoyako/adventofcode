program eighteen_one
    implicit none
    integer, parameter :: max_size = 100000
    integer :: ios, n, m, i, x, y, nx, ny, d, result, c, dx, dy, ncache
    integer :: map(300, 300), visited(300, 300), cache(max_size, 3), deltas(4, 2)

    map = 0
    visited = 0
    cache(1, 1) = 1
    cache(1, 2) = 1
    cache(1, 3) = 0
    ncache = 1

    deltas = reshape([&
        1, 0, &
       -1, 0, &
        0, 1, &
        0, -1], &
        shape=[4, 2])

    n = 71
    m = 71

    do i = 1, 1024
        read(*, *, iostat=ios) x, y
        if (ios /= 0) exit
        map(x+1, y+1) = -1
    end do

    result = 0
    do
        if (ncache == 0) exit
        ncache = ncache - 1
        x = cache(1, 1)
        y = cache(1, 2)
        c = cache(1, 3)
        cache(:max_size-1,:) = cache(2:,:)

        if (x == n .and. y == m) then
            result = c
            exit
        end if

        do d = 1, 4
            dx = deltas(d, 1)
            dy = deltas(d, 2)

            nx = x + dx
            ny = y + dy
            if (1 <= nx .and. nx <= n .and. 1 <= ny .and. ny <= m) then
                if (map(nx, ny) == 0) then
                    ncache = ncache + 1
                    cache(ncache, 1) = nx
                    cache(ncache, 2) = ny
                    cache(ncache, 3) = c + 1
                    map(nx, ny) = 1
                end if
            end if
        end do
    end do
    
    print *, result
end program eighteen_one
