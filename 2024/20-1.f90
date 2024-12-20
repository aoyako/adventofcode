program twenty_one
    implicit none
    integer, parameter :: max_size = 100000
    integer :: ios, n, m, i, j, x, y, nx, ny, d, result, c, dx, dy, bx, by, ex, ey, ncache, tmp, orig
    integer :: from_begin(1000, 1000), from_end(1000, 1000), cache(max_size, 3), deltas(4, 2), scache(max_size, 4)
    character(1) :: map(1000, 1000)
    character(256) :: line

    deltas = reshape([&
        1, 0, &
       -1, 0, &
        0, 1, &
        0, -1], &
        shape=[4, 2])

    n = 0
    m = 0
    do
        read(*, "(A)", iostat=ios) line
        if (ios /= 0) exit
        
        n = n + 1
        do i = 1, len_trim(line)
            map(n, i) = line(i:i)
            if (map(n, i) == "S") then
                bx = n
                by = i
                map(n, i) = "."
            end if
            if (map(n, i) == "E") then
                ex = n
                ey = i
                map(n, i) = "."
            end if
        end do

        m = len_trim(line)
    end do

    from_begin = -1
    cache(1, 1) = bx
    cache(1, 2) = by
    cache(1, 3) = 0
    ncache = 1
    from_begin(bx, by) = 0

    do
        if (ncache == 0) exit
        ncache = ncache - 1
        x = cache(1, 1)
        y = cache(1, 2)
        c = cache(1, 3)
        cache(:max_size-1,:) = cache(2:,:)

        do d = 1, 4
            dx = deltas(d, 1)
            dy = deltas(d, 2)

            nx = x + dx
            ny = y + dy
            if (1 <= nx .and. nx <= n .and. 1 <= ny .and. ny <= m) then
                if (map(nx, ny) == "." .and. from_begin(nx, ny) == -1) then
                    ncache = ncache + 1
                    cache(ncache, 1) = nx
                    cache(ncache, 2) = ny
                    cache(ncache, 3) = c + 1
                    from_begin(nx, ny) = c + 1
                end if
            end if
        end do
    end do

    from_end = -1
    cache(1, 1) = ex
    cache(1, 2) = ey
    cache(1, 3) = 0
    ncache = 1
    from_end(ex, ey) = 0

    do
        if (ncache == 0) exit
        ncache = ncache - 1
        x = cache(1, 1)
        y = cache(1, 2)
        c = cache(1, 3)
        cache(:max_size-1,:) = cache(2:,:)

        do d = 1, 4
            dx = deltas(d, 1)
            dy = deltas(d, 2)

            nx = x + dx
            ny = y + dy
            if (1 <= nx .and. nx <= n .and. 1 <= ny .and. ny <= m) then
                if (map(nx, ny) == "." .and. from_end(nx, ny) == -1) then
                    ncache = ncache + 1
                    cache(ncache, 1) = nx
                    cache(ncache, 2) = ny
                    cache(ncache, 3) = c + 1
                    from_end(nx, ny) = c + 1
                end if
            end if
        end do
    end do

    orig = from_begin(ex, ey)
    result = 0
    ncache = 0
    do i = 1, n
        do j = 1, m
            if (from_begin(i, j) /= -1) then
                do dx = -2, 2
                    do dy = -2, 2
                        if (abs(dx) + abs(dy) > 2) cycle
                        nx = i + dx
                        ny = j + dy
                        if (1 <= nx .and. nx <= n .and. 1 <= ny .and. ny <= m) then
                            if (from_end(nx, ny) /= -1) then
                                tmp = from_begin(i, j) + abs(dx) + abs(dy) + from_end(nx, ny)
                                do c = 1, ncache
                                    if (scache(c, 1) == i .and. scache(c, 2) == j .and. &
                                        scache(c, 3) == nx .and. scache(c, 4) == ny) goto 100 
                                end do

                                if (tmp < orig) then
                                    ncache = ncache + 1
                                    scache(ncache, 1) = i
                                    scache(ncache, 2) = j
                                    scache(ncache, 3) = nx
                                    scache(ncache, 4) = ny    
                                    if (tmp <= orig - 100) result = result + 1
                                end if
                            end if
                        end if

                        100 continue
                    end do
                end do
            end if
        end do
    end do
    
    print *, result
end program twenty_one
