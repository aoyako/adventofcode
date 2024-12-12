program twelve_one
    implicit none
    integer :: ios, i, j, n, m, result, x, y, area, perimeter, step
    character(len=256) :: line
    character(len=1) :: map(1000, 1000)
    integer :: visited(1000, 1000) = 0

    n = 0
    m = 0
    do
        read(*, "(A)", iostat=ios) line
        if (ios /= 0) exit

        n = n + 1
        do j = 1, len_trim(line)
            map(n, j) = line(j:j)
        end do

        m = len(trim(line))
    end do

    step = 1
    result = 0
    do i = 1, n
        do j = 1, m
            if (visited(i, j) == 0) then
                area = 0
                perimeter = 0
                call fill(map, visited, n, m, area, perimeter, i, j, step)

                result = result + perimeter*area
                step = step + 1
            end if
        end do
    end do

    print *,  result

contains
  subroutine fill(map, visited, n, m, area, perimeter, x, y, step)
    character(1), intent(in) :: map(:,:)
    integer, intent(inout) :: visited(:,:)
    integer, intent(in) :: n, m, x, y, step
    integer, intent(inout) :: area, perimeter

    integer :: cache(10000, 2), stack(10000, 2)
    integer :: d, nx, ny, i, j, ncache, nstack, cx, cy
    character(1) :: cdir
    logical :: has

    perimeter = 0
    area = 0

    ncache = 0
    nstack = 1
    stack(nstack, 1) = x
    stack(nstack, 2) = y

    do
        if (nstack == 0) exit

        cx = stack(nstack, 1)
        cy = stack(nstack, 2)
        nstack = nstack - 1

        if (.not. (1 <= cx .and. cx <= n .and. 1 <= cy .and. cy <= m)) then
            has = .false.
            ! do i = 1, ncache
            !     if (cache(ncache, 1) == cx .and. cache(ncache, 2) == cy) has = .true.
            ! end do
            if (.not. has) then
                ncache = ncache + 1
                cache(ncache, 1) = cx
                cache(ncache, 2) = cy
            end if
            cycle
        end if

        if ((visited(cx, cy) /= step .and. visited(cx, cy) /= 0) .or. map(cx, cy) /= map(x, y)) then
            has = .false.
            ! do i = 1, ncache
            !     if (cache(ncache, 1) == cx .and. cache(ncache, 2) == cy) has = .true.
            ! end do
            if (.not. has) then
                ncache = ncache + 1
                cache(ncache, 1) = cx
                cache(ncache, 2) = cy
            end if
            cycle
        end if

        if (visited(cx, cy) /= 0) cycle
        if (map(cx, cy) /= map(x, y)) cycle

        visited(cx, cy) = step
        area = area + 1

        do d = -1, 1, 2
            nstack = nstack + 1
            stack(nstack, 1) = cx + d
            stack(nstack, 2) = cy

            nstack = nstack + 1
            stack(nstack, 1) = cx
            stack(nstack, 2) = cy + d
        end do
    end do

    perimeter = ncache

  end subroutine fill

end program twelve_one
