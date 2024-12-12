program twelve_two
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
    logical function is_legal(n, m, x, y)
        integer, intent(in) :: n, m, x, y
        is_legal = (1 <= x .and. x <= n .and. 1 <= y .and. y <= m)
    end function is_legal

  subroutine fill(map, visited, n, m, area, perimeter, x, y, step)
    character(1), intent(in) :: map(:,:)
    integer, intent(inout) :: visited(:,:)
    integer, intent(in) :: n, m, x, y, step
    integer, intent(inout) :: area, perimeter

    integer :: stack(10000, 2)
    logical :: gaps(2, 2)
    logical :: in_gaps(2, 2)
    integer :: d, nx, ny, i, j, nstack, cx, cy, order, dx, dy
    character(1) :: cdir
    logical :: has

    perimeter = 0
    area = 1

    nstack = 1
    stack(nstack, 1) = x
    stack(nstack, 2) = y

    do
        if (nstack == 0) exit

        cx = stack(nstack, 1)
        cy = stack(nstack, 2)
        nstack = nstack - 1
        visited(cx, cy) = step

        gaps = .true.
        in_gaps = .true.

        if (is_legal(n, m, cx-1, cy) .and. is_legal(n, m, cx, cy-1)) then
            if (map(cx-1, cy) == map(x, y) .and. map(cx, cy-1) == map(x, y) .and. map(cx-1, cy-1) /= map(x, y)) then
                perimeter = perimeter + 1
            end if
        end if
        if (is_legal(n, m, cx+1, cy) .and. is_legal(n, m, cx, cy-1)) then
            if (map(cx+1, cy) == map(x, y) .and. map(cx, cy-1) == map(x, y) .and. map(cx+1, cy-1) /= map(x, y)) then
                perimeter = perimeter + 1
            end if
        end if
        if (is_legal(n, m, cx+1, cy) .and. is_legal(n, m, cx, cy+1)) then
            if (map(cx+1, cy) == map(x, y) .and. map(cx, cy+1) == map(x, y) .and. map(cx+1, cy+1) /= map(x, y)) then
                perimeter = perimeter + 1
            end if
        end if
        if (is_legal(n, m, cx-1, cy) .and. is_legal(n, m, cx, cy+1)) then
            if (map(cx-1, cy) == map(x, y) .and. map(cx, cy+1) == map(x, y) .and. map(cx-1, cy+1) /= map(x, y)) then
                perimeter = perimeter + 1
            end if
        end if

        do d = -1, 1, 2
            if (is_legal(n, m, cx+d, cy)) then
                if (visited(cx+d, cy) == 0 .and. map(cx+d, cy) == map(x, y)) then
                    nstack = nstack + 1
                    stack(nstack, 1) = cx + d
                    stack(nstack, 2) = cy
                    area = area + 1
                    visited(cx + d, cy) = step
                end if
                
                gaps(1, (d+1)/2 + 1) = map(cx+d, cy) /= map(x, y)
            end if

            if (is_legal(n, m, cx, cy+d)) then
                if (visited(cx, cy+d) == 0 .and. map(cx, cy+d) == map(x, y)) then
                    nstack = nstack + 1
                    stack(nstack, 1) = cx
                    stack(nstack, 2) = cy + d
                    area = area + 1
                    visited(cx, cy + d) = step
                end if

                gaps(2, (d+1)/2 + 1) = map(cx, cy+d) /= map(x, y)
            end if
        end do


        if (gaps(1, 1) .and. gaps(2,2)) perimeter = perimeter + 1
        if (gaps(1, 2) .and. gaps(2,2)) perimeter = perimeter + 1
        if (gaps(1, 1) .and. gaps(2,1)) perimeter = perimeter + 1
        if (gaps(1, 2) .and. gaps(2,1)) perimeter = perimeter + 1
    end do

  end subroutine fill


end program twelve_two
