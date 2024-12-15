program fifteen_one
    implicit none
    character(len=10000) :: line
    character(1) :: map(100, 100)
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
            map(i, j) = line(j:j)
            if (map(i,j) == "@") then
                x = i
                y = j
            end if
        end do

        n = n + 1
        m = len_trim(line)
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
            if (map(i, j) == "O") result = result + 100*(i-1) + (j-1)
        end do
    end do

    do ii = 1, n
        do jj = 1, m
            write(*, "(A)", advance="no") map(ii,jj)
        end do
        print *
    end do

    print *, result

contains
  subroutine simulate(map, n, m, x, y, dir)
    character(1), intent(inout) :: map(:,:)
    integer, intent(in) :: n, m
    integer, intent(inout) :: x, y
    integer :: dx, dy, nx, ny, i, j, k
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

    do k = 1, 100
        if (map(x+k*dx, y+k*dy) == "#") exit
        if (map(x+k*dx, y+k*dy) == ".") then
            map(x, y) = "."
            map(x+k*dx, y+k*dy) = "O"
            map(x+dx, y+dy) = "@"
            x = x + dx
            y = y + dy
            exit
        end if
    end do
    end subroutine simulate
end program fifteen_one
