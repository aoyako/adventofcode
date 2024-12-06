program six_one
    implicit none
    integer :: ios, i, j, n, m, result, x, y
    character(len=256) :: line
    character(len=1) :: map(1000, 1000)

    i = 1
    n = 0
    m = 0
    do
        read(*, "(A)", iostat=ios) line
        if (ios /= 0) exit

        do j = 1, len_trim(line)
            map(i, j) = line(j:j)
        end do

        n = n + 1
        m = len(trim(line))
        i = i + 1
    end do

    do i = 1, n
        do j = 1, m
            if (map(i, j) == "^") then
                x = i
                y = j
                goto 100
            end if
        end do
    end do

    100 continue

    result = 0

    call simulate(map, x, y, n, m)
    do i = 1, n
        do j = 1, m
            if (map(i,j) == "X") then
                result = result + 1
            end if
        end do
    end do

    print *,  result

contains
  subroutine simulate(arr, x, y, n, m)
    character(1), intent(inout) :: arr(:,:)
    integer, intent(inout) :: x, y, n, m
    integer :: dx, dy, nx, ny, i, j
    character(1) :: cdir

    cdir = map(x, y)
    do
        dx = 0
        dy = 0
        if (cdir == "^") dx = -1
        if (cdir == ">") dy = 1
        if (cdir == "v") dx = 1
        if (cdir == "<") dy = -1

        nx = x + dx
        ny = y + dy

        map(x,y) = "X"
        if (.not. (nx < 1 .or. nx > n .or. ny < 1 .or. ny > m)) then
            if (map(nx,ny) == "#") then
                select case (cdir)
                    case("^")
                        cdir = ">"
                    case(">")
                        cdir = "v"
                    case("v")
                        cdir = "<"
                    case("<")
                        cdir = "^"
                end select
            else
                x = nx
                y = ny
            end if
        else
            exit
        end if
    end do

  end subroutine simulate

end program six_one
