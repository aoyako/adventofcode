program six_two
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

    do i = 1, n
        do j = 1, m
            if (map(i,j) == "." .and. .not. all([i,j] == [x, y])) then
                map(i,j) = "#"
                if (simulate(map, x, y, n, m) == 1) result = result + 1
                map(i,j) = "."
            end if
        end do
    end do

    print *,  result

contains
  integer function simulate(arr, xin, yin, n, m)
    character(1), intent(in) :: arr(:,:)
    integer, intent(in) :: xin, yin, n, m
    integer :: dx, dy, nx, ny, i, j, step, x, y
    character(1) :: cdir
    logical :: visited(1000, 1000, 4)
    

    x = xin
    y = yin
    cdir = map(x, y)
    step = 0

    do
        dx = 0
        dy = 0
        if (cdir == "^") dx = -1
        if (cdir == ">") dy = 1
        if (cdir == "v") dx = 1
        if (cdir == "<") dy = -1

        nx = x + dx
        ny = y + dy

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

        step = step + 1
        if (step == 1000000) exit
    end do

    simulate = 0
    if (step == 1000000) simulate = 1

  end function simulate

end program six_two
