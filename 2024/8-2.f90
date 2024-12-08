program eight_two
    implicit none
    integer, parameter :: nchars = 300
    integer, parameter :: max_size = 1000
    integer :: ios, i, j, n, m, result, char
    character(len=256) :: line
    integer :: points(nchars, 300, 2) = 0
    integer :: map(max_size, max_size) = 0
    integer :: counts(nchars) = 0

    i = 1
    n = 0
    m = 0
    do
        read(*, "(A)", iostat=ios) line
        if (ios /= 0) exit

        do j = 1, len_trim(line)
            if (line(j:j) /= ".") then
                char = ichar(line(j:j))
                counts(char) = counts(char) + 1
                points(char, counts(char), 1) = i
                points(char, counts(char), 2) = j
            end if
        end do

        n = n + 1
        m = len(trim(line))
        i = i + 1
    end do

    do i = 1, nchars
        j = mark_echo(map, n, m, points(i, :, :), counts(i))
    end do

    result = 0
    do i = 1, n
        do j = 1, m
            result = result + map(i, j)
        end do
    end do

    print *,  result

contains
function mark_echo(map, n, m, points, count) result(output)
    integer, dimension(:,:), intent(inout) :: map
    integer, dimension(:,:), intent(in) :: points
    integer, intent(in) :: n, m, count
    integer :: output
    integer :: i, j, dx, dy, x1, y1, x2, y2, nx, ny

    do i = 1, count
        do j = i + 1, count
            x1 = points(i, 1)
            y1 = points(i, 2)
            x2 = points(j, 1)
            y2 = points(j, 2)

            dx = x2 - x1
            dy = y2 - y1

            nx = x1
            ny = y1
            do
                nx = nx + dx
                ny = ny + dy
                if (.not. (1 <= nx .and. 1 <= ny .and. nx <= n .and. ny <= m)) exit
                map(nx, ny) = 1
            end do
            nx = x2
            ny = y2
            do
                nx = nx - dx
                ny = ny - dy
                if (.not. (1 <= nx .and. 1 <= ny .and. nx <= n .and. ny <= m)) exit
                map(nx, ny) = 1
            end do
        end do
    end do
    
end function mark_echo

end program eight_two
