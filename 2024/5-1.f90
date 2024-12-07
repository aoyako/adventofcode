program five_one
    use util
    implicit none
    integer :: ios, i, j, n, m, result
    character(len=256) :: line
    integer, allocatable :: arr(:)
    integer :: map(1000, 1000) = 0

    do 
        read(*, "(A)", iostat=ios) line
        if (len_trim(line) == 0) exit

        do i = 1, len_trim(line)
            if (line(i:i) == "|") then
                line(i:i) = " "
            end if
        end do

        read(line, *, iostat=ios) i, j
        map(j, i) = 1
    end do

    result = 0
    do
        arr = read_int_list_sep(",")
        if (size(arr) == 0) exit

        if (check(map, arr)) then
            result = result + arr(1+size(arr)/2)
        end if
    end do

    print *,  result

contains
    logical function check(map, pages)
        integer, intent(in) :: map(:,:)
        integer, intent(in) :: pages(:)
        integer :: i, j, x, y

        do i = 1, size(pages)-1
            do j = i+1, size(pages)
                x = pages(i)
                y = pages(j)
                if (map(x, y) == 1) then
                    check = .false.
                    return
                end if
            end do
        end do

        check = .true.
    end function check
end program five_one