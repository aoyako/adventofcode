module util
    implicit none
contains
    recursive subroutine quicksort(a, low, high)
        integer, intent(inout) :: a(:)
        integer, intent(in) :: low, high
        integer :: pivot, i, j, temp

        if (low < high) then
            pivot = a(high)
            i = low - 1

            do j = low, high - 1
                if (a(j) <= pivot) then
                    i = i + 1
                    temp = a(i)
                    a(i) = a(j)
                    a(j) = temp
                end if
            end do

            temp = a(i + 1)
            a(i + 1) = a(high)
            a(high) = temp

            call quicksort(a, low, i)
            call quicksort(a, i + 2, high)
        end if
    end subroutine quicksort

    integer function count_whitespaces(line)
        character(len=*) :: line
        integer :: i
        integer :: count

        count = 0
        do i = 1, len(trim(adjustl(line)))
        if (line(i:i) == ' ') then
            count = count + 1
        end if
        end do

        count_whitespaces = count
    end function count_whitespaces

    function read_int_list()
        implicit none
        character(len=1024) :: input_line
        integer :: ios, n
        integer, allocatable :: read_int_list(:)
        integer :: empty_array(0)

        read(*, '(A)', iostat=ios) input_line
        if (ios /= 0) then
            read_int_list = empty_array
            return
        end if

        n = count_whitespaces(input_line) + 1
        
        allocate(read_int_list(n))

        read(input_line, *) read_int_list

    end function read_int_list
end module util
