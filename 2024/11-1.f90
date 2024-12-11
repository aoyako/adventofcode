program eleven_one
    use util
    use iso_fortran_env, only: int64
    implicit none
    integer, parameter :: blinks = 25
    integer(int64) :: ierr, i, j, n, m, extra_n, result
    integer, allocatable :: arr(:)
    integer(int64) :: main(10000000, 2)

    arr = read_int_list()
    n = size(arr)
    
    do i = 1, n
        main(i, 1) = arr(i)
        main(i, 2) = 1
    end do

    do i = 1, blinks
        m = process(main, n)
    end do

    result = 0
    do i = 1, n
        result = result + main(i, 2)
    end do

    print *, result

contains
    integer function process(map, n)
        integer(int64), intent(inout) :: map(:,:)
        integer(int64), intent(inout) :: n
        integer(int64) :: i, j, x, y, ndig, value, count, m
        integer(int64):: copy(10000000, 2)

        m = 0

        do i = 1, n
            value = map(i, 1)
            count = map(i, 2)

            if (value == 0) then
                m = m + 1
                copy(m, 1) = 1
                copy(m, 2) = count
            elseif (mod(int(log10(real(value))) + 1, 2) == 0) then
                ndig = int(log10(real(value))) + 1
                x = value / (10**(ndig/2))
                y = mod(value, (10**(ndig/2)))

                m = m + 1
                copy(m, 1) = x
                copy(m, 2) = count

                m = m + 1
                copy(m, 1) = y
                copy(m, 2) = count
            else
                m = m + 1
                copy(m, 1) = 2024*value
                copy(m, 2) = count
            end if
        end do

        do j = m, 1, -1
            do i = 1, j-1
                if (copy(i, 1) == copy(j, 1)) then
                    copy(i, 2) = copy(i, 2) + copy(j, 2)
                    copy(j, 2) = -1
                    exit
                end if
            end do
        end do

        i = 0
        do j = 1, m
            if (copy(j, 2) /= -1) then
                i = i + 1
                map(i, 1) = copy(j, 1)
                map(i, 2) = copy(j, 2)
            end if
        end do

        n = i

        process = 1
    end function process
end program eleven_one
