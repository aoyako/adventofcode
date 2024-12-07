program seven_two
    use util
    use iso_fortran_env, only: int64
    implicit none
    integer(int64) :: ios, i, j, result
    character(len=256) :: line
    character(len=1) :: map(1000, 1000)
    integer(int64), allocatable :: arr(:)

    i = 0
    do
        i = i + 1
        arr = read_big_int_list()
        if (size(arr) == 0) exit
        if (can_build(arr(1), arr(2:))) then 
            result = result + arr(1)
        end if
    end do

    print *, result

contains
    logical function can_build(tgt, arr)
        integer(int64), intent(in) :: tgt
        integer(int64), dimension(:), intent(in) :: arr
        integer(int64) :: tmp, map, i, j, it, n, digits

        n = size(arr)
        do map = 0, 3**(n-1)
            it = map
            tmp = arr(1)
            do i = 2, size(arr)
                if (mod(it, 3) == 0) then
                    tmp = tmp + arr(i)
                else if (mod(it, 3) == 1) then
                    tmp = tmp * arr(i)
                else
                    digits = int(log10(real(arr(i)))) + 1
                    tmp = tmp*(10**digits) + arr(i)
                end if
                it = it / 3
            end do

            if (tmp == tgt) then
                can_build = .true.
                return
            end if
        end do

        can_build = .false.
    end function can_build

    function read_big_int_list()
        implicit none
        character(len=1024) :: input_line
        integer(int64) :: ios, n, pos
        integer(int64), allocatable :: read_big_int_list(:)
        integer(int64) :: empty_array(0)

        read(*, '(A)', iostat=ios) input_line
        if (ios /= 0) then
            read_big_int_list = empty_array
            return
        end if

        n = count_whitespaces(input_line) + 1

        do
            pos = index(input_line, ":")
            if (pos == 0) exit
            input_line(pos:pos) = " "
        end do
        
        allocate(read_big_int_list(n))

        read(input_line, *) read_big_int_list

    end function read_big_int_list

end program seven_two
