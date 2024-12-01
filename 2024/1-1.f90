program one_one
    use sort_module
    implicit none
    integer, parameter :: max_size = 3000
    integer :: arr1(max_size), arr2(max_size)
    integer :: i, n, ios, res

    n = 0

    do
        read(*, *, iostat=ios) arr1(n + 1), arr2(n + 1)
        if (ios /= 0) exit
        n = n + 1
    end do

    call quicksort(arr1, 1, n)
    call quicksort(arr2, 1, n)

    res = 0
    do i = 1, n
        res = res + abs(arr1(i) - arr2(i))
    end do

    print *, res

end program one_one
