module sort_module
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
end module sort_module
