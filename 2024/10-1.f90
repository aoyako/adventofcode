program ten_one
    use iso_fortran_env, only: int64
    implicit none
    integer :: ios, i, j, n, m, result, nvisited
    character(len=256) :: line
    integer :: map(100, 100) = -1

    n = 0
    m = 0
    do
        read(*, "(A)", iostat=ios) line
        if (ios /= 0) exit

        do i = 1, len_trim(line)
            map(n+1, i) = ichar(line(i:i)) - ichar("0")
        end do

        m = len_trim(line)
        n = n + 1
    end do

    result = 0
    do i = 1, n
        do j = 1, m
            if (map(i, j) == 0) then 
                result = result + cways(map, n, m, i, j)
            end if
        end do
    end do

    print *, result

contains

integer function cways(map, n, m, x, y)
    integer, dimension(:,:), intent(in) :: map
    integer, intent(in):: n, m, x, y
    integer :: visited(100, 100)
    integer :: cache(10000, 2)
    integer :: i, j, result, ncache, cx, cy, d, cval

    visited(:,:) = 0
    ncache = 1
    cache(1, 1) = x
    cache(1, 2) = y

    result = 0
    do
        cx = cache(ncache, 1)
        cy = cache(ncache, 2)
        ncache = ncache - 1

        cval = map(cx, cy)
        if (cval == 9) result = result + 1

        do d = -1, 1, 2
            if (cx + d >= 1 .and. cx + d <= n) then
                if (visited(cx + d, cy) == 0 .and. map(cx + d, cy) == cval + 1) then
                    visited(cx + d, cy) = 1
                    cache(ncache+1, 1) = cx + d
                    cache(ncache+1, 2) = cy
                    ncache = ncache + 1
                end if
            end if
            if (cy + d >= 1 .and. cy + d <= m) then
                if (visited(cx, cy + d) == 0 .and. map(cx, cy + d) == cval + 1) then
                    visited(cx, cy + d) = 1
                    cache(ncache+1, 1) = cx
                    cache(ncache+1, 2) = cy + d
                    ncache = ncache + 1
                end if
            end if
        end do

        if (ncache == 0) exit
    end do
    
    cways = result
    
end function cways

end program ten_one