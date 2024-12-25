program twentyfour_one
    implicit none
    integer, parameter :: uint128 = selected_int_kind(38)
    character(len=256) :: line
    integer :: ecode, ios, n, ncache, i, result, j, nlocks, nkeys
    integer :: locks(1000, 5), keys(1000, 5), scheme(5)

    nlocks = 0
    nkeys = 0
    do
        read(*, *, iostat=ios) line
        if (ios /= 0) exit
        scheme = 0

        if (line(1:1) == "#") then
            nlocks = nlocks + 1
            do j = 1, 6
                read(*, *, iostat=ios) line
                do i = 1, 5
                    if (line(i:i) == "#") scheme(i) = scheme(i) + 1
                end do
            end do
            locks(nlocks,:) = scheme
        else
            nkeys = nkeys + 1
            do j = 1, 6
                read(*, *, iostat=ios) line
                do i = 1, 5
                    if (line(i:i) == "#") scheme(i) = scheme(i) + 1
                end do
            end do
            scheme = scheme - 1
            keys(nkeys,:) = scheme
        end if
    end do

    result = 0
    do i = 1, nkeys
        do j = 1, nlocks
            if (match(keys(i,:), locks(j,:))) result = result + 1
        end do
    end do

    print *, result

contains
function match(lock, key) result(output)
    integer, intent(in) :: lock(:), key(:)
    logical :: output
    integer :: i
    
    do i = 1, 5
        if (lock(i) + key(i) > 5) then
            output = .false.
            return
        end if
    end do
    output = .true.
    
end function match

end program twentyfour_one
