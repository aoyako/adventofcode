program twentythree_two
    implicit none
    integer :: ios, x, y, result, i
    character(256) :: line
    integer :: map(1000, 1000)
    integer :: visited(1000, 1000, 1000), P(1000), R(1000), XX(1000)

    map = 0
    visited = 0
    do
        read(*, "(A)", iostat=ios) line
        if (ios /= 0) exit

        x = (ichar(line(1:1)) - ichar("a"))*30 + (ichar(line(2:2)) - ichar("a")) + 1
        y = (ichar(line(4:4)) - ichar("a"))*30 + (ichar(line(5:5)) - ichar("a")) + 1
        map(x, y) = 1
        map(y, x) = 1
    end do

    R = 0
    XX = 0
    P = 0
    P = merge(1, 0, sum(map, dim=1) > 0)

    P = bron_lerbosch(map, R, P, XX)

    do i = 1, 1000
        if (contains_set(P, i)) then
           call convert(i)
        end if
    end do

contains

! algorithm BronKerbosch1(R, P, X) is
!     if P and X are both empty then
!         report R as a maximal clique
!     for each vertex v in P do
!         BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
!         P := P \ {v}
!         X := X ⋃ {v}
recursive function bron_lerbosch(map, R, P, X) result(output)
    integer, intent(in) :: R(:), map(:,:)
    integer, intent(inout) :: P(:), X(:)
    integer :: output(1000), R_new(1000), P_new(1000), X_new(1000), v, new_set(1000)

    output = 0
    R_new = 0
    P_new = 0
    X_new = 0
    new_set = 0
    if (is_empty(P) .and. is_empty(X)) then
        call update_set(output, R)
        return
    end if

    do v = 1, 1000
        if (.not. contains_set(P, v)) cycle
        R_new = R
        P_new = P
        X_new = X
        call add_set(R_new, v)
        call intersect_set(P_new, map(v,:))
        call intersect_set(X_new, map(v,:))

        new_set = bron_lerbosch(map, R_new, P_new, X_new)
        call update_set(output, new_set)

        call remove_set(P, v)
        call add_set(X, v)
    end do
    
end function bron_lerbosch

subroutine update_set(output, res)
    integer, intent(inout) :: output(:)
    integer, intent(in) :: res(:)
    if (sum(res) > sum(output)) output = res
end subroutine update_set

subroutine add_set(set, value)
    integer, intent(inout) :: set(:)
    integer, intent(in) :: value
    set(value) = 1
end subroutine add_set

subroutine remove_set(set, value)
    integer, intent(inout) :: set(:)
    integer, intent(in) :: value
    set(value) = 0
end subroutine remove_set

logical function contains_set(set, value)
    integer, intent(inout) :: set(:)
    integer, intent(in) :: value
    contains_set = set(value) == 1
end function contains_set

logical function is_empty(set)
    integer, intent(in) :: set(:)
    is_empty = sum(set) == 0
end function is_empty

subroutine intersect_set(set_a, set_b)
    integer, intent(inout) :: set_a(:)
    integer, intent(in) :: set_b(:)
    set_a = set_a * set_b
end subroutine intersect_set

subroutine convert(v)
    integer, intent(in) :: v
    integer :: x, y

    x = (v-1) / 30
    y = mod((v-1), 30)

    write(*, "(A3)", advance="no") char(x+ichar("a"))//char(y+ichar("a"))//","
end subroutine convert

end program twentythree_two
