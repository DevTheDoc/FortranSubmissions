implicit none
integer, allocatable :: sq(:,:)
integer :: n, i, j, k, row, col, new_row, new_col

print *, 'Enter odd size n:'
read *, n

if (n < 1 .or. mod(n, 2) == 0) then
    print *, 'n must be a positive odd number.'
    stop
end if

allocate(sq(n, n))
sq = 0

row = 1
col = (n + 1) / 2

do k = 1, n*n
    sq(row, col) = k

    new_row = row - 1
    new_col = col + 1
    if (new_row < 1) new_row = n
    if (new_col > n) new_col = 1

    if (sq(new_row, new_col) /= 0) then
        new_row = row + 1
        new_col = col
        if (new_row > n) new_row = 1
    end if

    row = new_row
    col = new_col
end do

do i = 1, n
    write(*,*) (sq(i, j), j = 1, n)
end do


deallocate(sq)
end
