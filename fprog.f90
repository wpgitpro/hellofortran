program f_prog
    integer :: i,j(2)
    real :: r
    character(len=10) :: str

    i = 4711
    j = (/13, 14/)
    r = 4712.0
    str = "abcd"
    call c_sub(trim(str)//char(0), i, r, j)
end program f_prog