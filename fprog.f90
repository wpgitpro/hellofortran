program f_prog
    integer :: i
    real :: r
    character(len=10) :: str
    common i, r
    i = 111
    r = 112.0
    str = "efghij"
    write(*,'(A,F5.1)') "This is the real: ", r
    call c_sub(trim(str)//char(0), i, r)
    call testsub()
    write(*,'(A,F5.1)') "This is the real: ", r
end program f_prog

!
! gcc -c -fno-leading-underscore csub.c
!

subroutine testsub()
integer i 
real r 
common i, r 
r = 211.0
write(*,'(A,I3)') "This is the integer: ", i
write(*,'(A,F5.1)') "This is the real: ", r
end subroutine testsub

