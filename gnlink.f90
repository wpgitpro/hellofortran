! Program gnlink
! Version: 0.1
!
PROGRAM helloworld
USE mathModule
IMPLICIT NONE
INTEGER i, j
INTEGER nl, nv
COMPLEX :: link
link = (3,4)
PRINT *, "Hello World!"  
PRINT *, "pi:", pi, "e:", e, "gamma:", gamma  
PRINT *, link
WRITE(*,*) "Number Of Mechanism Loops ?"
READ(*,'(I1)') nl
WRITE(*,*) nl
do i = 1, nl
    WRITE(*,*) "Number of Links in Loop ?"
    READ(*,'(I1)') nv
    do j = 1, nv
        WRITE(*,*) "Link ", j
    end do
end do
WRITE(*,*) "End of program"
STOP 0 
END PROGRAM helloworld
