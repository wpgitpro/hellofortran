! Version: 0.1
!
PROGRAM helloworld
USE mathModule
IMPLICIT NONE
INTEGER nl
COMPLEX :: link
link = (3,4)
PRINT *, "Hello World!"  
PRINT *, "pi:", pi, "e:", e, "gamma:", gamma  
PRINT *, link
WRITE(*,*) "Number Of Mechanism Loops ?"
READ(*,'(I1)') nl
WRITE(*,*) nl
STOP 0 
END PROGRAM helloworld
