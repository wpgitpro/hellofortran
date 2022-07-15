!
! Version: 0.1.2
!
PROGRAM helloworld
USE mathModule
IMPLICIT NONE
INTEGER i, j
INTEGER :: nl
INTEGER, DIMENSION(5) :: nv
COMPLEX :: link, linka, linkb
REAL, DIMENSION(5,5) :: model
LOGICAL :: lexist
linka = (3,4)
linkb = (4,6)
link = linka + linkb
PRINT *, "Hello World!"  
PRINT *, "pi:", pi, "e:", e, "gamma:", gamma  
PRINT *, link
WRITE(*,*) "Number Of Mechanism Loops ?"
READ(*,'(I1)') nl
WRITE(*,*) nl
WRITE(*,*) "Maximum number of vectors in any loop ?"
READ(*,'(I1)') 
do i = 1, nl
    WRITE(*,*) "Number of Links in Loop ?", i
    READ(*,'(I1)') nv(i)
    do j = 1, nv(i)
        WRITE(*,*) "Length of link ", j, "?"
        READ(*,*) model(i,j) 
        WRITE(*,*) "Link ", j, " is ", model(i,j)
    end do
end do
!
! Does model file already exist?
INQUIRE(FILE="model.dat",EXIST=lexist)
IF (.NOT. lexist) THEN
    OPEN(UNIT=9,FILE='model.dat',STATUS='NEW',ACTION='WRITE')
    ! Output
    ! Number of vectors in get_model
    ! Number of Loops
    ! Number of vectors in loop
    WRITE(9,*) nv(1), nl, nv(1)
    CLOSE(9,STATUS='KEEP')
END IF
!
WRITE(*,*) "End of program"
STOP 0 
END PROGRAM helloworld

SUBROUTINE save_model(model)
    ! notes
END SUBROUTINE save_model

SUBROUTINE get_model(model)
    ! notes
END SUBROUTINE get_model

SUBROUTINE check_model(model)
    ! notes
END SUBROUTINE check_model


