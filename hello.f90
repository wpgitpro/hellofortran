!
! Version: 0.1.2
!
PROGRAM helloworld
USE mathModule
IMPLICIT NONE
CHARACTER*20 :: modelname
INTEGER i, j
INTEGER :: nl
INTEGER, DIMENSION(4) :: loopseq
INTEGER, DIMENSION(5) :: nv
COMPLEX :: link(20), linka, linkb
REAL, DIMENSION(5,5) :: model
LOGICAL :: lexist
!
! Common block IS1
!
! NL - The number of vector loops in the mathematical representation
!      of the Mechanism
! NV - The number of vectors in the mathematical representation
! NI - The number of independent (or input) parameters
! NCOM - The number of common vector pairs
! NC - The number of common vectors in the mathematical representation
! NUMCAS - The number of cases (i.e. the number of mechanism positions analyzed)
! NMLINK - The number of moving links in the mechanism
! NN - The number of nodes in the mechanism. These are the
!      points where vectors meet and the locations of the reaction forces
!
! Common block IA1
! LS(5,8) - Contains vector loop sequence information
! CID(10,3) - Stores information about common vector pairs
!             Primary vector, secondary vector, Angle or Length
! IN(10,2) - Stores information to identify the independent (input) parameters
!            Vector number, Angle or Length
! DEP(10,2) - Stores information to identify the dependent parameters
!             Vector number, Angle or Length
! REFDIR(20,2) - Is a reference table for all the vector directions
! REFMAG(20,2)
! LINKID(20,11)
! COMLKS(20)
! CLCON(4,20)
! PATH(20,2)
!
! Common block RA1
! Common block RA2
! Common block RA3
!
!
modelname = "model.dat"
linka = (3,4)
linkb = (4,6)
link(1) = linka + linkb
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
    WRITE(*,*) "Loop sequence (ex. 2 3 -4 -1)?"
    READ(*,*) loopseq 
end do
!
! Does model file already exist?
INQUIRE(FILE=modelname,EXIST=lexist)
IF (.NOT. lexist) THEN
    OPEN(UNIT=9,FILE=modelname,STATUS='NEW',ACTION='WRITE')
    ! Output
    ! model(1) Number of vectors in get_model
    ! model(2) Number of Loops
    ! model(3) Number of vectors in loop
    ! model(4) ncom 
    ! model(5) ninput 
    WRITE(9,*) nv(1), nl, nv(1), 0
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

subroutine grashof(lngth)
! nv vectors
!
REAL, INTENT(IN) :: lngth(4)
REAL lmax,lmin,la,lb
CHARACTER*18 mename
lmax=amax1(lngth(1),lngth(3),lngth(2),lngth(4))
lmin=amin1(lngth(1),lngth(3),lngth(2),lngth(4))
if (lngth(1).ne.lmax.and.lngth(1).ne.lmin) then
  la=lngth(1)
  if (lngth(3).ne.lmax.and.lngth(3).ne.lmin) then
    lb=lngth(3)
  else
    if(lngth(2).ne.lmax.and.lngth(2).ne.lmin) then
      lb=lngth(2)
    else
      lb=lngth(4)
    endif
  endif
else
  if (lngth(3).ne.lmax.and.lngth(3).ne.lmin) then
    la=lngth(3)
    if(lngth(2).ne.lmax.and.lngth(2).ne.lmin) then
      lb=lngth(2)
    else
      lb=lngth(4)
    endif
  else
    la=lngth(2)
    lb=lngth(4)
  endif
endif
if ((lmax+lmin).lt.(la+lb)) then
    if (lmin.eq.lngth(1)) then
      mename = 'CRANK-ROCKER'
    endif
    if (lmin.eq.lngth(4)) then
      mename = 'DRAG-LINK'
    endif
    if (lmin.eq.lngth(3)) then
      mename = 'DOUBLE-ROCKER'
    endif
    if (lmin.eq.lngth(2)) then
      mename = 'ROCKER-CRANK'
    endif
else
    if ((lmin+lmax).eq.(la+lb)) then
      mename = 'CHANGE POINT MECH.'
    endif
    if ((lmin+lmax).gt.(la+lb)) then
      mename = 'NON-GRASHOF'
    endif
endif
WRITE (*,'(A1,A6,A1,A2)') CHAR(27),'[24;1H',CHAR(27),'[K'
WRITE (*,'(A,A,A)') 'MECH. TYPE = ',mename,' <ENTER>'

read(5,*)
return
end






