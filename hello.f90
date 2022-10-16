!
! Version: 0.1.2
!
PROGRAM helloworld
USE gnlink
USE fame
IMPLICIT NONE
CHARACTER*20 :: modelname, mename
CHARACTER*1 :: prev
INTEGER i, j
INTEGER intval
INTEGER :: nl, nv, maxv, nd, ndepend, nc, ncom, ninput, numl
INTEGER, DIMENSION(2,10) :: Loop_seq
COMPLEX :: link(20), linka, linkb
REAL, DIMENSION(250) :: MODEL
REAL, DIMENSION(20) :: EXTRA

LOGICAL :: lexist

real(kind=8), dimension(3) :: y,z

COMMON /fe1/mename

! y = (/2., 3., 4./)
! call fsub(y,z)
! print *, "z = ",z

!
! MAXV - Maximum number of vectors in a loop
!        Set to NV if single loop mechanism
!
! Common block IS1
!
! NV - The number of vectors in the mathematical representation
! NL - The number of vector loops in the mathematical representation
!      of the Mechanism
! MODEL(3)
! NC - The number of common vectors in the mathematical representation
! NI - The number of independent (or input) parameters
!
! NCOM - The number of common vector pairs

! NUMCAS - The number of cases (i.e. the number of mechanism positions analyzed)
! NMLINK - The number of moving links in the mechanism
!
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
WRITE(*,*) 'Has the mechanism model been previously stored on file ? (Y or N) '
READ(*,*) prev
IF (prev .EQ. 'Y') THEN
END IF
! 
WRITE(*,*) "Number Of Mechanism Loops ?"
READ(*,'(I1)') nl
WRITE(*,*) nl
!
WRITE(*,*) "Number Of Vectors ?"
READ(*,*) nv
WRITE(*,*) nv
IF (nl > 1) THEN
   WRITE(*,*) "Maximum Number Of Vectors In Any Loop ?"
   READ(*,*) maxv
   WRITE(*,*) maxv
   WRITE(*,*) "Total Number Of Common Vector Pairs ?"
   READ(*,*) ncom
   WRITE(*,*) ncom
   nc = ncom
else
   maxv = nv
   nc = 1
   numl = 2
END IF
WRITE(*,*) "Number Of Dependent Variables ?"
READ(*,*) ndepend
WRITE(*,*) ndepend
nd = 2*nl  
WRITE(*,*) "Number Of Inputs ?"
READ(*,*) ninput 
WRITE(*,*) ninput

! WRITE(*,*) "Maximum number of vectors in any loop ?"
! READ(*,'(I1)') maxv
do i = 1, nl
    ! WRITE(*,*) "Number of Links in Loop ?", i
    ! READ(*,'(I1)') nv
    ! do j = 1, nv
    !     WRITE(*,*) "Length of link ", j, "?"
    !     READ(*,*) model(j) 
    !     WRITE(*,*) "Link ", j, " is ", model(j)
    ! end do
    !
    ! THE LOOP SEQUENCE MUST BE SPECIFIED
    !
    WRITE(*,*) "Loop sequence for loop ", i, " (ex. 2 3 -4 -1) ?"
    READ(*,'(I4)') Loop_seq(i,:)
    WRITE(*,*) Loop_seq(i,:)
end do

! Number of dependent variables equals 2 times the number of loops
nd = nl * 2

!
! Does model file already exist?
! Save it
!
INQUIRE(FILE=modelname,EXIST=lexist)
IF (.NOT. lexist) THEN
    OPEN(UNIT=9,FILE=modelname,STATUS='NEW',ACTION='WRITE')
    ! Output
    ! model(1) Number of vectors in get_model
    ! model(2) Number of Loops
    ! model(3) Max vectors in loop
    ! model(4) ncom 
    ! model(5) ninput 
    WRITE(9,*) nv, nl, nv, ncom, ninput
    CLOSE(9,STATUS='KEEP')
END IF
!
! Vector information is now accepted for the initial position
!
! do i=1, nv
!   WRITE(*,*) 'For Vector Number ', I
!   WRITE(*,*) 'Vector Length ?'
!   READ(*,*) LEN(1,i)
!   MODEL(5 + i) = LEN(1,i)
!   WRITE(*,*) 'Vector Angle ?'
!   READ(*,*) ANG(1,i)
!   MODEL(5 + i + nv) = ANG(1,i)
! end do

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
! nv - number of vectors
!
REAL, INTENT(IN) :: lngth(4)
REAL lmax,lmin,la,lb
CHARACTER*20 mename
COMMON /fe1/mename

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
