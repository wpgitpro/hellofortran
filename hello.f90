!
! Version: 0.1.2
!
PROGRAM helloworld
IMPLICIT NONE
CHARACTER*20 :: modelname, mename
CHARACTER*1 :: prev
CHARACTER*1 :: dtype, Increment, Yorn
INTEGER i, j
INTEGER intval
INTEGER :: Nl, Nv, maxv, Nd, nDepend, Nc, Ncom, Ninput, numl, Paper
INTEGER, DIMENSION(2,10) :: Loop_seq
INTEGER Count, S, N, P
COMPLEX :: linka
REAL, DIMENSION(250) :: Model
REAL, DIMENSION(20) :: Extra

! REAL Len, Ang, Real, Imag, E, Coeff, Inv_coeff, Const, Prod
REAL Len(3,20), Ang(3,20)

! REAL Input(5, Ninput), Inpunt$(Ninput)
REAL Input(5,10), InputType(10), Pva(3,10)

!
! Depend and Depend$ from GNLink
INTEGER Depend(10), DependType(10)
INTEGER Com_ident(10,3), Com(10)
INTEGER Var_num
REAL Var_increm, Var_final
INTEGER Level

LOGICAL :: lexist, ltest

REAL(kind=8), dimension(3) :: y,z

COMPLEX Polar_rect

COMMON /fe1/ mename

ltest = .TRUE.

linka = Polar_rect(5.0,60.0)
WRITE(*,*) linka

IF (.NOT. ltest) THEN

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

! Input
! 1 - Initial Displacement
! 2 - Initial Velocity
! 3 - Initial Acceleration
! 4 - Motion Type (see below)
! 5 - Vector Number of Input
!
! Input4
! A or L for each input
! Values for 1 to Ninput


! Com_ident (see CID above)
!
! Pva(3,Ninput)
! 1 - Current Displacement
! 2 - Current Velocity
! 3 - Current Acceleration
!
! Res(1000)

modelname = "model.dat" 
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
READ(*,*) Ndepend
WRITE(*,*) Ndepend
nd = 2*nl  

WRITE(*,*) "Number Of Inputs ?"
READ(*,*) Ninput 
WRITE(*,*) Ninput

! Do the Main_sub part

WRITE(*,*) "***** THE LOOP VECTOR SEQUENCES MUST BE SPFCIFIED *****"
DO N = 1, NL
  IF (Nl .EQ. 1) THEN
    Loop_seq(N,1) = Nv   
  ELSE
    WRITE(*,*) "FOR VECTOR LOOP NUMBER ", N
    WRITE(*,*) "Enter The Number Of Vectors For This Loop"
    READ(*,*) Loop_seq(N,1)
  END IF
  ! Update the Model information
  Model(6+4*Ncom+(N-1)*(Maxv+1)+2*Nv+4*Nl) = Loop_seq(N,1)
  WRITE(*,*) "Enter Each Vector Number And Sign In Order Of Sequence"
  DO P=2, Loop_seq(N,1)+1
    Write(*,*) "Vector ", P-1, "Sequence ", N
    READ(*,*) Loop_seq(N,P)
    Model(5+4*Ncom+(N-1)*(Maxv+1)+2*Nv+4*Nl+P) = Loop_seq(N,P)
  END DO
END DO
!
! Vector information is now accepted for the initial position
!
WRITE(*,*) "PLEASE SUPPLY THE FOLLOWING FOR EACH VECTOR, FOR THE INITIAL POSITION"
DO N=1, Nv
   WRITE(*,*) 'For Vector Number ', N
   WRITE(*,*) 'Vector Length ?'
   READ(*,*) Len(1,N)
   Model(5+N) = Len(1,N)
   WRITE(*,*) 'Vector Angle ?'
   READ(*,*) Ang(1,N)
   Model(5+Nv+N) = Ang(1,N)
END DO
!
! Common Variables Are Identified In The Following Block
!
IF (Nl .GT. 1) THEN
  DO N=1, Ncom
    WRITE(*,*) "COMMON VECTOR PAIRS MUST NOW BE IDENTIFIED"
    WRITE(*,*) "Enter The Primary Vector Number"
    READ(*,*) Com_ident(N,1)
    Model(6+2*Nv+4*Nl+(N-1)*3)=Com_ident(N,1)
    WRITE(*,*) "Enter The Common Vector Number"
    READ(*,*) Com_ident(N,2)
    Model(7+2*Nv+4*Nl+(N-1)*3)=Com_ident(N,1)  
    WRITE(*,*) "Are The Variables Angles Or Lengths ? (A or L)"   
    READ(*,*) Com(3)
    WRITE(*,*) "Enter The Difference Value"
    READ(*,*) Com_ident(N,3)
    Model(8+2*Nv+4*Nl+(N-1)*3)=Com_ident(N,3)
  END DO
END IF

WRITE(*,*) "PLEASE SUPPLY THE FOLLOWING INFORMATION FOR EACH DEPENDENT VARIABLE"
DO N=1,Nd
  WRITE(*,*) "FOR DEPENDENT VARIABLE NUMBER",N
  WRITE(*,*) "Variable's Vector Number ?"
  READ(*,*) Depend(N)
  Model(5+2*Nv+N)=Depend(N)
  WRITE(*,*) "Is The Variable An Angle Or A Length ? (A or L)"
  READ(*,*) dtype
  IF (dtype .EQ. "A") THEN
     DependType(N) = 0
  ELSE IF (dtype .EQ. "L") THEN
     DependType(N) = 1
  ELSE
     DependType(N) = 0
  END IF
  Model(5+2*Nv+2*Nl+N)=DependType(N)
END DO

! Loop Connection Is Determined in SUB Loop_con

! Insert code for Loop_con here

! Loop_con(Loop_con(*), Ls(*), Com_ident(*), Com$(*), Nl, Ncom)

IF (Nl .GT. 1) THEN

   Count = 1
   S = 1

   DO N=S, Nl
   
   END DO
   
END IF


! Print Model CALL Print_model

! Hard Copy?

! Modify model?

! Store model


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

! Input types:
! 1 - Constant Velocity
! 2 - Constant Acceleration
! 3 - User Defined
! 4 - User Defined
! 5 - User Defined 
! 6 - User Defined
! 7 - User Defined
!

DO N=1, Ninput
   IF (Ninput .EQ. 1) THEN
      WRITE(*,*) "FOR THE INPUT VARIABLE"
   ELSE
      WRITE(*,*) "FOR THE INPUT VARIABLE ", N
   END IF
   WRITE(*,*) "What Is The Vector Number Of The Variable?"
   READ(*,*) Input(5,N)
   WRITE(*,*) "Is The Variable An Angle Or A Length? (A or L)"
   READ(*,*) InputType(N)
   WRITE(*,*) "Please Enter The Motion Type For This Variable (Answer By Number Only)"
   READ(*,*) Input(4,N)
   Input(4,N) = 1
   IF (Input(4,N) .NE. 1) THEN
      WRITE(*,*) " What Is The Variable's Initial Velocity, Vo?"
      READ(*,*) Input(2,N)
   ELSE
      WRITE(*,*) "What Is the Variable's Constant Velocity?"
      READ(*,*) Pva(2,N)
      Pva(3,N) = 0
      Input(3,N) = 0
   END IF
   IF (Input(4,N) .NE. 2) THEN
      WRITE(*,*) "What Is The Constant Acceleration Value?"
      READ(*,*) Pva(3,N)
   ELSE
      WRITE(*,*) "What Is The Initial Acceleration Value?"
      READ(*,*) Pva(3,N)
   END IF
   
END DO

! Checking To See If All Inputs Are Constant Velocity

WRITE(*,*) "DO YOU WANT TO INCREMENT THE INPUT VARIABLE, OR THE TIME? (I or T)"
READ(*,*) Increment

WRITE(*,*) "WHICH INPUT VARIABLE DO YOU WANT TO INCREMENT? (Answer By Number Only)"
READ(*,*) Var_num

WRITE(*,*) "Increment Of Input Variable?"
READ(*,*) Var_increm

WRITE(*,*) "What Is The Final Variable Value?"
READ(*,*) Var_final

WRITE(*,*) "Do You Wish To Correct Any Of The Input Variable Information?"
READ(*,*) Yorn

Level = 0

IF (Increment .EQ. "I") THEN

END IF

END IF

WRITE(*,*) "End of program"
STOP 0 
END PROGRAM helloworld

SUBROUTINE Mod_sub
END SUBROUTINE Mod_sub

SUBROUTINE Closure(Nd)
   REAL, DIMENSION(Nd) :: E 
END SUBROUTINE Closure

COMPLEX FUNCTION Polar_rect(inputlen, inputang)
   REAL, INTENT(IN) :: inputlen, inputang
   REAL(kind=8), PARAMETER :: pi = 3.141592653589793d0
   Polar_rect = cmplx(inputlen * COS(inputang*pi/180), inputlen * SIN(inputang*pi/180))
END FUNCTION

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
