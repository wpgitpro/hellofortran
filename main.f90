!
! Version: 0.1.3
! Start Date: Nov 21
!
PROGRAM MAIN
!
! IMPLICIT NONE
!
CHARACTER(LEN=20) :: modelname, mename
CHARACTER*1 :: prev
CHARACTER*1 :: mtype, Increment
!
INTEGER :: NL, NV, MAXV, ND, NC, NCOM, NINPUT, NUML
!
INTEGER, DIMENSION(2,10) :: Loop_seq
!
INTEGER :: Count, S, N, P
!
REAL(8) :: Model(250)
! REAL :: Extra(20)
! REAL :: lngth(10)
!
! REAL(8) Len, Ang
REAL(8) Len(3,20), Ang(3,20)
!
! , Real, Imag, E, Coeff, Inv_coeff, Const, Prod
!
! REAL Input(5, Ninput), Input$(Ninput)
REAL Input(5,10)
! REAL Pva(3,10)
! CHARACTER*1 :: InputType(10)
!
!
! Depend and Depend$ from GNLink
! Depend(10,N) where N is
! 1 - Vector Number
! 2 - Angle (0) or Length (1)
!
INTEGER Depend(10,2)
!
INTEGER Com_ident(10,3), Com(10)
! INTEGER Var_num
! REAL Var_increm, Var_final
INTEGER Level
!
LOGICAL :: lexist, ltest
!
! REAL(kind=8), dimension(3) :: y,z
! REAL(kind=8) :: E(10)
!
COMPLEX Polar_rect, Init_pos(20), Ec
! COMPLEX Curr_pos(20)
!
COMMON /fe1/ mename
!
!
! Model Description
!
! Model(1) Total number of vectors
! Model(2) Number of Loops
! Model(3) Max vectors in loop 
! Model(4) ncom 
! Model(5) ninput 
    ! 
    ! Link lengths in the model array start at 6 and angles start at (6 + nv) 
    ! 
    ! C = 1
    ! DO 10 N=6,Model(1)+5
    !     Len(1,C)=Model(N)
    !     Ang(1,C)=Model(N+Model(1))
    ! 10 CONTINUE
    !
    ! C = 1
    ! I=6+2*Model(1)
    ! DO 20 N=I, I+2*Model(2)-1
    !    ! Depend(C)=Model(N)
    !    IF Model(N+2*Model(2)) .GE. 0 THEN
    !       Depend(C) = "A"
    !    ELSE
    !       Depend(C) = "L"
    !    END IF
    !    C = C + 1
    ! 20 CONTINUE
    !
    ! I = I+4*Model(2)
    ! IF Model(4) .EQ. 0 THEN
    ! END IF
    !
    ! 
    !
    ! I=I+4*NL
    ! IF (NC.EQ.0) GOTO 40
    ! DO 30 N=1,NC
    !    N1=(N-1)*3+I
    !    CID(N,1)=INT(MODEL(N1))
    !    CID(N,2)=INT(MODEL(N1+1))
    !    CID(N,3)=INT(MODEL(I+3*NC+N-1))
    !    COMDIF(N)=MODEL(N1+2)
    ! 30 CONTINUE
    ! 40 I=I+4*NC
    ! DO 50 N=1,NL
    !    DO 45 R=1,8
    !       N1=I+(N-1)*8+R-1
    !       LS(N,R)=INT(MODEL(N1))
    !    45 CONTINUE
    ! 50 CONTINUE
    ! ND = INT(RESULT(2))
    ! NUMCAS = INT(RESULT(3))
    ! 
!
!
ltest = .FALSE.
!
! IF (.NOT. ltest) THEN
!
!
!
! FAME
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
! GNLink
!
! Input(N,Ninput)
! 1 - Initial Displacement
! 2 - Initial Velocity
! 3 - Initial Acceleration
! 4 - Motion Type (see below)
! 5 - Vector Number of Input
!
! Input$
! A or L for each input
! Values for 1 to Ninput
!
! Com_ident(Nc, 3) Com$(Nc) (see CID above)
!
! Pva(3,Ninput) current values for input variables
! 1 - Current Displacement
! 2 - Current Velocity
! 3 - Current Acceleration
!
! Res(1000)
!
! E(Nd)
! Coeff(Nd, Nd)
! Inv_coeff(Nd, Nd)
!
! Const(Nd, 1)
! Prod(Nd, 1)
! Loop_seq(Nl, Maxv+1)
! Depend(Nd) Depend$(Nd)
! Input(5, Ninput) Input4(Ninput)
!
modelname = "model.dat" 
WRITE(*,*) 'Has the mechanism model been previously stored on file ? (Y or N) '
READ(*,*) prev
IF (prev .EQ. 'Y') THEN
END IF
! 
WRITE(*,*) "Number Of Mechanism Loops ?"
READ(*,'(I1)') NL
WRITE(*,*) NL
!
WRITE(*,*) "Number Of Vectors ?"
READ(*,*) NV
WRITE(*,*) NV
!
IF (NL > 1) THEN
   WRITE(*,*) "Maximum Number Of Vectors In Any Loop ?"
   READ(*,*) MAXV
   WRITE(*,*) MAXV
   WRITE(*,*) "Total Number Of Common Vector Pairs ?"
   READ(*,*) NCOM
   WRITE(*,*) NCOM
   NC = NCOM
ELSE
   MAXV = NV
   NC = 1
   NUML = 2
   NCOM = 0
END IF
!
WRITE(*,*) "Number Of Dependent Variables ?"
READ(*,*) ND
WRITE(*,*) ND
!
! The number of dependent variables should equal the number of loops times 2
!
ND = 2*NL
!
WRITE(*,*) "Number Of Inputs ?"
READ(*,*) NI
WRITE(*,*) NI
!
! Do the Main_sub part
! The loop sequences are stored in Loop_seq with P+1 entries for each loop
!
!
WRITE(*,*) "***** THE LOOP VECTOR SEQUENCES MUST BE SPECIFIED *****"
DO N = 1, NL
  IF (NL .EQ. 1) THEN
    Loop_seq(N,1) = NV  
  ELSE
    WRITE(*,*) "FOR VECTOR LOOP NUMBER ", N
    WRITE(*,*) "Enter The Number Of Vectors For This Loop"
    READ(*,*) Loop_seq(N,1)
  END IF
  !
  !
  ! WRITE(*,*) "***** THE VECTOR SEQUENCES MUST BE SPECIFIED *****"
  ! Update the Model information
  !
  !
  WRITE(*,*) "Model entry ", INT(6+4*NCOM+(N-1)*(MAXV+1)+2*NV+4*NL)
  !
  Model(INT(6+4*NCOM+(N-1)*(MAXV+1)+2*NV+4*NL)) = Loop_seq(N,1)
  !
  WRITE(*,*) "Enter Each Vector Number And Sign, In Order Of Sequence"
  DO P=2, Loop_seq(N,1)+1
     Write(*,*) "Vector ", P-1, "Sequence ", N
     READ(*,*) Loop_seq(N,P)
     !
     Model(INT(5+4*NCOM+(N-1)*(MAXV+1)+2*NV+4*NL+P)) = Loop_seq(N,P)
     !
  END DO
END DO
!
!
! Vector information is now accepted for the initial position
!
!
WRITE(*,*) "PLEASE SUPPLY THE FOLLOWING FOR EACH VECTOR, FOR THE INITIAL POSITION"
DO N=1, NV
   WRITE(*,*) 'For Vector Number ', N
   WRITE(*,*) 'Vector Length ?'
   READ(*,*) Len(1,N)
   Model(5+N) = Len(1,N)
   WRITE(*,*) 'Vector Angle ?'
   READ(*,*) Ang(1,N)
   Model(5+NV+N) = Ang(1,N)
   ! Set initial position in complex coordinates
   Init_pos(N) = Polar_rect(Len(1,N), Ang(1,N))
END DO
!
!
! Common Variables Are Identified In The Following Block
! (For Multi-Loop Mechanisms Only)
!
!
IF (NL .GT. 1) THEN
  DO N=1, NCOM
    WRITE(*,*) "COMMON VECTOR PAIRS MUST NOW BE IDENTIFIED"
    WRITE(*,*) "Enter The Primary Vector Number"
    READ(*,*) Com_ident(N,1)
    Model(6+2*NV+4*NL+(N-1)*3)=Com_ident(N,1)
    WRITE(*,*) "Enter The Common Vector Number"
    READ(*,*) Com_ident(N,2)
    Model(7+2*NV+4*NL+(N-1)*3)=Com_ident(N,1)  
    WRITE(*,*) "Are The Variables Angles Or Lengths ? (A or L)"   
    READ(*,*) Com(3)
    WRITE(*,*) "Enter The Difference Value"
    READ(*,*) Com_ident(N,3)
    Model(8+2*Nv+4*Nl+(N-1)*3)=Com_ident(N,3)
  END DO
END IF
!
!
! There are two dependent variables for each mechanism loop. Course 25.212
!
!
WRITE(*,*) "PLEASE SUPPLY THE FOLLOWING INFORMATION FOR EACH DEPENDENT VARIABLE"
!
DO N=1,Nd  
  WRITE(*,*) "FOR DEPENDENT VARIABLE NUMBER", N
  WRITE(*,*) "Variable's Vector Number ?"
  READ(*,*) Depend(N,1)
  Model(5+2*Nv+N)=Depend(N,1)
  WRITE(*,*) "Is The Variable An Angle Or A Length ? (A or L)"
  READ(*,*) mtype
  IF (mtype .EQ. "A") THEN
     Depend(N,2) = 0
  ELSE IF (mtype .EQ. "L") THEN
     Depend(N,2) = 1
  ELSE
     Depend(N,2) = 0
  END IF
  Model(5+2*Nv+2*Nl+N)=Depend(N,2)
END DO
!
! Loop Connection Is Determined in SUB Loop_con
!
! Insert code for Loop_con here
!
! Loop_con(Loop_con(*), Ls(*), Com_ident(*), Com$(*), Nl, Ncom)
!
IF (Nl .GT. 1) THEN
!
   Count = 1
   S = 1
!
   DO N=S, Nl  
   END DO
!   
END IF
!
!
! Print Model CALL Print_model
!
! Hard Copy?
!
! Modify model?
!
! Store model
!
!
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
    WRITE(9,*) NV, NL, MAXV, NCOM, NINPUT
    CLOSE(9,STATUS='KEEP')
END IF
!
! Input variable motions:
!
! 1 - Constant Velocity
! 2 - Constant Acceleration
! 3 - User Defined
! 4 - User Defined
! 5 - User Defined 
! 6 - User Defined
! 7 - User Defined
!
!
IF (.NOT. ltest) THEN
DO N=1, Ninput
   IF (N .EQ. 1) THEN
      WRITE(*,*) "THE FOLLOWING INPUT VARIABLE MOTIONS ARE AVAILABLE:"
      WRITE(*,*) "         MOTION NUMBER               MOTION TYPE   "
      WRITE(*,*) "               1                     Constant Velocity"
      WRITE(*,*) "               2                     Constant Acceleration"
      WRITE(*,*) "               3                     User Defined Function #1"    
      WRITE(*,*) "               4                     User Defined Function #2"      
      WRITE(*,*) "               5                     User Defined Function #3"      
      WRITE(*,*) "               6                     User Defined Function #4"
      WRITE(*,*) "               7                     User Defined Function #5"
   END IF
   IF (Ninput .EQ. 1) THEN
      WRITE(*,*) "FOR THE INPUT VARIABLE"
   ELSE
      WRITE(*,*) "FOR THE INPUT VARIABLE ", N
   END IF
   WRITE(*,*) "What Is The Vector Number Of The Variable?"
   READ(*,*) Input(N,1)
   WRITE(*,*) "Is The Variable An Angle Or A Length? (A or L)"
   READ(*,*) mtype
   IF (mtype .EQ. "A") THEN
     Input(N,2) = 0
   ELSE IF (mtype .EQ. "L") THEN
      Input(N,2) = 1
   ELSE
      Input(N,2) = 0
   END IF
   WRITE(*,*) "Please Enter The Motion Type For This Variable (Answer By Number Only)"
   READ(*,*) Input(N,3)
   !
   ! Input(4,N) = 1
   ! IF (Input(4,N) .NE. 1) THEN
   !    WRITE(*,*) " What Is The Variable's Initial Velocity, Vo?"
   !    READ(*,*) Input(2,N)
   ! ELSE
   !    WRITE(*,*) "What Is the Variable's Constant Velocity?"
   !    READ(*,*) Pva(2,N)
   !    Pva(3,N) = 0
   !    Input(3,N) = 0
   ! END IF
   ! IF (Input(4,N) .NE. 2) THEN
   !    WRITE(*,*) "What Is The Constant Acceleration Value?"
   !    READ(*,*) Pva(3,N)
   ! ELSE
   !    WRITE(*,*) "What Is The Initial Acceleration Value?"
   !    READ(*,*) Pva(3,N)
   ! END IF
   ! IF (InputType(N) .EQ. "L") THEN
   !    Input(1,N) = Len(1,INT(Input(5,N)))
   ! ELSE
   !    Input(1,N) = Ang(1,INT(Input(5,N)))
   ! END IF
   !
END DO
!
! Checking To See If All Inputs Are Constant Velocity
!
! WRITE(*,*) "DO YOU WANT TO INCREMENT THE INPUT VARIABLE, OR THE TIME? (I or T)"
! READ(*,*) Increment
!
! WRITE(*,*) "WHICH INPUT VARIABLE DO YOU WANT TO INCREMENT? (Answer By Number Only)"
! READ(*,*) Var_num
!
! WRITE(*,*) "Increment Of Input Variable?"
! READ(*,*) Var_increm
!
! WRITE(*,*) "What Is The Final Variable Value?"
! READ(*,*) Var_final
!
! WRITE(*,*) "Do You Wish To Correct Any Of The Input Variable Information?"
! READ(*,*) Yorn
!
CALL Closure(Loop_seq, Init_pos, 1, Ec)
!
Level = 0
!
IF (Increment .EQ. "I") THEN
END IF
!
END IF
!
WRITE(*,*) "End of program"
STOP 0 
END PROGRAM MAIN



SUBROUTINE Closure(Loop_seq, Init_pos, Loop, Ec)
   INTEGER, DIMENSION(2,10), INTENT(IN) :: Loop_seq
   INTEGER, INTENT(IN) :: Loop
   COMPLEX, DIMENSION(20), INTENT(IN) :: Init_pos
   COMPLEX, INTENT(INOUT) :: Ec
   INTEGER :: Vec, Dir
   ! Loop = 1
   DO N=2, Loop_seq(Loop,1)+1
      Vec = ABS(Loop_seq(1,N))
      IF (Loop_seq(1,N) .GT. 0) THEN
         Dir = 1
      ELSE
         Dir = -1
      END IF
      Ec = Ec + Init_pos(Vec)*Dir
   END DO
   WRITE(*,*) Ec
END SUBROUTINE Closure

COMPLEX FUNCTION Polar_rect(inputlen, inputang)
   REAL(8), INTENT(IN) :: inputlen, inputang
   REAL(8), PARAMETER :: pi = 3.141592653589793D0
   Polar_rect = cmplx(inputlen * COS(inputang*pi/180), inputlen * SIN(inputang*pi/180))
END FUNCTION

SUBROUTINE grashof(lngth)
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
