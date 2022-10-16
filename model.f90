module model

    ! Model
    !
    ! model(1) Number of vectors in get_model
    ! model(2) Number of Loops
    ! model(3) Max vectors in loop
    ! model(4) ncom 
    ! model(5) ninput 
    ! 
    ! Link lengths in the model array start at 6 and angles start at (6 + nv) 
    ! 
    ! DO 10 N=6,NV+5
    !     LENGTH(N-5)=MODEL(N)
    !     ANGLE(N-5)=MODEL(N+NV)
    ! 10 CONTINUE
    !
    !
    !     I=6+2*NV
    !  NF=I+2*NL-1
    !  DO 20 N=I,NF
    !      C=N-(5+2*NV)
    !      DEP(C,1)=INT(MODEL(N))
    !      DEP(C,2)=INT(MODEL(N+2*NL))
    ! 20 CONTINUE
    !  I=I+4*NL
    !  IF (NC.EQ.0) GOTO 40
    !  DO 30 N=1,NC
    !      N1=(N-1)*3+I
    !      CID(N,1)=INT(MODEL(N1))
    !      CID(N,2)=INT(MODEL(N1+1))
    !      CID(N,3)=INT(MODEL(I+3*NC+N-1))
    !      COMDIF(N)=MODEL(N1+2)
    ! 30 CONTINUE
    ! 40 I=I+4*NC
    !  DO 50 N=1,NL
    !      DO 45 R=1,8
    !           N1=I+(N-1)*8+R-1
    !          LS(N,R)=(IN)T(MODEL(N1))
    ! 45     CONTINUE
    ! 50 CONTINUE
    !  ND = INT(RESULT(2))
    ! NUMCAS = INT(RESULT(3))
    ! 

contains

end module model