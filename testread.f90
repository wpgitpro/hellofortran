      SUBROUTINE READGN (R)
      INTEGER I,N,C,NF,R,L,N1
      REAL MODEL(250),EXTRA(20)
      CHARACTER NAME*20,AAA*1
      LOGICAL VAR
      COMMON/REA/I,N,C,NF,L,MODEL,EXTRA,N1
      COMMON/CGN/NAME,AAA
      COMMON/LGN/VAR
      I = 0
    1 WRITE(*,2)
    2 FORMAT(/,1x,'Enter Model File Name: ',\)
      READ(*,3) NAME
    3 FORMAT(BN,A)
      INQUIRE(FILE=NAME,EXIST=VAR)
      IF (VAR) THEN
          OPEN(3,FILE=NAME,FORM='UNFORMATTED')
          READ(3) MODEL
          READ(3) EXTRA
          CLOSE(3,STATUS='KEEP')
      ELSE 
          WRITE(*,'(1X,A,A,A)') 'File ',NAME,' Not Found.'
          WRITE(*,'(1X,A)') '<T> to retry'
          WRITE(*,'(1X,A)') '<R> to return to Master Menu'
   23     CALL GETKEY(I,AAA,N,C)
          IF (N.EQ.20) THEN
              GOTO 1
          ELSE IF (N.EQ.19) THEN
              R = 1
              GOTO 999
          ELSE 
              GOTO 23
          END IF
      END IF
    6 WRITE(*,4)
    4 FORMAT(1x,'Enter Kinematic Results File Name : ',\)
      READ(*,3) NAME
      INQUIRE(FILE=NAME,EXIST=VAR)
      IF (VAR) THEN
          OPEN(4,FILE=NAME,FORM='UNFORMATTED')
          READ(4) RESULT
          CLOSE(4,STATUS='KEEP')
      ELSE 
          WRITE(*,'(1X,A,A,A)') 'File ',NAME,' Not Found.'
          WRITE(*,'(1X,A)') '<T> to retry'
          WRITE(*,'(1X,A)') '<R> to return to Master Menu'
   24     CALL GETKEY(I,AAA,N,C)
          IF (N.EQ.20) THEN
              GOTO 6
          ELSE IF (N.EQ.19) THEN
              R = 1
              GOTO 999
          ELSE 
              GOTO 24
          END IF
      END IF
      DO 5 I = 1,10
         IN(I,1) = INT(EXTRA(I*2-1))
         IN(I,2) = INT(EXTRA(I*2))
    5 CONTINUE
      NV=INT(MODEL(1))
      NL=INT(MODEL(2))
      NC=INT(MODEL(4))
      NI=INT(MODEL(5))
      DO 10 N=6,NV+5
          LENGTH(N-5)=MODEL(N)
          ANGLE(N-5)=MODEL(N+NV)
   10 CONTINUE
      I=6+2*NV
      NF=I+2*NL-1
      DO 20 N=I,NF
          C=N-(5+2*NV)
          DEP(C,1)=INT(MODEL(N))
          DEP(C,2)=INT(MODEL(N+2*NL))
   20 CONTINUE
      I=I+4*NL
      IF (NC.EQ.0) GOTO 40
      DO 30 N=1,NC
          N1=(N-1)*3+I
          CID(N,1)=INT(MODEL(N1))
          CID(N,2)=INT(MODEL(N1+1))
          CID(N,3)=INT(MODEL(I+3*NC+N-1))
          COMDIF(N)=MODEL(N1+2)
   30 CONTINUE
   40 I=I+4*NC
      DO 50 N=1,NL
          DO 45 R=1,8
              N1=I+(N-1)*8+R-1
              LS(N,R)=INT(MODEL(N1))
   45     CONTINUE
   50 CONTINUE
      ND = INT(RESULT(2))
      NUMCAS = INT(RESULT(3))
  999 RETURN
      END
C
      SUBROUTINE REFTAB                  
      INTEGER I,J
      COMMON/RTB/I,J
      DO 20 I = 1,20
          MASS(I) = 0.0
          CENTRO(I,1) = 0.0
          CENTRO(I,2) = 0.0
          POLAR(I) = 0.0
   20 CONTINUE
      DO 100 I = 1,NV
           REFDIR(I,1) = 0
           REFDIR(I,2) = 0
           REFMAG(I,1) = 0
           REFMAG(I,2) = 0
  100 CONTINUE
      DO 200 I = 1,NI
           IF(IN(I,2).EQ.0) THEN
                REFDIR(IN(I,1),1) = 1
                REFDIR(IN(I,1),2) = I
           ELSE
                REFMAG(IN(I,1),1) = 1
                REFMAG(IN(I,1),2) = I
           END IF
  200 CONTINUE
      DO 300 I = 1,ND
           IF(DEP(I,2).EQ.0) THEN
                REFDIR(DEP(I,1),1) = 2
                REFDIR(DEP(I,1),2) = I
           ELSE
                REFMAG(DEP(I,1),1) = 2
                REFMAG(DEP(I,1),2) = I
           END IF
  300 CONTINUE
      DO 600 I = 1,NC
           DO 400 J = 1,NI
                IF(IN(J,1).EQ.CID(I,1)) THEN
                     IF(CID(I,3).EQ.0) THEN
                          REFDIR(CID(I,2),1) = 3
                          REFDIR(CID(I,2),2) = J
                     ELSE
                          REFMAG(CID(I,2),1) = 3
                          REFMAG(CID(I,2),2) = J
                     END IF
                     GOTO 600
                END IF
  400      CONTINUE
           DO 500 J = 1,ND
                IF(DEP(J,1).EQ.CID(I,1)) THEN
                     IF(CID(I,3).EQ.0) THEN
                          REFDIR(CID(I,2),1) = 4
                          REFDIR(CID(I,2),2) = J
                     ELSE
                          REFMAG(CID(I,2),1) = 4
                          REFMAG(CID(I,2),2) = J
                     END IF
                     GOTO 600
                END IF
  500      CONTINUE
  600 CONTINUE
      RETURN
      END 
C
      SUBROUTINE IDLINK 
      INTEGER I,J,K
      COMMON/IDL/I,J,K
      NMLINK = 0
      DO 50 I = 1,20
          DO 45 J = 1,11
              LINKID(I,J) = 0
   45     CONTINUE
   50 CONTINUE
      DO 300 I = 1,NI
          NMLINK = NMLINK + 1
          LINKID(NMLINK,1) = IN(I,1)
          DO 100 J = I+1,NI
              IF(IN(J,1).EQ.IN(I,1)) THEN
                  NMLINK = NMLINK - 1
                  GOTO 300
              END IF
  100     CONTINUE
          DO 200 J = 1,ND
              IF(DEP(J,1).EQ.IN(I,1)) THEN 
                  NMLINK = NMLINK - 1
                  GOTO 300
              END IF
  200     CONTINUE
  300 CONTINUE
      DO 500 I = 1,ND
          NMLINK = NMLINK + 1
          LINKID(NMLINK,1) = DEP(I,1)
          DO 400 J = I+1,ND
              IF(DEP(J,1).EQ.DEP(I,1)) THEN 
                  NMLINK = NMLINK - 1
                  IF(J.EQ.ND) LINKID(NMLINK+1,1) = 0
                  GOTO 500
              END IF
  400     CONTINUE
  500 CONTINUE
      K = 0
      DO 700 I = 1,NMLINK
          DO 600 J = 1,NC
              IF(CID(J,1).EQ.LINKID(I,1)) THEN
                  K = K + 1
                  LINKID(I,2) = LINKID(I,2) + 1
                  COMLKS(K) = J
              END IF
  600     CONTINUE
  700 CONTINUE
      RETURN
      END
C
      SUBROUTINE NODID 
      INTEGER COUNT,VN,PN,IC,TP(5,8),SAME(20,6),COM(6),COMHT(6),
     & FLAG,VECTOR,HORT,NODES(20,5),CURENT,NEXT,CURHT,NEXTHT,I,IA,
     & IB,J,K,L,M,N,NUMCOM
      REAL D
      COMMON/NOD/COUNT,VN,PN,IC,TP,SAME,COM,COMHT,FLAG,VECTOR,HORT,
     & CURENT,NEXT,CURHT,NEXTHT,I,IA,IB,J,K,L,M,N,NUMCOM,NODES,D
      DO 30 I = 1,20
          DO 20 J = 1,6
              SAME(I,J) = 0
              IF(J.LT.6) NODES(I,J) = 0
   20     CONTINUE
   30 CONTINUE
      DO 50 I = 1,NL
          DO 40 J = 1,LS(I,1)+1
              TP(I,J) = LS(I,J)
   40     CONTINUE
   50 CONTINUE
      COUNT = 0
      DO 1000 I = 1,NMLINK
          VN = LINKID(I,1)                                              THERE IS ALWAYS A 
          LINKID(I,3) = 1                                               NODE AT THE HEAD OF
          LINKID(I,4) = VN                                              OF THE LINK'S PRIMARY
          LINKID(I,5) = 1                                               VECTOR
          PRIMNO(I) = 1
          IF(REFDIR(VN,1).GT.0) THEN                                    THEN PRIMARY ALSO ROTATES
              LINKID(I,3) = 2                                           AND HAS A NODE AT TAIL
              LINKID(I,6) = VN
              LINKID(I,7) = -1
              PRIMNO(I) = 2
          END IF
          IF(LINKID(I,2).GT.0) THEN                                     THERE ARE SOME COMMON VECTORS
              DO 500 J = 1,LINKID(I,2)
                  PN = COMLKS(COUNT+J)                                  PAIR NUMBER
                  IF(CID(PN,3).EQ.0) THEN                               ANGLES ARE COMMON COMPONENTS 
                      D = ABS((LENGTH(VN)-LENGTH(CID(PN,2)))/LENGTH(VN))
                      IF(COMDIF(PN).NE.0.0.OR.D.GT.0.00001) THEN        PAIR HAS DIFFERENCE IN ANGLE OR
                          LINKID(I,3) = LINKID(I,3) + 1                 DIFFERENCE IN LENGTH
                          IC = 2*LINKID(I,3)                       
                          LINKID(I,IC+2) = CID(PN,2)
                          LINKID(I,IC+3) = 1
                      ELSE                                              ELSE VECTORS ARE IDENTICAL AND
                          DO 300 IA = 2,NL                              TP MUST BE CHANGED FOR LATTER USE
                              DO 200 IB = 2,TP(IA,1)+1
                                  IF(IABS(TP(IA,IB)).EQ.CID(PN,2)) THEN
                                      TP(IA,1) = TP(IA,1) - 1
                                      DO 100 IC = IB,TP(IA,1)+1
                                          TP(IA,IC) = TP(IA,IC+1)
  100                                 CONTINUE
                                      TP(IA,IC) = 0
                                  END IF
  200                         CONTINUE
  300                     CONTINUE
                          SAME(VN,1) = SAME(VN,1) + 1
                          SAME(VN,SAME(VN,1)+1) = CID(PN,2)
                      END IF
                  ELSE
                      IF(LENGTH(VN).NE.LENGTH(CID(PN,2))) THEN          COMMON LEN BUT NOT SAME LEN
                          LINKID(I,3) = LINKID(I,3) + 1
                          IC = 2*LINKID(I,3)                       
                          LINKID(I,IC+2) = CID(PN,2)
                          LINKID(I,IC+3) = 1
                      ELSE IF(ANGLE(VN).EQ.ANGLE(CID(PN,2))) THEN       VECTORS ARE IDENTICAL
                          DO 330 IA = 2,NL                              
                              DO 320 IB = 2,TP(IA,1)+1
                                  IF(IABS(TP(IA,IB)).EQ.CID(PN,2)) THEN
                                      TP(IA,1) = TP(IA,1) - 1
                                      DO 310 IC = IB,TP(IA,1)+1
                                          TP(IA,IC) = TP(IA,IC+1)
  310                                 CONTINUE
                                      TP(IA,IC) = 0
                                  END IF
  320                         CONTINUE
  330                     CONTINUE
                          SAME(VN,1) = SAME(VN,1) + 1
                          SAME(VN,SAME(VN,1)+1) = CID(PN,2)
                      END IF
                  END IF
  500         CONTINUE
              COUNT = COUNT + LINKID(I,2)
          END IF
 1000 CONTINUE
      DO 1008 I = 1,20
          IF(SAME(I,1).GT.1) THEN
              DO 1007 J = 1,SAME(I,1)
                  DO 1006 K = J+1,SAME(I,1)
                      IF(SAME(I,K+1).EQ.SAME(I,J+1)) THEN
                          DO 1005 L = K+1,SAME(I,1)+1
                              SAME(I,L) = SAME(I,L+1)
 1005                     CONTINUE
                          SAME(I,1) = SAME(I,1) - 1
                          GOTO 1008
                      END IF
 1006             CONTINUE
 1007         CONTINUE
          END IF
 1008 CONTINUE
      DO 1014 N = 1,20
          IF(SAME(N,1).GT.0) THEN
              DO 1013 M = 2,SAME(N,1)+1
                  DO 1012 I = 2,NL
                      DO 1011 J = 2,LS(I,1)+1
                          IF(IABS(LS(I,J)).EQ.SAME(N,M)) THEN
                              SAME(N,M) = LS(I,J+1)
                              GOTO 1013
                          END IF
 1011                 CONTINUE
 1012             CONTINUE
 1013         CONTINUE
          END IF
 1014 CONTINUE
      NN = 1
      DO 2000 I = 1,NL
          DO 1800 J = 1,TP(I,1)
              FLAG = 0
              NUMCOM = 0
              IF(J.EQ.1) THEN
                  IF(I.EQ.1) THEN
                      CURENT = TP(1,TP(1,1)+1)
                      NEXT = TP(1,2)
                      GOTO 1050
                  ELSE
                      GOTO 1800
                  END IF
              END IF
              CURENT = TP(I,J)
              NEXT = TP(I,J+1)
 1050         IF(CURENT.GT.0) THEN
                  CURHT = 1
              ELSE 
                  CURHT = -1
              END IF
              IF(NEXT.GT.0) THEN
                  NEXTHT = -1
              ELSE
                  NEXTHT = 1
              END IF
              IF(CURENT.GT.0) THEN
                  NUMCOM = SAME(CURENT,1)
              END IF
              DO 1100 K = 1,NUMCOM
                  COM(K) = SAME(CURENT,K+1)
                  IF(COM(K).GT.0) THEN
                      COMHT(K) = -1
                  ELSE
                      COMHT(K) = 1
                  END IF
                  COM(K) = IABS(COM(K))
 1100         CONTINUE
              IF(NEXT.LT.0) THEN
                  NUMCOM = NUMCOM + SAME(IABS(NEXT),1)
                  DO 1150 L = K,NUMCOM
                      COM(L) = SAME(IABS(NEXT),L+1)
                      IF(COM(L).GT.0) THEN
                          COMHT(L) = -1
                      ELSE
                          COMHT(L) = 1
                      END IF
                      COM(L) = IABS(COM(L))
 1150             CONTINUE
              END IF
              CURENT = IABS(CURENT)
              NEXT   = IABS(NEXT)
              DO 1500 K = 1,NMLINK
                  DO 1400 L = 1,LINKID(K,3)
                      VECTOR = LINKID(K,L*2+2)
                      HORT  =  LINKID(K,L*2+3)
                      IF(VECTOR.EQ.CURENT.AND.HORT.EQ.CURHT) THEN
                          FLAG = 1
                          NODES(K,1) = NODES(K,1) + 1
                          NODES(K,NODES(K,1)+1) = NN
                          GOTO 1400
                      END IF
                      IF(VECTOR.EQ.NEXT.AND.HORT.EQ.NEXTHT) THEN
                          FLAG = 1
                          NODES(K,1) = NODES(K,1) + 1
                          NODES(K,NODES(K,1)+1) = NN
                          GOTO 1400
                      END IF
                      DO 1300 M = 1,NUMCOM
                          IF(VECTOR.EQ.COM(M).AND.HORT.EQ.COMHT(M)) THEN
                              FLAG = 1
                              NODES(K,1) = NODES(K,1) + 1
                              NODES(K,NODES(K,1)+1) = NN
                              GOTO 1400
                          END IF
 1300                 CONTINUE
 1400             CONTINUE
 1500         CONTINUE
              IF(FLAG.EQ.1) THEN
                  PATH(NN,1) = I
                  PATH(NN,2) = J
                  NN = NN + 1
              END IF
 1800     CONTINUE
 2000 CONTINUE
      NN = NN - 1
      DO 3000 I = 1,NMLINK
          DO 2800 J = 1,LINKID(I,3)
              LINKID(I,J+3) = NODES(I,J+1)
 2800     CONTINUE
          DO 2900 K = J,8
              LINKID(I,K+3) = 0
 2900     CONTINUE
 3000 CONTINUE
      DO 4000 I = 1,NN
          IF(PATH(I,1).EQ.1.AND.PATH(I,2).EQ.1) THEN
              PATH(I,2) = 0
              GOTO 4000
          END IF
          DO 3800 J = 2,LS(PATH(I,1),1)+1
              IF(LS(PATH(I,1),J).EQ.TP(PATH(I,1),PATH(I,2))) THEN
                  PATH(I,2) = J - 1
                  GOTO 4000
              END IF
 3800     CONTINUE
 4000 CONTINUE
      DO 7000 I = 1,NMLINK
          IF (PRIMNO(I).EQ.2) THEN
              VN = LINKID(I,1)
              DO 6000 J = 1, NL
                  DO 5000 K = 1, LS(J,1)
                      IF (LS(J,K+1).EQ.(-VN)) THEN
                          PRIMNO(I) = -2
                      END IF
 5000             CONTINUE
 6000         CONTINUE
          END IF
 7000 CONTINUE
      RETURN                                             
      END     
C
      SUBROUTINE LCON 
      INTEGER CINFO(20),CVN,I1,I2,I3,I4,LEND,NUM
      COMMON/LCO/CINFO,CVN,I1,I2,I3,I4,LEND,NUM
      DO 100 I1 = 1, 20
          CINFO(I1) = 0
          DO 50 I2 = 1,4
              CLCON(I2,I1) = 0
   50     CONTINUE
  100 CONTINUE
      DO 200 I1 = 1,NC
          CINFO(CID(I1,2)) = CID(I1,1)
  200 CONTINUE
      DO 800 I1 = 2,NL
          CVN = CINFO(LS(I1,2))
          DO 700 I2 = 1,I1-1
              DO 600 I3 = 1,LS(I2,1)
                  IF(IABS(LS(I2,I3+1)).EQ.CVN) THEN
                      IF(I2.EQ.1) THEN
                          CLCON(I1-1,1) = I3
                          DO 300 I4 = 2,I3+1
                              IF(I4.EQ.I3+1.AND.LS(1,I4).GT.0) THEN
                                  CLCON(I1-1,1) = CLCON(I1-1,1) - 1
                                  GOTO 300
                              ELSE
                              CLCON(I1-1,I4) = LS(1,I4)
                              END IF
  300                     CONTINUE
                          GOTO 800
                      ELSE
                          NUM = CLCON(I2-1,1)
                          CLCON(I1-1,1) = I3 + NUM
                          DO 400 I4 = 2,NUM+1
                              CLCON(I1-1,I4) = CLCON(I2-1,I4)
  400                     CONTINUE
                          LEND = NUM+1+I3
                          DO 500 I4 = NUM+2,LEND
                              IF(I4.EQ.LEND.AND.LS(I2,I4-NUM).GT.0) THEN
                                  CLCON(I1-1,1) = CLCON(I1-1,1) - 1
                                  GOTO 500
                              ELSE
                                  CLCON(I1-1,I4) = LS(I2,I4-NUM)
                              END IF
  500                     CONTINUE
                          GOTO 800
                      END IF
                  END IF
  600         CONTINUE
  700     CONTINUE
  800 CONTINUE
      RETURN
      END 
C
      SUBROUTINE CURRENT (POSNUM)
      INTEGER T,N,POSNUM,PN,AL,I,K,KD,KI
      REAL RAD,C,INDEP(10,3),DEPEND(10,3)
      COMMON/CUR/T,N,PN,AL,I,K,KD,KI,RAD,C,INDEP,DEPEND
      NI = INT(RESULT(1))
      ND = INT(RESULT(2))
      K = (POSNUM-1)*((NI+ND)*3+1)
C     TIME = RESULT(K+4) ** MUST PASS TIME TO CALLER IF IT IS NEEDED **
      DO 50 I = 1,NI
          KI = K+(I-1)*3
          INDEP(I,1) = RESULT(KI+5)
          INDEP(I,2) = RESULT(KI+6)
          INDEP(I,3) = RESULT(KI+7)
C   Need array of independent values for plotting for independent
C   variable #1
          IF (I.EQ.1) INDVAR(POSNUM) = RESULT(KI+5)
   50 CONTINUE
      DO 75 I = 1,ND
          KD = K+NI*3+(I-1)*3
          DEPEND(I,1) = RESULT(KD+5)
          DEPEND(I,2) = RESULT(KD+6)
          DEPEND(I,3) = RESULT(KD+7)
   75 CONTINUE
      DO 100 I = 1,NV
          RAD = 3.141592654/180.0
          T = REFDIR(I,1)
          N = REFDIR(I,2)
          IF(T.EQ.0) THEN
              CALL BASE LINK(I,N,VECDIR,ANGLE,RAD)
          ELSE IF(T.EQ.1) THEN
              C = 0.0
              PN = 1
              CALL MOVING LINK(I,N,PN,VECDIR,INDEP,COMDIF,C,RAD)
          ELSE IF(T.EQ.2) THEN
              C = 0.0
              PN = 1
              CALL MOVING LINK(I,N,PN,VECDIR,DEPEND,COMDIF,C,RAD)
          ELSE IF(T.EQ.3) THEN
              C = 1.0
              AL = 0
              CALL PAIR NUM(I,NC,CID,AL,PN)
              CALL MOVING LINK(I,N,PN,VECDIR,INDEP,COMDIF,C,RAD)
          ELSE IF(T.EQ.4) THEN
              C = 1.0
              AL = 0
              CALL PAIR NUM(I,NC,CID,AL,PN)
              CALL MOVING LINK(I,N,PN,VECDIR,DEPEND,COMDIF,C,RAD)
          END IF
          RAD = 1.00
          T = REFMAG(I,1)
          N = REFMAG(I,2)
          IF(T.EQ.0) THEN
              CALL BASE LINK(I,N,VECMAG,LENGTH,RAD)
          ELSE IF(T.EQ.1) THEN
              C = 0.0
              PN = 1
              CALL MOVING LINK(I,N,PN,VECMAG,INDEP,COMDIF,C,RAD)
          ELSE IF(T.EQ.2) THEN
              C = 0.0
              PN = 1
              CALL MOVING LINK(I,N,PN,VECMAG,DEPEND,COMDIF,C,RAD)
          ELSE IF(T.EQ.3) THEN
              C = 1.0
              AL = 1
              CALL PAIR NUM(I,NC,CID,AL,PN)
              CALL MOVING LINK(I,N,PN,VECMAG,INDEP,COMDIF,C,RAD)
          ELSE IF(T.EQ.4) THEN
              C = 1.0
              AL = 1
              CALL PAIR NUM(I,NC,CID,AL,PN)
              CALL MOVING LINK(I,N,PN,VECMAG,DEPEND,COMDIF,C,RAD)
          END IF
  100 CONTINUE
      RETURN
      END
C
C
C--------------------
C
C
      SUBROUTINE BASE LINK(I,N,VECTOR,PART,RAD)
      INTEGER I,N
      REAL VECTOR(20,3),PART(20),RAD
          VECTOR(I,1) = PART(I)*RAD
          VECTOR(I,2) = 0.0
          VECTOR(I,3) = 0.0
      RETURN
      END
C
C
C-------------------
C
C
      SUBROUTINE MOVING LINK(I,N,PN,VECTOR,VARIABLE,COMDIF,C,RAD)
      INTEGER I,N,PN
      REAL VECTOR(20,3),VARIABLE(10,3),COMDIF(10),C,RAD
          VECTOR(I,1) = VARIABLE(N,1) + C*COMDIF(PN)*RAD
          VECTOR(I,2) = VARIABLE(N,2)
          VECTOR(I,3) = VARIABLE(N,3)
      RETURN
      END
C
C
C-------------------
C
C
      SUBROUTINE PAIR NUM(I,NC,CID,AL,PN)
      INTEGER I,NC,CID(10,3),AL,PN,J
      COMMON/PAI/J
      DO 100 J = 1,NC
          IF(CID(J,2).EQ.I.AND.CID(J,3).EQ.AL) THEN
              PN = J
              GOTO 200
          END IF
  100 CONTINUE
  200 RETURN
      END
C
      SUBROUTINE NODAL COORDINATES
      INTEGER VN,LN,I,J
      REAL SIGN
      COMMON/NDC/VN,LN,SIGN,I,J
      DO 100 I = 1,NN
          EXWHY(I,1) = 0.0
          EXWHY(I,2) = 0.0
  100 CONTINUE
      DO 400 I = 1,NN
          LN = PATH(I,1)
          IF(LN.GT.1) THEN
              DO 200 J = 2,CLCON(LN-1,1)+1
                  VN = IABS(CLCON(LN-1,J))
                  IF(CLCON(LN-1,J).GT.0) THEN
                      SIGN =  1.000
                  ELSE
                      SIGN = -1.000
                  END IF
                  EXWHY(I,1) = EXWHY(I,1) 
     &                       + SIGN*VECMAG(VN,1)*COS(VECDIR(VN,1))
                  EXWHY(I,2) = EXWHY(I,2)
     &                       + SIGN*VECMAG(VN,1)*SIN(VECDIR(VN,1))
  200         CONTINUE
          END IF
          DO 300 J = 2,PATH(I,2)+1
              VN = IABS(LS(LN,J))
              IF(LS(LN,J).GT.0) THEN
                  SIGN =  1.000
              ELSE
                  SIGN = -1.000
              END IF
              EXWHY(I,1) = EXWHY(I,1) 
     &                   + SIGN*VECMAG(VN,1)*COS(VECDIR(VN,1))
              EXWHY(I,2) = EXWHY(I,2)
     &                   + SIGN*VECMAG(VN,1)*SIN(VECDIR(VN,1))
  300     CONTINUE
  400 CONTINUE
      RETURN
      END
