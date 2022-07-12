C Steve's fourbar program
      PROGRAM fourbar
      use appgraphics
      COMPLEX P(3),Wa,Wb,Za
      COMPLEX Zb
      REAL Alpha(3),BetaA(3),BetaB(3),lngths(6)
      REAL radius(2,6)
      INTEGER current,A,B,DONE,dev
C      INTEGER start
C
      INTEGER::myscreen
C
      LOGICAL first
      PARAMETER (A=1,B=2,DONE=3)
      CHARACTER*5 clear,yesno*1,words,word2
C
C Add some windowing stuff
      myscreen=initwindow(640,480,title="Testwindow",closeflag=.TRUE.)
C      call closewindow(myscreen)
C
C     File name here - words
      words='words'
      word2='word2'
      call intro(words)
      write (*,'(A)') 'Enter<4> for color monitor<6> for mono?'
      read (*, '(I1)') dev
      clear= '[2J  '
      write (*, '(A1,A3)') char(27),clear
      write (*, '(A,A/)') 'Enter <N> to retrieve', 
     &  ' data from a file. '
      read (*, '(A1)') yesno
      if (yesno .eq. 'n' .or. yesno .eq. 'N') goto 6
      call intro(word2)
 1    continue
      first=.true.
C      
C     added a fake value for current variable
      current=DONE
C
      call inidata(P,Alpha,Wb,Zb)
C
 10   call angle(BetaA, A, P, Alpha, Wa, Za)
      call set(radius, Wa, Wb, Za, Zb, P(1), lngths)
      if (first) goto 20
      call grasof(lngths)
      call draw(radius, lngths, dev)
      call fix(current)
      if (current .eq. A) goto 10
      if (current .eq. DONE) goto 3
 20   call angle(BetaB, B, P, Alpha, Wb, Zb)
      call set(radius, Wa, Wb, Za, Zb, P(1), lngths)
      call grasof(lngths)
 2    continue
      call draw(radius, lngths, dev)
      first=.false.
      call fix(current)
      if (current .eq. B) goto 20

      if (current .eq. A) goto 10
 3    continue
      call cleanup
      write (*, '(A,A/)') 'Do you want to see the', 
     & ' mechanism in motion? <Y/N> '
      read (*, '(A1)') yesno
      if (yesno .eq. 'n' .or. yesno .eq. 'N') goto 4
      call move(radius, BetaA, BetaB, lngths, dev)
      call cleanup
 4    continue
      write (*, '(A/)') 'Do you want file utilities? <Y/N> '
      read (*, '(A1)') yesno
      if (yesno .eq. 'n' .or. yesno .eq. 'N') goto 5
 6    continue
      call savdata(P,BetaA,BetaB,Radius,Alpha,Wa,Wb,Za,Zb,lngths)
      if (yesno .eq. 'n' .or. yesno .eq. 'N') goto 2
 5    continue
      write (*,'(A/)') 'Do you want the link redrawn? <Y/N> '
      read (*, '(A1)') yesno
      if (yesno .eq. 'y' .or. yesno .eq. 'Y') goto 2
      write (*, '(A,A/)') 'Do you want to re-enter', 
     & ' initial data? <Y/N> '
      read (*, '(A1)') yesno
      if (yesno .eq. 'y' .or. yesno .eq. 'Y') goto 1
      write (*, '(A,A/)') 'Do you want to ', 
     &  'end session and return to DOS? <Y/N> '
      read (*, '(A1)') yesno
      if (yesno .eq. 'n' .or. yesno .eq. 'N') goto 4
      call cleanup
C      
      call closewindow(myscreen)
C      
      stop
      end
      
      subroutine intro(title)
      CHARACTER*80 line, title*5
      INTEGER io
C
      open (7, file=title)
C
C     Old Watfiv logic
C      loop : info
C        read (7,10) line
C          at end done
C            quit : info
C          end at end
C        write (*, 10) line
C      end loop
C
C     New looping logic
C
      DO
        READ(7,'(A80)',iostat=io) line
        IF (io/=0) EXIT
        WRITE (*,10) line
      END DO
C
 10   format (A80)
C 9    close (7, status='keep')
      close (7,status='keep')
      return
      end
      
      subroutine inidata(P, Alpha, Wb, Zb)
      COMPLEX P(3), Wb, Zb
      REAL Ptempx, Ptempy, Alpha(3)
      INTEGER n,m
      CHARACTER*1 respns
      Wb=cmplx(0.0,0.0)
      Zb=cmplx(0.0,0.0)
 1    Write (*,4) 'Enter the x&y coordinates of each precsion points.'
      Write (*,5) 'Then the angular orientation of the point.'
      DO 90 n=1,3
          write (*,121) 'The x coordinate of Position ',n,' is?'
          read (*,10) Ptempx
          write (*,121) 'The y coordinate of Position ',n,' is?'
          read (*,10) Ptempy
          P(n)=cmplx(Ptempx,Ptempy)
          write (*,121) 'The rotation (degrees) of Position ',n, ' is?'
          read (*,10) Alpha(n)
          Alpha(n)=Alpha(n)*3.141593/180.0
 90   continue
      do 100 m=1,3
          write(*,50) 'Position ',m,'(x y)',P(m),' Angle(rads)',Alpha(m)
 100  continue
      write (*,30) 'Do you want to change any data? (y/n)'
      read (*,40) respns
      if (respns .eq. 'y' .or. respns .eq. 'Y') goto 1
 4    format (/A)
 5    format (A)
 10   format (BN,F8.0)
 121  format (/A,I1,A$)
 30   format (/A)
 40   format (A1)
 50   format (/A,I1,A,2(2X,F10.5),A,F10.5)
      return
      end
      
      subroutine angle(Beta,current,P,Alpha,W,Z)
      INTEGER i,current,j,k,m
      COMPLEX P(3),W,Z,Delta2,Delta3,b2a3,b3a2,b2,a3,b3,a2,detrmnt
      COMPLEX pivot,D(3),R(3)
      REAL Alpha(3),Beta(3),pi,dbe12,dbe13,b12a13,b13a12
C      REAL angc
      REAL da12,da13,pointx,pointy,Dlnth(3),Darg(3),ANG(3)
C      REAL angs
      REAL gamma1,DB12A,DB12B
C      REAL tring3,tring2
C      REAL gamma2,DB13B,DB13A
      CHARACTER*1 rs,ls,side,test
      CHARACTER*6 posi,erase
      PARAMETER (pi=3.141593,rs='A',ls='B')
      posi  = '[24;1H'
      erase = '[K    '
  1   if (current .EQ. 1) then
        side = rs
      else
        side = ls
      endif
      write(*,'(A1,A6)') char(27),posi
      write(*,'(A1,A2)') char(27),erase
      write(*,'(A1,A,A)') side,' side, ',
     & ' set angle<A> or pivot<P>? '
      read(*,'(A1)') test
      if ((test .eq. 'A') .or. (test .eq. 'a')) goto 2
      if ((test .eq. 'P') .or. (test .eq. 'p')) goto 3
      goto 1
  2   continue
      write(*,'(A1,A6)') char(27),posi
      write(*,'(A1,A2)') char(27),erase
      do 100 i=1,3
        write(*,20) 'Side ',side,' link angle ',i,' (deg) ='
        read(*,30) Beta(i)
        write(*,'(A1,A6)') char(27),posi
        write(*,'(A1,A2)') char(27),erase
        Beta(i)=Beta(i)*pi/180.0
  100 continue
  3   continue
      da12=Alpha(2)-Alpha(1)
      da13=Alpha(3)-Alpha(1)
      a3=cmplx((cos(da13)),sin(da13))
      a2=cmplx((cos(da12)),sin(da12))
      if (test .EQ. 'P' .OR. test .EQ. 'p') then
        write(*,'(A1,A6)') char(27),posi
        write(*,'(A1,A2)') char(27),erase
        write(*,'(A,A1,A)') 'x position of ',side,'o is?'
        read(*,30) pointx
        write(*,'(A1,A6)') char(27),posi
        write(*,'(A1,A2)') char(27),erase
        write(*,'(A,A1,A)') 'y position of ',side,'o is?'
        read(*,30) pointy
        pivot=cmplx(pointx,pointy)
        do 111 j=1,3
          R(j)=P(j)-pivot
  111   continue
        D(1)=R(3)*a2-R(2)*a3
        D(2)=R(1)*a3-R(3)
        D(3)=R(2)-R(1)*a2
        do 222 k=1,3
          Dlnth(k)=CABS(D(k))
          Darg(k)=ATAN2((IMAG(D(k))),(REAL(D(k))))
          if ((Darg(k)) .LT. 0.0) Darg(k)=2.0*pi+Darg(k)
  222   continue
        ANG(3)=((Dlnth(1))**2+(Dlnth(2))**2-(Dlnth(3))**2)/
     &         (2*(Dlnth(1)*Dlnth(2)))
        if (ABS(ANG(3)) .GT. 1.0) then
          write(*,'(A1,A6)') char(27),posi
          write(*,'(A1,A2)') char(27),erase
          write(*,'(A)') 'Impossible configuration<enter>!'
C          pause
          read (5,*)
          goto 1
        endif
        ANG(3)=ACOS(ANG(3))
        if (Darg(1) .GT. pi) then
          gamma1 = Darg(1)-pi
        else
          gamma1 = Darg(1)+pi
        endif
        DB12A=gamma1+ANG(3)-Darg(2)
        DB12B = gamma1-ANG(3)-Darg(2)
        if (ABS(tan(dba12)-tan(DB12A)) .GT.
     &      (ABS(tan(dba12)-tan(DB12B)))) then
          dbe12=DB12A
        else
          dbe12=DB12B
        endif
        if (dbe12 .GT. 2.0*pi) dbe12=dbe12-2.0*pi
        if (dbe12 .LT. -2.0*pi) dbe12=dbe12+2.0*pi
        b2=cmplx(cos(dbe12),sin(dbe12))
        b3=((-1.0)*D(1)-D(2)*b2)/(D(3))
        dbe13=ATAN2(IMAG(b3),REAL(b3))
        write(*,'(A1,A6)') char(27),posi
        write(*,'(A1,A2)') char(27),erase
        write(*,40) 'angle12=',dbe12*180.0/pi,'angle13='
     &              ,dbe13*180.0/pi,'<enter>'
C        pause
        read (5,*)

      else
        dbe12=Beta(2)-Beta(1)
        dbe13=Beta(3)-Beta(1)
        b3=cmplx((cos(dbe13)),sin(dbe13))
        b2=cmplx((cos(dbe12)),(sin(dbe12)))
      endif
      b12a13=dbe12+da13
      b13a12=dbe13_da12
      Delta2=P(2)-P(1)
      Delta3=P(3)-P(1)
      b2a3=cmplx((cos(b12a13)),(sin(b12a13)))
      b3a2=cmplx((cos(b13a12)),(sin(b13a12)))
      detrmnt=b2a3-b3a2-b2-a3+b3+a2
      W=(Delta2*a3-Delta3*a2+Delta3-Delta2)/detrmnt
      Z=(Delta3*b2-Delta2*b3-Delta3+Delta2)/detrmnt
      Beta(1)=ATAN2((IMAG(W)),(REAL(W)))
      Beta(3)=Beta(1)+dbe13
      Beta(2)=Beta(1)+dbe12
      if ((Beta(1) .GT. 2.0*pi) .OR. (Beta(2) .GT. 2.0*pi) .OR.
     &    (Beta(3) .GT. 2.0*pi)) then
        do 333 m=1,3
          Beta(m) = Beta(m) - 2.0*pi
  333   continue
      endif
      if ((Beta(1) .LT. 2.0*pi) .OR. (Beta(2) .LT. 2.0*pi) .OR.
     &    (Beta(3) .LT. 2.0*pi)) then
        do 444 m=1,3
          Beta(m) = Beta(m) + 2.0*pi
 444    continue
      endif
C 10   format (//A,A1/)
 20   format (A,A1,A,I1,A1)
 30   format (BN,F8.0)
 40   format (A,F8.3,F8.3,A)
      return
      end
      
      subroutine set(Link, Wa, Wb, Za, Zb, P, lngths)
      COMPLEX Wa, Wb, Za, Zb, P
      REAL Link(2,6),lngths(6)
      INTEGER x,y,i
      PARAMETER (x=1,y=2)
      Link(x,1)=real(P-Za-Wa)
      Link(y,1)=aimag(P-Za-Wa)
      Link(x,2)=real(P-Za)
      Link(y,2)=aimag(P-Za)
      Link(x,3)=real(P)
      Link(y,3)=aimag(P)
      Link(x,4)=real(P)
      Link(y,4)=aimag(P)
      Link(x,5)=real(P-Zb)
      Link(y,5)=aimag(P-Zb)
      Link(x,6)=real(P-Zb-Wb)
      Link(y,6)=aimag(P-Zb-Wb)
      Do 500 i=1,2
        lngths(i)=sqrt(((Link(x,i+1)-Link(x,i))**2)+
     &   ((Link(y,i+1)-Link(y,i))**2))
 500  continue
      Do 600 i=3,4
        lngths(i)=sqrt(((Link(x,i+2)-Link(x,i+1))**2)+
     &   ((Link(y,i+2)-Link(y,i+1))**2))
 600  continue
      lngths(5)=sqrt(((Link(x,5)-Link(x,2))**2) +
     & ((Link(y,5)-Link(y,2))**2))
      lngths(6)=sqrt(((Link(x,6)-Link(x,2))**2) +
     & ((Link(y,6)-Link(y,1))**2))
      return
      end
      
      subroutine grasof(lngth)
      REAL lngth(6),lmax,lmin,la,lb
      CHARACTER*18 mename
      lmax=amax1(lngth(1),lngth(5),lngth(4),lngth(6))
      lmin=amin1(lngth(1),lngth(5),lngth(4),lngth(6))
      if (lngth(1).ne.lmax.and.lngth(1).ne.lmin) then
        la=lngth(1)
        if (lngth(5).ne.lmax.and.lngth(5).ne.lmin) then
          lb=lngth(5)
        else
          if(lngth(4).ne.lmax.and.lngth(4).ne.lmin) then
            lb=lngth(4)
          else
            lb=lngth(6)
          endif
        endif
      else
        if (lngth(5).ne.lmax.and.lngth(5).ne.lmin) then
          la=lngth(5)
          if(lngth(4).ne.lmax.and.lngth(4).ne.lmin) then
            lb=lngth(4)
          else
            lb=lngth(6)
          endif
        else
          la=lngth(4)
          lb=lngth(6)
        endif
      endif
      if ((lmax+lmin).lt.(la+lb)) then
        if (lmin.eq.lngth(1)) then
          mename = 'CRANK-ROCKER'
        endif
        if (lmin.eq.lngth(6)) then
          mename = 'DRAG-LINK'
        endif
        if (lmin.eq.lngth(5)) then
          mename = 'DOUBLE-ROCKER'
        endif
        if (lmin.eq.lngth(4)) then
          mename = 'ROCKER-CRANK'
        endif
      else
        if ((lmin+lmax).eq.(la+lb)) then
          mename = 'CHANGE POINT MECH.'
        endif
        if ((lmin+lmax).gt.(la+lb)) then
          mename = 'NON-GRASOF'
        endif
      endif
      WRITE (*,'(A1,A6,A1,A2)') CHAR(27),'[24;1H',CHAR(27),'[K'
      WRITE (*,'(A,A,A)') 'MECH. TYPE = ',mename,' <ENTER>'
C      pause
C     Replace pause statement
C     STDIN = 5 ?
C
      read(5,*)
      return
      end
      
      subroutine draw(Links,lngths,dev)
      REAL Links(2,6),lngths(6)
      INTEGER x,y,j,L
      PARAMETER (x=1,y=2)
      REAL ptx(5),pty(5),mx(2),mn(2),cupx(2),cupy(2),Ax,Ay,Bx,By
      INTEGER i,K,dev,tprt
C      INTEGER index, asf(13)
      REAL height,space,expand,Aox,Aoy,Box,Boy,Px,Py,SCALE,size
      REAL TP(2),BTM(2),LFT(2),RT(2),zerox(2),zeroy(2),psns(2)
      L=1
      mx(x)=Links(x,1)
      mx(y)=Links(y,1)
      mn(x)=Links(x,1)
      mn(y)=Links(x,1)
      DO 600 K=1,6
        if (K .EQ. 4) L=3
        ptx(L)=Links(x,K)
        pty(L)=Links(y,K)
        mx(x)=amax1(ptx(L),mx(x))
        mx(y)=amax1(pty(L),mx(y))
        mn(x)=amin1(ptx(L),mn(x))
        mn(y)=amin1(pty(L),mn(y))
        L=L+1
 600  continue
      DO 700 i=1,2
        cupx(i)=ptx(y*i)
        cupy(i)=pty(y*i)
 700  continue
      if ((mx(x)-mn(x)) .GT. (mx(y)-mn(y))) then
        mn(y) = .5*(mn(y)+mx(y)+mn(x)-mx(x))
        mx(y) = mn(y)+mx(x)-mn(x)
      else
        mn(x) = .5*(mn(x)+mx(x)+mn(y)-mx(y))
        mx(x) = mn(x)+mx(y)-mn(y)
      endif
      size=mx(x)-mn(x)
      if (size .GT. 2500) then
        write(*,*) '!!!The system too large to be drawn!!!<enter>'
C        pause
        read(5,*)
        goto 988
      endif
      do 111 j=1,2
        mx(j)=mx(j)+0.1*size
        mn(j)=mn(j)-0.1*size
 111  continue
      SCALE = .025
      if (size .GT. 0.50) SCALE = .05
      if (size .GT. 1.0) SCALE = .1
      if (size .GT. 2.50) SCALE = .25
      if (size .GT. 5.0) SCALE = .5
      if (size .GT. 10.0) SCALE = 1.0
      if (size .GT. 25.0) SCALE = 2.5
      if (size .GT. 50.0) SCALE = 5.0
      if (size .GT. 100.0) SCALE = 10.0
      if (size .GT. 250.0) SCALE = 25.0
      if (size .GT. 500.0) SCALE = 50.0
      if (size .GT. 1000.0) SCALE = 100.0
      LFT(1)=SCALE*(NINT(mn(y)/SCALE))
      RT(2)=SCALE*(NINT(mx(x)/SCALE))
      BTM(2)=SCALE*(NINT(mx(y)/SCALE))
      TP(2)=SCALE*(NINT(mn(x)/SCALE))
      LFT(2)=LFT(1)
      BTM(1)=LFT(1)
      TP(1)=TP(2)
      RT(1)=TP(2)
      tprt=INT((mx(x)-mn(x))/SCALE)
      Aox=Links(x,1)-0.04*size
      Aoy=Links(y,1)-0.06*size
      Box=Links(x,6)+0.02*size
      Boy=Links(y,6)-0.06*size
      Ax=Links(x,2)-0.08*size
      Ay=Links(y,2)
      Bx=Links(x,5)+0.04*size
      By=Links(y,5)
      Px=Links(x,3)
      Py=Links(y,3)+0.04*size
 
C
C     GKS stuff here
C
C      call gopks(0)
C      call gicga4(dev)
C      call gopwk(1,0,dev)
C      call gacwk(1)
C      call gsvp(1,.391,1.0,.128,1.0)
C      CALL GSELNT(1)
C      call gswn(1,mn(x),mx(x),mn(y),mx(y))
C      call gsln(3)
C      call gsplci(2)
      do 100 j=1,tprt
C        call gpl(2,TP,BTM)
C        call gpl(2,RT,LFT)
        tp(1)=tp(1)+1.0*SCALE
        lft(1)=lft(1)+1.0*SCALE
        TP(2)=TP(1)
        LFT(2)=LFT(1)
 100  continue
C      call gsmk(4)
C      call gpm(5,ptx,pty)
C      call gsln(1)
      zeroy(1)=0.0
      zeroy(2)=0.0
C      call gpl(2,RT,zeroy)
      zerox(1)=0.0
      zerox(2)=0.0
C      call gpl(2,zerox,BTM)
C      call gsplci(1)
C      call gpl(5,ptx,pty)
C      call gpl(2,cupx,cupy)
      height=.055*(mx(x)-mn(x))
      space=0
      expand=0.8
C      call gstxfp(1,2)
C      call gschh(height)
C      call gschsp(space)
C      call gschxp(expand)
C      call gtx(Aox,Aoy,'Ao')
C      call gtx(Ax,Ay,'A')
C      call gtx(Bx,By,'B')
C      call gtx(Box,Boy,'Bo')
C      call gtx(px,py,'P')
C      call gdawk(1)
C      call gclwk(1)
C      call gclks()
      write(*,'(A,F5.1)') 'GRID=',SCALE
      write(*,10) 'POSITION/X/Y'
      write (*,10) 'Ao'
      write (*,20) Links(x,1)
      write (*,20) Links(y,1)
      write (*,10) 'A(1)'
      write (*,20) Links(x,2)
      write (*,20) Links(y,2)
      write (*,10) 'Bo'
      write (*,20) Links(x,6)
      write (*,20) Links(y,6)
      write (*,10) 'B(1)'
      write (*,20) Links(x,5)
      write (*,20) Links(y,5)
      write (*,10) 'LENGTHS'
      write (*,10) 'Ao-A'
      write (*,20) lngths(1)
      write (*,10) 'A-B'
      write (*,20) lngths(5)
      write (*,10) 'Bo-B'
      write (*,20) lngths(4)
      write (*,10) 'Ao-Bo'
      write (*,30) lngths(6)
 10   format (A)
 20   format (F10.4)
 30   format (F10.4)
 988  continue
      return
      end
      
      
      subroutine fix(current)
      INTEGER current
      CHARACTER*1 anser
      CHARACTER*6 posi,erase
      posi ='[24;1H'
      erase='[K    '
      write(*,30) char(27),posi
      write(*,10) 'Do you want to change anything? <Y/N>'
      read(*,20) anser
      write(*,30) char(27), posi
      write(*,40) char(27),erase
      if (anser .eq. 'Y' .or. anser .eq. 'y') then
        write(*,10) 'Which side? <A/B>'
        read(*,20) anser
        write(*,30) char(27), posi
        write(*,40) char(27), erase
        if (anser .eq. 'a' .or. anser .eq. 'A') then
          current = 1
        else
          current = 2
        endif
      else
        current = 3
      endif
 10   format (A)
 20   format (A1)
 30   format (A1,A6)
 40   format (A1,A2)
      return
      end
      
      
      subroutine savdata(P,BetaA,BetaB,L,Alpha,Wa,Wb,Za,Zb,lngths)
      COMPLEX P(3), Wa, Wb, Za, Zb
      REAL BetaA(3), BetaB(3), Alpha(3), L(2,6), lngths(6)
      INTEGER x,y,i,wait
      CHARACTER Rest*1, flname*6
      PARAMETER (x=1,y=2,pi=3.141593)
 1    continue
      write (*,10) 'Retrieve <R> or Store <S> or Neither <N>?'
      read (*,'(A1)') rest
      if (rest.eq.'n' .or. rest .eq. 'N') goto 11
      write (*,10) 'What filename (6 letters)?'
      read (*,'(A6)') flname
      if (rest .eq. 'r' .or. rest .eq. 'R') then
        open(1,ERR=999,FILE=flname,STATUS='new')
        read (1,20) P,BetaA,BetaB,L,Alpha,Wa,Wb,Za,Zb,lngths
        close (1,ERR=999,STATUS='KEEP')
      else
        open(2,ERR=999,FILE=flname,STATUS='new')
        write(2,20) P,BetaA,BetaB,L,Alpha,Wa,Wb,Za,Zb,lngths
        close(2,ERR=999,STATUS='KEEP')
      endif
 11   continue
      write (*,10) 'Do you want to see a table of the data?<Y/N>'
      read (*,'(A1)') rest
      if (rest .eq. 'n' .or. rest .eq. 'N') goto 12
C
C      write(*,'(A//)') ' hit <enter> to continue when you see 111'
C      pause 111
C
      wait = 1
C     open(3,file='CON')
      open(3,status='SCRATCH')
      write(*,'(A1,A4)') char(27),'[=3h'
 13   continue
      write(3,30) ' FOUR BAR LINKAGE SYNTHESIS '
      write(3,40) ' PRECISION POINT DATA       '
      write(3,50) 'POINT #','POSITION','ANGLE','X','Y','RADS','DEG'
      do 111 i=1,3
        write(3,60) i,P(i),Alpha(i),Alpha(i)*180.0/pi
 111  continue
C      if (wait .eq. 1) pause 111
      write(3,40) '        LINK ANGLE DATA     '
      write(3,70) 'POSITION','LINK A','LINK B','RAD','DEG','RAD','DEG'
      do 222 i=1,3
      write(3,60) i,BetaA(i),BetaA(i)*180.0/pi,
     &              BetaB(i),BetaB(i)*180.0/pi
 222  continue
C      if (wait .eq. 1) pause 111
      write(3,40) 'LINK LENGTH AND POSITION DATA'
      write(3,90) 'LINK(Ao-A=1,A-B=5,P-A=2,P-B=3,B-Bo=4,Bo-Ao=6)'
      write(3,100) 'LINK','LENGTHS','POSITION of FIRST POINT','X','Y'
      do 333 i=1,6
      write (3,110) i,lngths(i),L(x,i),L(y,i)
 333  continue
      write(3,120)
      write(*,10) 'DO YOU WANT THIS TABLE PRINTED <Y/N>?'
      read (*,'(A1)') rest
      if (rest .eq. 'y' .or. rest .eq. 'Y') then
        close (3)
        open (3,FILE='PRN')
        wait = 0
        goto 13
      else
        goto 11
      endif
 10   format (A)
 20   format (6(6(E12.6)/),5(E12.6))
 30   format (26X,28('*')/,26X,A/,26X,28('*')//)
 40   format (//26X,A/)
 50   format (6X,A,T26,A,T58,A/,T22,A,T37,A,T52,A,T67,A/)
 60   format (T7,I1,T15,4(3X,F12.5)/)
 70   format (T3,A,T27,A,T57,A/,T21,A,T36,A,T51,A,T66,A/)
 90   format(8X,A/)
 100  format(T6,A,T18,A,T41,A/,T42,A,T64,A/)
 110  format(T7,I1,T15,F12.5,T36,F12.5,T58,F12.5/)
 120  format(///T5,70('*')//////////)
 999  write(*,10) '<<<<<<<< An error has occurred >>>>>>>>>>'
      goto 1
 12   continue
      return
      end
      
      subroutine move(link,BetaA, BetaB,L,dev)
      REAL link(2,6),BetaA(3),BetaB(3),Tempx(6),Tempy(6),cupx(2)
      REAL ANG2P,A,Rd,Thetad,Phi,Step,L(6),pi,Theta(4),cupy(2)
      REAL storex(5),storey(5),midx(2),midy(2),mx(2),mn(2)
      REAL tesang(2),Phic,size,posi1x(5),posi1y(5),tracex(100)
      REAL posi2x(5),posi2y(5),tracey(100),bar1x(2),bar1y(2)
      REAL bar2x(2),bar2y(2),start,posi3x(5),posi3y(5),bar3x(2)
      REAL bar3y(2)
      INTEGER x,y,dev,i,j,k,m,mktype,mkcol,n,once
      PARAMETER (x=1,y=2,pi=3.141593)
      WRITE (*,'(/////////A,A)') '+++'
      WRITE (*,'(A)') '***THE SPECIFIED MOTION OF THE LINK IS***'
      WRITE (*,'(A)') '++++++++++++++++++++++++++++++++++++++++++++'
      ANG2P=ACOS(((L(2))**2+(L(5))**2-(L(3))**2)/(2*L(2)*L(5)))
      tesang(1)=ATAN((link(y,3)-link(y,5))/(link(x,3)-link(x,5)))
      if (tesang(1) .LT. 0.0) tesang(1)=2.0*pi+tesang(1)
      tesang(2)=ATAN((link(y,2)-link(y,5))/(link(x,2)-link(x,5)))
      if (tesang(2).LT.0.0) tesang(2)=2.0*pi+tesang(2)
      if (tesang(2).GT.pi) then
        if ((tesang(1).GT.tesang(2)).OR.((tesang(2)-tesang(1)).GT.pi))
     &     ANG2P=(-1.0)*ANG2P
      else
        if ((tesang(1).GT.tesang(2)).AND.
     &      ((tesang(1)-tesang(2)).LT.pi)) ANG2P=(-1.0)*ANG2P
      endif
      mx(x)=link(x,1)
      mn(x)=link(x,1)
      mx(y)=link(y,1)
      mn(y)=link(y,1)
      do 22 j=1,5
        storex(j)=0.0
        storey(j)=0.0
 22   continue
      do 33 j=1,2
        midx(j)=0.0
        midy(j)=0.0
 33   continue
      tempx(1)=link(x,1)
      tempx(5)=link(x,6)
      tempy(1)=link(y,1)
      tempy(5)=link(y,6)
      Thetad=ATAN2((link(y,2)-tempy(5)),(link(x,2)-tempx(5)))
      if (Thetad.LT.0.0) Thetad=2.0*pi+Thetad
      if (Thetad.GE.pi) then
        if (BetaB(1).LT.Thetad.AND.BetaB(1).GT.(Thetad-pi)) then
          A=-1.0
        else
          A=1.0
        endif
      else
        if (BetaB(1).LT.Thetad.OR.BetaB(1).GT.(Thetad+pi)) then
          A=-1.0
        else
          A=1.0
        endif
      endif
      start=BetaA(1)
      if (BetaA(1).LT.BetaA(2).AND.BetaA(2).LT.BetaA(3))
     &    Step=(BetaA(3)-BetaA(1))/10.0
      if (BetaA(1).GT.BetaA(2).AND.BetaA(2).GT.BetaA(3))
     &    Step=(BetaA(3)-BetaA(1))/10.0
      if (BetaA(1).LT.BetaA(3).AND.BetaA(3).LT.BetaA(2))
     &    Step=(BetaA(2)-BetaA(1))/10.0
      if (BetaA(1).GT.BetaA(3).AND.BetaA(3).GT.BetaA(2))
     &    Step=(BetaA(2)-BetaA(1))/10.0
      if (BetaA(2).LT.BetaA(1).AND.BetaA(1).LT.BetaA(3)) then
        start=BetaA(2)
        Step=(BetaA(3)-BetaA(2))/10.0
      endif
      if (BetaA(2).GT.BetaA(1).AND.BetaA(1).GT.BetaA(3)) then
        Step=(BetaA(3)-BetaA(2))/10.0
        start=BetaA(2)
      endif
      do 555 i=0,10
        Theta(1)=start+Step*real(i)
        tempx(2)=tempx(1)+L(1)*cos(Theta(1))
        tempy(2)=tempy(1)+L(1)*sin(Theta(1))
        Rd=SQRT((tempx(2)-tempx(5))**2+(tempy(2)-tempy(5))**2)
        Thetad=ATAN2((tempy(2)-tempy(5)),(tempx(2)-tempy(5)))
        if (Thetad.LT.0.0) Thetad=2.0*pi+Thetad
        Phic=(((L(4))**2)+(Rd**2)-((L(5))**2))/(2*L(4)*Rd)
        if (ABS(Phic).GT.1.0) goto 666
        Phi=ACOS(Phic)
        Theta(4)=Thetad+A*Phi
        tempx(4)=tempx(5)+L(4)*cos(Theta(4))
        tempy(4)=tempy(5)+L(4)*sin(Theta(4))
        Theta(2)=ATAN2((tempy(4)-tempy(2)),(tempx(4)-tempx(2)))
        if ((Theta(2)).LT.0.0) Theta(2)=2.0*pi+Theta2
        Theta(3)=Theta(2)+ANG2P
        tempx(3)=tempx(2)+L(2)*cos(Theta(3))
        tempy(3)=tempy(2)+L(2)*sin(Theta(3))
        do 13 k=1,5
          mx(x)=amax1(tempx(k),mx(x))
          mx(y)=amax1(tempy(k),mx(y))
          mn(x)=amin1(tempx(k),mn(x))
          mn(y)=amin1(tempy(k),mn(y))
 13     continue
 666    continue
 555  continue
      if ((mx(x)-mn(x)).GT.(mx(y)-mn(y))) then
        mn(y)=0.5*(mn(y)+mx(y)+mn(x)-mx(x))
        mx(y)=mn(y)+mx(x)-mn(x)
      else
        mn(x)=0.5*(mn(x)+mx(x)+mn(y)-mx(y))
        mx(x)=mn(y)+mx(y)-mn(y)
      endif
      size=mx(x)-mn(x)
      do 777 k=1,2
        mx(k)=mx(k)+0.05*size
        mn(k)=mn(k)-0.05*size
 777  continue
      mktype=1
      mkcol=0
C      call gopks(0)
C      call gicga4(dev)
C      call gopwk(1,0,dev)
C      call gacwk(1)
C      call gsmk(mktype)
C      call gspmc(mkcol)
C      call gsvp(1,.195,.805,.128,1.0)
C      call gselnt(1)
C      call gswn(1,mn(x),mx(x),mn(y),mx(y))
      step=step/10.0
      Theta(1)=start-step
      do 888 m=1,2
      once=0
      do 111 i=0,101
      Theta(1)=Theta(1)+step
      tempx(2)=tempx(1)+L(1)*cos(Theta(1))
      tempy(2)=tempy(1)+L(1)*sin(Theta(1))
      Rd=SQRT((tempx(2)-tempx(5))**2+(tempy(2)-tempy(5))**2)
      Thetad=ATAN2((tempy(2)-tempy(5)),(tempx(2)-tempx(5)))
      if (Thetad.LT.0.0) Thetad=2.0*pi+Thetad
      Phic=(((L(4))**2)+(Rd**2)-((L(5))**2))/(2*L(4)*Rd)
      if (ABS(Phic).GT.1.0) then
        write (*,'(A1,A6)') char(27),'[24;1H'
        write (*,'(A)') 'Linkage lock-up! <ENTER> to continue'
        once=1
        goto 444
      endif
      Phi=ACOS(Phic)
      Theta(4)=Thetad+A*Phi
      tempx(4)=tempx(5)+L(4)*cos(Theta(4))
      tempy(4)=tempy(5)+L(4)*sin(Theta(4))
      Theta(2)=ATAN2((tempy(4)-tempy(2)),(tempx(4)-tempx(2)))
      if ((Theta(2)).LT.0.0) Theta(2)=2.0*pi+Theta(2)
      Theta(3)=Theta(2)+ANG2P
      tempx(3)=tempx(2)+L(2)*cos(Theta(3))
      tempy(3)=tempy(2)+L(2)*cos(Theta(3))
      cupx(1)=tempx(2)
      cupx(2)=tempx(4)
      cupy(1)=tempy(2)
      cupy(2)=tempy(4)
      if (m.EQ.1) then
        if ((Theta(1).LE.BetaA(1)).AND.((Theta(1)+ABS(Step)).GT.
     &     (BetaA(1)))) then
          do 19 n=1,5
            posi1x(n)=tempx(n)
            posi1y(n)=tempy(n)
 19       continue
          do 119 n=1,2
            bar1x(n)=cupx(n)
            bar1y(n)=cupy(n)
119       continue
        endif    
        if ((Theta(1).LE.BetaA(2)).AND.((Theta(1)+ABS(Step)).GT.
     &     (BetaA(2)))) then
          do 29 n=1,5
            posi2x(n)=tempx(n)
          posi2y(n)=tempy(n)
29        continue
          do 129 n=1,2
            bar2x(n)=cupx(n)
            bar2y(n)=cupy(n)
129       continue
        endif
        if ((Theta(1).LE.BetaA(3)).AND.((Theta(1)+ABS(Step)).GT.
     &     (BetaA(3)))) then
          do 39 n=1,5
            posi3x(n)=tempx(n)
            posi3y(n)=tempy(n)
39        continue
          do 139 n=1,2
            bar3x(n)=cupx(n)
            bar3y(n)=cupy(n)
139       continue
        endif
      endif
      if (i.EQ.0.OR.i.EQ.101) goto 14
      tracex(i)=tempx(3)
      tracey(i)=tempy(3)
 14   continue
C      call gsplci(0)
C      call gpl(5,storex,storey)
C      call gpl(2,midx,midy)
C      call gsplci(1)
C      call gpl(5,tempx,tempy)
C      call gpl(2,cupx,cupy)
C      call gpm(1,tempx(3),tempy(3))
      do 222 j=1,5
        storex(j)=tempx(j)
        storey(j)=tempy(j)
 222  continue
      do 333 j=1,2
        midx(j)=cupx(j)
        midy(j)=cupy(j)
 333  continue
 444  continue
 111  continue
      if (m.EQ.2) goto 49
C      call gspmci(1)
C      call gsplci(1)
C      call gpm(100,tracex,tracey)
C      call gpl(5,posi1x,posi1y)
C      call gpl(5,posi2x,posi2y)
C      call gpl(5,posi3x,posi3y)
C      call gpl(2,bar1x,bar1y)
C      call gpl(2,bar2x,bar2y)
C      call gpl(2,bar3x,bar3y)
  49  continue
      write (*,'(A1,A6)') char(27),'[24;1H'
      write (*,'(A)') '<ENTER> TO CONTINUE'
C      pause
      read (5,*)
C 
      write (*,'(A1,A6,A1,A2)') char(27),'[24;1H',char(27),'[K'
      step=(-1.0)*step
C      call gspmci(0)
 888  continue
      return
      end
      
      subroutine cleanup
      INTEGER ifake
      ifake = 2
      return
      end
      