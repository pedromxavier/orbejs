C PROGRAM ORBE ver 3
C TABARE GALLARDO September 2016
C BASED ON EVORB (BRUNINI AND GALLARDO)
C www.astronomia.edu.uy/orbe
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 MPLA
	CHARACTER*50 FILEIN,FILEOU,ACOMM
	parameter (NTT=100)
	DIMENSION XPLA(NTT,3),VPLA(NTT,3)
	DIMENSION APLA(NTT),EPLA(NTT)
	DIMENSION MPLA(NTT),RPLA(NTT)
	DIMENSION POS(3),VEL(3)
	COMMON/CTE/GM,GM0
C DATA INPUT
	FILEIN='orbeini.dat'
C OUTPUT
	FILEOU='orbeout.dat'
C UNIT TIME = JULIAN YEAR
C UNIT MASS = SOLAR MASS
C UNIT DISTANCE = AU
	PI=4.D0*DATAN(1.D0)
	DOSPI=2.D0*PI
	GAU=0.01720209895D0
	ANIO=365.25D0
	GM=(GAU*ANIO)**2
	GAR=PI/180.D0
C T INITIAL = 0
	CTIEMPO=0.D0
C      TINIC=0.D0
	T=0.D0
	WRITE(*,*)'O---------------------------------------------------O'
	WRITE(*,*)'|                       ORBE 3                      |'
	WRITE(*,*)'|              version September 2016               |'
	WRITE(*,*)'|            www.astronomia.edu.uy/orbe             |'
	WRITE(*,*)'O---------------------------------------------------O'
C READ DATA
	OPEN(2,FILE=FILEIN)
	READ(2,1004) ACOMM
	READ(2,*) CMASS    ! CENTRAL MASS
	READ(2,1004)ACOMM
	READ(2,*) TMAX         ! FINAL TIME IN YEARS
	READ(2,1004)ACOMM
	READ(2,*) TSAL         ! OUTPUT SNAPSHOTS
	GM0=GM*CMASS
	PEORBMIN=100000.0D0
	NPLA=0
	READ(2,1004)ACOMM               ! ORBITAL ELEMENTS
	DO 101 IU=1,100
		READ(2,*,END=500)A0,E0,XI0,XN0,W0,EME0,VAMA
		NPLA=NPLA+1
		XI0=XI0*GAR
		I=NPLA
        EME0=EME0*GAR
		XN0=XN0*GAR
		W0=W0*GAR
C MEAN MOTION
		ENE=DSQRT(GM*(CMASS+VAMA)/A0**3)
C MINIMUM ORBITAL PERIOD
        PEORB=DOSPI/ENE
        IF (PEORB.LT.PEORBMIN) THEN
          PEORBMIN=PEORB
        ENDIF
C PERIHELION PASSAGE IN DAYS
        MPLA(I)=GM*VAMA
C INITIAL POTITION AND VELOCITY
		CALL POVE(a0,E0,XI0,XN0,W0,eme0,ene,POS,VEL)
        RPLA(I)=0.D0
		DO J=1,3
	  		RPLA(I)=RPLA(I)+POS(J)**2
	  		XPLA(I,J)=POS(J)
	  		VPLA(I,J)=VEL(J)
		ENDDO
		RPLA(I)=DSQRT(RPLA(I))
101   CONTINUE
500   CLOSE(2)
C SAVE ORBITAL ELEMENTS FOR T=0
      	OPEN(1,FILE=FILEOU,STATUS='UNKNOWN',ACCESS='APPEND')
      	DO KK=1,NPLA
			CALL PLANO(XPLA,VPLA,KK,CLINA,DOSPI,GM0,MPLA,ON,W,AMED,SE,SA)
			WRITE(1,1001)T,KK,SA,SE,CLINA,ON,W,AMED
      	ENDDO
      	CLOSE(1)
C TIME STEP PEORBMIN/40 APROX
        PASO=PEORBMIN/40.D0
        PAS1=0.00001D0
        DO IU=1,7
			IF (PASO.GT.PAS1) THEN
				H=PAS1
			ENDIF
			IF (PASO.GT.PAS1*2.D0) THEN
				H=PAS1*2.D0
			ENDIF
			IF (PASO.GT.PAS1*5.D0) THEN
				H=PAS1*5.D0
			ENDIF
			PAS1=PAS1*10.D0
      	ENDDO
C TSAL MUST BE AN INTEGER NUMBER OF STEPS H
      	TSAL=DNINT(TSAL/H)*H
		WRITE(*,*)'BODIES: ',NPLA
		WRITE(*,*)'FINAL TIME: ',TMAX
		WRITE(*,*)'TIMESTEP: ',H
C TMAX NEGATIVE INTEGRATE BACKWARDS
      	IF(TMAX.LT.0.) H=-H
      		HK = 0.5D0*H
200   TGRID=0.D0
210   TGRID=TGRID+H
      		CTIEMPO=CTIEMPO+1.D0
      		T=CTIEMPO*H
      		CALL MOVIKEP(NPLA,HK,XPLA,VPLA,RPLA,APLA,EPLA,MPLA)
			CALL PERTURB(XPLA,VPLA,RPLA,NPLA,MPLA,H)
			CALL MOVIKEP(NPLA,HK,XPLA,VPLA,RPLA,APLA,EPLA,MPLA)
      		IF(DABS(TGRID-DSIGN(TSAL,H)).LT.1.D-05)THEN
				OPEN(1,FILE=FILEOU,STATUS='UNKNOWN',ACCESS='APPEND')
				NSURVIVORS=NPLA
				DO 50 KK=1,NPLA
C IF KK WAS ELIMINATED
	  				IF(MPLA(KK).EQ.1.2345D-30) THEN
            			NSURVIVORS=NSURVIVORS-1
            			GOTO 50
	  				ENDIF
	  				CALL PLANO(XPLA,VPLA,KK,CLINA,DOSPI,GM0,MPLA,ON,W,AMED,SE,SA)
	  				WRITE(1,1001)T,KK,SA,SE,CLINA,ON,W,AMED
50    CONTINUE
        			CLOSE(1)
					WRITE(*,1012)T,NSURVIVORS
        			GOTO 200
      		ENDIF
      		IF((TMAX-T)*DSIGN(1.D0,H).GT.0.)GOTO 210
1001  FORMAT(1P,E13.6,0P,I4,F13.6,F9.6,4F7.2)
1004  FORMAT(A50)
1012  FORMAT(F14.1,I8)
		    STOP
      		END
C ========================================================================
C ========================================================================
C ADVANCE KEPLERIAN MOTION HALF STEP
      SUBROUTINE MOVIKEP(NPLA,H,XPLA,VPLA,RPLA,APLA,EPLA,MPLA)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 MPLA
      parameter (NTT=100)
      DIMENSION XPLA(NTT,3),VPLA(NTT,3)
      DIMENSION APLA(NTT),EPLA(NTT)
      DIMENSION RPLA(NTT),MPLA(NTT)
      DIMENSION POS(3),VEL(3)
      COMMON/CTE/GM,GM0
      DO 410 I=1,NPLA
C IF I WAS ELIMINATED
       	IF(MPLA(I).EQ.1.2345D-30) THEN
           GOTO 410
        ENDIF
	R0=RPLA(I)
	V02=VPLA(I,1)*VPLA(I,1)+VPLA(I,2)*VPLA(I,2)+VPLA(I,3)*VPLA(I,3)
        UA0=2.D0/R0-V02/(GM0+MPLA(I))
        A0=1.D0/UA0
        IF(A0.GT.99999.0.OR.A0.LT.0.0) THEN
C WE ELIMINATE THE BODY PUTTING AN ABSURDE MASS
          MPLA(I)=1.2345D-30
          APLA(I)=90000.D0
          WRITE(*,*) "BODY ELIMINATED: ",I
          GOTO 410
        ENDIF
	DO L=1,3
	  POS(L)=XPLA(I,L)
	  VEL(L)=VPLA(I,L)
	ENDDO
	ALPM=MPLA(I)
	CALL MOVREL(GM0,H,POS,VEL,R0,E0,ALPM)
	RPLA(I)=R0
	APLA(I)=A0
	EPLA(I)=E0
	DO L=1,3
	  XPLA(I,L)=POS(L)
	  VPLA(I,L)=VEL(L)
	ENDDO
410   CONTINUE
      RETURN
      END
C ========================================================================
C ========================================================================
C COMPUTE MUTUAL PERTURBATIONS IN ONE STEP
      SUBROUTINE PERTURB(XPLA,VPLA,RPLA,NPLA,MPLA,H)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 MPLA
      parameter (NTT=100)
      DIMENSION XPLA(NTT,3),VPLA(NTT,3),RPLA(NTT)
      DIMENSION MPLA(NTT)
      DIMENSION FINDX(NTT),FINDY(NTT),FINDZ(NTT)
      DO 10 I=1,NPLA
C IF I WAS ELIMINATED
       	IF(MPLA(I).EQ.1.2345D-30) THEN
           GOTO 10
        ENDIF
	XGM=MPLA(I)
	RP3=XGM/RPLA(I)**3
	FINDX(I)=XPLA(I,1)*RP3
	FINDY(I)=XPLA(I,2)*RP3
	FINDZ(I)=XPLA(I,3)*RP3
 10   continue
      DO 20 I=1,NPLA
C IF I WAS ELIMINATED
       	IF(MPLA(I).EQ.1.2345D-30) THEN
           GOTO 20
        ENDIF
        DO 30 J=I+1,NPLA
C IF J WAS ELIMINATED
          IF(MPLA(J).EQ.1.2345D-30) THEN
            GOTO 30
          ENDIF
	  DX=XPLA(I,1)-XPLA(J,1)
	  DY=XPLA(I,2)-XPLA(J,2)
	  DZ=XPLA(I,3)-XPLA(J,3)
	  DRQ=DX*DX+DY*DY+DZ*DZ
	  DIJ3=DSQRT(DRQ)**3
	  DX=DX/DIJ3
	  DY=DY/DIJ3
	  DZ=DZ/DIJ3
	  PIJX=MPLA(I)*DX-FINDX(I)
	  PIJY=MPLA(I)*DY-FINDY(I)
	  PIJZ=MPLA(I)*DZ-FINDZ(I)
	  VPLA(J,1)=VPLA(J,1)+H*PIJX
	  VPLA(J,2)=VPLA(J,2)+H*PIJY
	  VPLA(J,3)=VPLA(J,3)+H*PIJZ
	  PJIX=-MPLA(J)*DX-FINDX(J)
	  PJIY=-MPLA(J)*DY-FINDY(J)
	  PJIZ=-MPLA(J)*DZ-FINDZ(J)
	  VPLA(I,1)=VPLA(I,1)+H*PJIX
	  VPLA(I,2)=VPLA(I,2)+H*PJIY
	  VPLA(I,3)=VPLA(I,3)+H*PJIZ
 30     continue
 20   continue
      RETURN
      END
C ========================================================================
C ========================================================================
C SOLVING KEPLER EQUATION
      SUBROUTINE SOLKEP(EX,M,E)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 M,MK
      TOLE=1.D-14
      DOSPI=8.D0*DATAN(1.D0)
      M=DMOD(M,DOSPI)
      E=M
 100  E0=E
      SE=DSIN(E0)
      CE=DCOS(E0)
      ES=EX*SE
      EC=1.D0-EX*CE
      MK=E0-ES
      U=(MK-M)/EC
      XPRI=E0-U
      XSEG=E0-U/(1.D0-U*ES)
      E=(XPRI+XSEG)/2.D0
      DEX=DABS(E-E0)
      IF(DEX.GT.TOLE)GOTO 100
	RETURN
      END
C ========================================================================
C ========================================================================
C CALCULATING ORBITAL ELEMENTS
      SUBROUTINE PLANO(XPLA,VPLA,I,INCLI,DOSPI,GM0,MPLA,ON,W,AMED,SE,SA)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 INCLI,MPLA
      parameter (NTT=100)
      DIMENSION XPLA(NTT,3),VPLA(NTT,3),MPLA(NTT)
      ERR=1.D-12
      XX=XPLA(I,1)
      YY=XPLA(I,2)
      ZZ=XPLA(I,3)
      R=DSQRT(XX*XX+YY*YY+ZZ*ZZ)
      VX=VPLA(I,1)
      VY=VPLA(I,2)
      VZ=VPLA(I,3)
      V2=VX*VX+VY*VY+VZ*VZ
      HX=YY*VZ-ZZ*VY
      HY=ZZ*VX-XX*VZ
      HZ=XX*VY-YY*VX
      HHXY=DSQRT(HX*HX+HY*HY)
      YN=DATAN2(HHXY,hz)
      INCLI=YN
      ON=DATAN2(HX,-HY)
      IF(ON.LT.0D0) THEN
        ON=ON+DOSPI
      END IF
      CMU=GM0+MPLA(I)
      EX=(VY*HZ-VZ*HY)/CMU-XX/R
      EY=(VZ*HX-VX*HZ)/CMU-YY/R
      EZ=(VX*HY-VY*HX)/CMU-ZZ/R
      EE=DSQRT(EX*EX+EY*EY+EZ*EZ)
      SE=EE
C IF INCLINATION IS 0 OR 180
C IN THIS CASE W IS THE LONGITUDE OF PERIHELION
      IF (SIN(YN).LT.ERR) THEN
        ON=0.D0
        W=DATAN2(EY,EX)
        ELSE
        SEW=EZ/DSIN(YN)
        COW=EX*DCOS(ON)+EY*DSIN(ON)
        W=DATAN2(SEW,COW)
      ENDIF
      IF (W.LT.0D0) THEN
        W=W+DOSPI
      END IF
      A=1.D0/(2.D0/R-V2/CMU)
      IF (A.GT.99999.0.OR.A.LT.0.0) THEN
C WE ELIMINATE THE BODY PUTTING AN ABSURDE MASS
          MPLA(I)=1.2345D-30
          SA=90000.D0
          WRITE(*,*) "BODY ELIMINATED: ",I
          RETURN
      ENDIF
      SA=A
      AE=0D0
      IF (A.GT.0.D0) THEN
        SEE=(XX*VX+YY*VY+ZZ*VZ)/DSQRT(A*CMU)
        COE=1.D0-R/A
        AE=DATAN2(SEE,COE)
        IF (AE.LT.0D0) THEN
          AE=AE+DOSPI
        END IF
        AMED=AE-EE*DSIN(AE)
        AMED=DMOD(AMED,DOSPI)
        IF (AMED.LT.0D0) THEN
          AMED=AMED+DOSPI
        END IF
      END IF
      INCLI=INCLI/DOSPI*36.D1
      ON=ON/DOSPI*36.D1
      W=W/DOSPI*36.D1
      AMED=AMED/DOSPI*36.D1
      RETURN
      END
C ========================================================================
C ========================================================================
C COMPUTE INITIAL POSITION AND VELOCITY
      SUBROUTINE POVE(A,E,I,ANODE,PERI,M,N,X,XP)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 I,M,N
      DIMENSION X(3),XP(3)
      D2=2.D0
      CALL SOLKEP(E,M,EX)
      V=D2*DATAN(DSQRT((1.D0+E)/(1.D0-E))*(DSIN(EX/D2)/DCOS(EX/D2)))
      R=A*(1.D0-E*DCOS(EX))
      BETA=N*A/DSQRT(1.D0-E*E)
      X1=R*DCOS(V)
      X2=R*DSIN(V)
      XP1=-BETA*DSIN(V)
      XP2= BETA*(E+DCOS(V))
      CK=DCOS(ANODE)
      SK=DSIN(ANODE)
      CI=DCOS(I)
      SI=DSIN(I)
      CO=DCOS(PERI)
      SO=DSIN(PERI)
C VECTORS P AND Q
      P1=CK*CO-SK*CI*SO
      P2=SK*CO+CK*CI*SO
      P3=SI*SO
      Q1=-CK*SO-SK*CI*CO
      Q2=-SK*SO+CK*CI*CO
      Q3= SI*CO
C POSITION AND VELOCITY
      X(1)=P1*X1+Q1*X2
      X(2)=P2*X1+Q2*X2
      X(3)=P3*X1+Q3*X2
      XP(1)=P1*XP1+Q1*XP2
      XP(2)=P2*XP1+Q2*XP2
      XP(3)=P3*XP1+Q3*XP2
      RETURN
      END
C ========================================================================
C ========================================================================
      SUBROUTINE MOVREL(GM0,DT,X,V,R,E,ALPM)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(3),V(3)
      GGM=GM0+ALPM
      VQ=V(1)*V(1)+V(2)*V(2)+V(3)*V(3)
      A=1.D0/((2.D0/R)-VQ/GGM)
      EC=1.D0-R/A
      U=X(1)*V(1)+X(2)*V(2)+X(3)*V(3)
      EN=SQRT(GGM/(A*A*A))
      ES=U/(EN*A*A)
      E=DSQRT(EC*EC+ES*ES)
      CALL KEPREL(ES,EC,EN,DT,DX,C,S,FP)
      F=(A/R)*(C-1.D0)+1.D0
      G=DT+(S-DX)/EN
      FDOT=-A*EN*S/(R*FP)
      GDOT=1.D0+(C-1.D0)/FP
      X1=X(1)*F+V(1)*G
      X2=X(2)*F+V(2)*G
      X3=X(3)*F+V(3)*G
      V1=X(1)*FDOT+V(1)*GDOT
      V2=X(2)*FDOT+V(2)*GDOT
      V3=X(3)*FDOT+V(3)*GDOT
      X(1)=X1
      X(2)=X2
      X(3)=X3
      V(1)=V1
      V(2)=V2
      V(3)=V3
      R=DSQRT(X(1)*X(1)+X(2)*X(2)+X(3)*X(3))
      RETURN
      END
C ========================================================================
C ========================================================================
C SOLVING KEPLER EQUATION FOR DT
      SUBROUTINE KEPREL(ES,EC,EN,DT,X,C,S,FP)
      IMPLICIT REAL*8(A-H,O-Z)
      TOL=1.D-13
      X=EN*DT
100   S=DSIN(X)
      C=DCOS(X)
      F=X-EC*S+ES*(1.D0-C)-ENDT
      FP=1.D0-EC*C+ES*S
      FPP=EC*S+ES*C
      FPPP=EC*C-ES*S
      DX=-F/FP
      DX=-F/(FP+DX*FPP/2.D0)
      DX=-F/(FP+DX*FPP/2.D0+DX*DX*FPPP/6.D0)
      X=X+DX
      IF(DABS(DX).GT.TOL)GOTO 100
      RETURN
      END
C ========================================================================
C ========================================================================
