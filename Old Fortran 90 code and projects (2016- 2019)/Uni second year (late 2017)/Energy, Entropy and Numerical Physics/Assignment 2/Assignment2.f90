PROGRAM Assignment2
IMPLICIT NONE

INTEGER:: i, j, k, l, HalfMaxPos_Neg, HalfMaxPos_Pos
INTEGER, PARAMETER:: n=300
REAL:: DiffC, H, dt, TEnd, HalfMax, Errorpos, Errorneg, FWHM  
double precision:: Temp(-n:n)=0.0, d2Tempdx2(-n:n), dTempdt(-n:n)=0.0
DiffC = 1.1; H = 0.1; tend = 10.0; dt=1E-4

DO i = -10, 10 !Creating Array of initial temp.
  Temp(i) = 20.0
END DO

OPEN(12, FILE = 'FWHM.dat')
WRITE(12,*) 'p ''FWHM.dat'' u 1:2 w l'

DO k = 0, Tend/dt !From 0 to 10 s in 100000 steps
  DO i = -n+1, n-1 !calculates the rate of change in temp for all x's
      dTempdt(i) = DiffC*(Temp(i+1)-2.0*Temp(i)+Temp(i-1))/H**2 
  END DO
  DO j = -n+1, n-1 !calculates new temp for all x's
      Temp(j) = Temp(j)+dt*dtempdt(J) !new temp = rate of change in temp * change in time
  END DO
  HalfMax = Temp(0)/2; Errorpos =1; ErrorNeg = 1
  DO l = -n+1, n-1
    IF (l >= -n+1 .and. l < 0 .and. abs(HalfMax-Temp(l)) <= Errorneg ) THEN
       Errorneg = abs(HalfMax-Temp(l)); HalfMaxPos_Neg = L
       !WRITE(6,*) 'HalfMaxPos_Neg', HalfMaxPos_Neg
    ELSE IF (l <= n+1 .and. l>0 .and. abs(HalfMax-Temp(l)) <= Errorpos ) THEN
       Errorpos = abs(HalfMax-Temp(l)) ; HalfMaxPos_Pos = L
       !WRITE(6,*) 'HalfMaxPos_pos', HalfMaxPos_pos
    END IF
  END DO  
   FWHM= REAL(HalfMaxPos_Pos-HalfMaxPos_Neg) * H
       
WRITE(6,*) Temp(0), 'fwhm', FWHM, Errorpos, errorneg, halfmax

WRITE(12,*) K, FWHM
CLOSE(12)
END DO

OPEN (11, FILE = 'Results.dat')
WRITE(11,*) 'p ''Results.dat'' u 1:2 w l'
  DO i = -n+1, n-1 !Prints results to file 
    WRITE(11,*) i*0.1, Temp(i)
  END DO
CLOSE(11)

WRITE(6,*) 'Results, X, Temp, printed to file ''Results.dat''.'

END PROGRAM