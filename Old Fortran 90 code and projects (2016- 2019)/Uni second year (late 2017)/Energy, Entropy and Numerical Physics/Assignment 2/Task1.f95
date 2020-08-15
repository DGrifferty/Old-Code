PROGRAM Task1
IMPLICIT NONE
    
INTEGER, PARAMETER:: n=80
INTEGER:: i, j, it
REAL:: Epsilon0, EpsilonRH, KBoltz, CSalt, e, Kappa, OldVal, X, Y
REAL:: Phi(-n:n, -n:n), h, tol
 
Epsilon0=8.85E-12; EpsilonRH=80; KBoltz = 1.38-E13; h=0.05; tol=1.0E-5; Phi = 0.0; CSalt=3E26
 
!Kappa = SQRT((2.0*CSalt)*e**2.0)/(Epsilon0*EpsilonRH*KBoltz)
Kappa=1.0
WRITE(6,*) Kappa
Kappa = Kappa/1.0E9
 

WRITE(6,*) 1.0/Kappa

DO i=-n,n
  DO j = -n, n
    X = REAL(i)*h; Y = REAL(j)*h
    IF ((X**2.0+Y**2.0) <= 1.0**(2.0+tol)) THEN
      PHI(i,j) = 100.0
    END IF
  END DO
END DO

DO it = 1, 100000
  DO i = -n+1,n-1
    DO j = -n+1, n-1
      X=REAL(i)*h; Y=REAL(j)*h; OldVal = Phi(i,j)
      IF ((X**2.0+Y**2.0) <= 1.0**(2.0+tol)) THEN
        !We are inside the cylinder
			ELSE
        Phi(i,j) = (Phi(i-1, j)+Phi(i,j+1)+Phi(i,j+1)+Phi(i,j-1))/(4.0+h**2.0+Kappa**2.0)
			END IF

	  END DO
  END DO
END DO

OPEN(1, FILE = 'Results.dat')  
DO i = -n,n
  DO j = -n,n
    WRITE(1,'(f4.2, f4.2, f7.4)') REAL(i)*h, REAL(j)*h, Phi(i,j)
  END DO
END DO

          
 END PROGRAM