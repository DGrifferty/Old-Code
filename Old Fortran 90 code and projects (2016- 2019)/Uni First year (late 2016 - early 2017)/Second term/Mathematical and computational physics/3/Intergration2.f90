!--------------------------------------------------
!PHY1038 Assignment 3 Finite Difference Methods
!URN: 6408056
!February 2017
!--------------------------------------------------

PROGRAM	StepsEightAndNine
	
INTEGER, PARAMETER:: DP = 2 
INTEGER :: i
REAL(KIND = DP), EXTERNAL:: Fx, Itrapeziumrule
REAL(KIND = DP) :: X, A, B, Fa, Fb, Itrapeziumruletwo, P


INTERFACE
  
  FUNCTION Fx(X)
    REAL(KIND = 2), INTENT (IN) :: X
    REAL(KIND = 2) :: Fx
  END FUNCTION

  FUNCTION Itrapeziumrule(A, B, N)
  INTEGER, INTENT (IN) :: N
  REAL(KIND = 2), INTENT (IN) :: A, B
  REAL(KIND = 2), EXTERNAL :: Fx
  REAL(KIND = 2) :: Itrapeziumrule, Fa, Fb, X, P
  END FUNCTION
  
END INTERFACE
 



X=2
N=40
A=1
B=0
IF (A>B) THEN
  P=A; A=B; B=P
END IF
 
WRITE(6,*) Fx(X)
WRITE(6,*) Itrapeziumrule (A, B, N)


END PROGRAM  StepsEightAndNine

FUNCTION Fx(X)
  IMPLICIT NONE
  REAL(KIND = 2), INTENT (IN) :: X
  REAL(KIND = 2) :: Fx
  
  Fx = 1/(X**2.0+3.0*X+2)

END FUNCTION
  
 
  
FUNCTION Itrapeziumrule(A, B, N)
  INTEGER, INTENT (IN) :: N
  INTEGER :: i
  REAL(KIND = 2), INTENT (IN) :: A, B
  REAL(KIND = 2), EXTERNAL :: Fx
  REAL(KIND = 2) :: Itrapeziumrule, Fa, Fb, X, H, Itrapeziumruletwo
  
  Itrapeziumruletwo=0

  X = A; Fa = Fx(X); X = B; Fb = Fx(X); H=abs(B-A)/N    

  DO i=1,N-1
    X = (A + (i*H)); Itrapeziumruletwo = (H*Fx(X))+ Itrapeziumruletwo
  END DO
  
  Itrapeziumrule=(H/2)*(Fa + Fb)+Itrapeziumruletwo
    
END FUNCTION
  
 
 

  

  