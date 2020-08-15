!--------------------------------------------------
!PHY1038 Assignment 3 Finite Difference Methods
!URN: 6408056
!February 2017
!--------------------------------------------------

PROGRAM	StepsEightAndNine
	
INTEGER, PARAMETER:: DP = 2 
INTEGER :: i
REAL(KIND = DP), EXTERNAL:: Fx, ItrapeziumRule, Iexact
REAL(KIND = DP) :: X, A, B, Fa, Fb, ItrapeziumRuleTwo, P, ERROR


INTERFACE
  
  FUNCTION Fx(X)
    REAL(KIND = 2), INTENT (IN) :: X
    REAL(KIND = 2) :: Fx
  END FUNCTION

  FUNCTION ItrapeziumRule(A, B, N)
  INTEGER, INTENT (IN) :: N
  REAL(KIND = 2), INTENT (IN) :: A, B
  REAL(KIND = 2), EXTERNAL :: Fx
  REAL(KIND = 2) :: ItrapeziumRule, Fa, Fb, X, P
  END FUNCTION

  Function Iexact(A, B)
  REAL(KIND=2), INTENT (IN) :: A, B
  REAL(KIND=2) :: Iexact
  END FUNCTION Iexact
   
END INTERFACE
 



X=2
N=40
A=1
B=0
IF (A>B) THEN
  P = A; A = B; B = P
  !remember to take negative later ie fig=g
END IF
 
WRITE(6,*) Fx(X)
WRITE(6,*) ItrapeziumRule (A, B, N)
WRITE(6,*) Iexact(A, B)
ERROR = Iexact(A, B) - ItrapeziumRule (A, B, N)
WRITE(6,*) 'Error with',n,' strips =', ERROR



END PROGRAM  StepsEightAndNine


FUNCTION Fx(X)
  IMPLICIT NONE
  REAL(KIND = 2), INTENT (IN) :: X
  REAL(KIND = 2) :: Fx
  
  Fx = 1/(X**2.0+3.0*X+2)

END FUNCTION
  
 
  
FUNCTION ItrapeziumRule(A, B, N)
  INTEGER, INTENT (IN) :: N
  INTEGER :: i
  REAL(KIND = 2), INTENT (IN) :: A, B
  REAL(KIND = 2), EXTERNAL :: Fx
  REAL(KIND = 2) :: ItrapeziumRule, Fa, Fb, X, H, ItrapeziumRuleTwo
  
  ItrapeziumRuletwo=0

  X = A; Fa = Fx(X); X = B; Fb = Fx(X); H=abs(B-A)/N    

  DO i=1,N-1
    X = (A + (i*H)); ItrapeziumRuletwo = (H*Fx(X))+ ItrapeziumRuleTwo
  END DO
  
  ItrapeziumRule=(H/2)*(Fa + Fb)+ItrapeziumRuleTwo
    
END FUNCTION
  

FUNCTION Iexact(A, B)
  REAL(KIND=2), INTENT (IN) :: A, B
  REAL(KIND=2) :: Iexact

  Iexact = log((B+1)/(B+2))-log((A+1)/(A+2))

END FUNCTION

  

  