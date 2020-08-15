!--------------------------------------------------
!PHY1038 Assignment 3 Finite Difference Methods
!URN: 6408056
!February 2017
!--------------------------------------------------

PROGRAM	StepsEightAndNine
	
INTEGER, PARAMETER:: DP = 2 
INTEGER :: i
REAL(KIND = DP), EXTERNAL:: Fx, Itrapeziumrule
REAL(KIND = DP) :: X, A, B, Fa, Fb, Itrapeziumruletwo, ItrapeziumruleP


INTERFACE
  
  FUNCTION Fx(X)
    REAL(KIND = 2), INTENT (IN) :: X
    REAL(KIND = 2) :: Fx
  END FUNCTION

  FUNCTION Itrapeziumrule(A, B, N)
  INTEGER, INTENT (IN) :: N
  REAL(KIND = 2), INTENT (IN) :: A, B
  REAL(KIND = 2), EXTERNAL :: Fx
  REAL(KIND = 2) :: Itrapeziumrule, Fa, Fb, X
  END FUNCTION
  
END INTERFACE
 



X=2
N=300
A=1
B=0
!make sure B>A
 
WRITE(6,*) Fx(X)



  Itrapeziumruletwo=0

  X = A
  Fa = Fx(X)
  WRITE(6,*) 'FA', Fa
  X = B
  Fb = Fx(X)!can do outside function?
  WRITE(6,*) 'FB', Fb
  H=abs(B-A) !nessescary?
  WRITE(6,*) 'H', H
  ItrapeziumruleP =  (H/2)*(Fa + Fb)
  WRITE(6,*) 'ITRAP1', ItrapeziumruleP

  DO WHILE (A+ih<=B)
    X = A + (i*H)
    Itrapeziumruletwo = Fx(x)+ Itrapeziumruletwo
    WRITE(6,*) 'ITRAP', i, 'HERE', Itrapeziumruletwo
  END DO
  
  Itrapeziumruletwo=H*Itrapeziumruletwo
  
  ItrapeziumruleP=ItrapeziumruleP+Itrapeziumruletwo













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

  X = A
  Fa = Fx(X)
  X = B
  Fb = Fx(X)!can do outside function?
  H=abs(B-A) !nessescary?
  Itrapeziumrule =  (H/2)*(Fa + Fb)

  DO i=1,N-1
    X = A + (i*H)
    Itrapeziumruletwo = Fx(x)+ Itrapeziumruletwo
  END DO
  
  Itrapeziumruletwo=H*Itrapeziumruletwo
  
  Itrapeziumrule=Itrapeziumrule+Itrapeziumruletwo
    
END FUNCTION
  
 
 

  

  