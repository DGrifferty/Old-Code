PROGRAM BUGS
REAL(KIND=2):: X
INTEGER::i, N
REAL(KIND = 2), EXTERNAL:: Fx
REAL(KIND = 2) :: X, A, B, Fa, Fb, Itrapeziumruletwo, Itrapeziumrule


  
INTERFACE
  
  FUNCTION Fx(X)
    REAL(KIND = 2), INTENT (IN) :: X
    REAL(KIND = 2) :: Fx
  END FUNCTION

END INTERFACE
 




X=2
N=40
A=0
B=1

 
  Itrapeziumruletwo=0

  X = A
  Fa = Fx(X)
  X = B
  Fb = Fx(X)
  H=abs(B-A)/N 
  Itrapeziumrule =  (H/2)*(Fa + Fb)
  
  DO i=1,N-1
    X = (A + (i*H))
    Itrapeziumruletwo = Fx(x)+ Itrapeziumruletwo
    WRITE(6,*) Itrapeziumruletwo
  END DO
    
  Itrapeziumruletwo=H*Itrapeziumruletwo
  
  Itrapeziumrule=(H/2)*(Fa + Fb)+Itrapeziumruletwo

  WRITE(6,*) Itrapeziumrule
  
  
  end program


  FUNCTION Fx(X)
  IMPLICIT NONE
  REAL(KIND = 2), INTENT (IN) :: X
  REAL(KIND = 2) :: Fx
  
  Fx = 1/(X**2.0+3.0*X+2)

END FUNCTION
  