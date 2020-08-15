!--------------------------------------------------
!PHY1038 Assignment 3 Finite Difference Methods
!URN: 6408056
!February 2017
!--------------------------------------------------

PROGRAM Finite_Difference_Methods

INTEGER, PARAMETER:: DP = 2 !Set so transfers to functions!
INTEGER :: i
REAL(KIND = DP):: X, H, YatX, YatXplusH, P, YatXminusP, YatXplusP, Xorg, XplusP, XminusP, Q, YatXminusH, A, B, Fa, Fb, Hi
REAL(KIND = DP) :: ItrapeziumRuleTwo, ERROR
REAL(KIND = DP), EXTERNAL:: Fx, Dforxh, DCenxh, ExactDerivativeAtX, DCentTwoxh, ItrapeziumRule, Iexact

INTERFACE !use module
    
  FUNCTION Fx(X)  !removex, xh
    REAL(KIND = DP), INTENT(IN) :: X
    REAL(KIND = DP):: Fx    
  END FUNCTION  Fx

  FUNCTION DForxh(X, H, Xorg)
    REAL(KIND = DP), INTENT(IN) :: H, Xorg
    REAL(KIND = DP):: DForxh, YatX, YatXplusH, X
    REAL(KIND = DP), EXTERNAL:: Fx
  END FUNCTION DForxh

  FUNCTION DCenxh(X, H, Xorg)
    REAL(KIND = DP), INTENT(IN) :: H, Xorg
    REAL(KIND = DP):: DCenxh, YatXminusP, YatXplusP, X, P
    REAL(KIND = DP), EXTERNAL:: Fx
  END FUNCTION DCenxh

  FUNCTION ExactDerivativeAtX(X)
    REAL(KIND = DP), INTENT(IN) :: X
    REAL(KIND = DP):: ExactDerivativeAtX
  END FUNCTION  ExactDerivativeAtX
 
  FUNCTION DCentTwoxh(X, H, Xorg)
    REAL(KIND = DP), INTENT(IN) :: H, Xorg
    REAL(KIND = DP), EXTERNAL:: Fx
    REAL(KIND = DP):: DCentTwoxh, YatX, YatXplusH, YatXminusH, X
  END FUNCTION 

  FUNCTION ItrapeziumRule(A, B, N)
  INTEGER, INTENT (IN) :: N
  INTEGER :: i
  REAL(KIND = 2), INTENT (IN) :: A, B
  REAL(KIND = 2), EXTERNAL :: Fx
  REAL(KIND = 2) :: ItrapeziumRule, Fa, Fb, X, Hi, ItrapeziumRuleTwo
  END FUNCTION

  FUNCTION Iexact(A, B)
  REAL(KIND=2), INTENT (IN) :: A, B
  REAL(KIND=2) :: Iexact
  END FUNCTION Iexact

  FUNCTION ISimpson (A, B, N)
  REAL(KIND=2), INTENT(IN) :: A, B, N
  REAL(KIND=2) :: ISimpson
  END FUNCTION ISimpson

END INTERFACE

X=2
N=40
A=1
B=0
IF (A>B) THEN
  P = A; A = B; B = P
  !remember to take negative later ie fig=g
END IF

READ(5,*) X
Xorg=X
READ(5,*) H

WRITE(6,*) 'Value of y', Fx(X)
WRITE(6,*) 'X=', X
WRITE(6,*) 'Value from =h', DForxh(X, H, Xorg)
WRITE(6,*) 'X=', X
WRITE(6,*) 'Value from =/-p', DCenxh(X, H, Xorg)
WRITE(6,*) 'X=', X
WRITE(6,*) 'Value of exact', ExactDerivativeAtX(X)
WRITE(6,*) 'X=', X
WRITE(6,*) 'Value of d2', DCentTwoxh(X, H, Xorg)
WRITE(6,*) 'X=', X
WRITE(6,*) Fx(X)
WRITE(6,*) ItrapeziumRule (A, B, N)
WRITE(6,*) Iexact(A, B)
ERROR = Iexact(A, B) - ItrapeziumRule (A, B, N)
WRITE(6,*) 'Error with',n,' strips =', ERROR


END PROGRAM Finite_Difference_Methods




FUNCTION Fx(X)
  IMPLICIT NONE
  REAL(KIND = 2), INTENT (IN) :: X
  REAL(KIND = 2):: Fx
  Fx = 1/(X**2.0+3.0*X+2)
END FUNCTION Fx

FUNCTION DForxh(X, H, Xorg)
  IMPLICIT NONE 
  REAL(KIND = 2), INTENT(IN) :: H, Xorg
  REAL(KIND = 2), EXTERNAL:: Fx
  REAL(KIND = 2):: DForxh, YatX, YatXplusH, X
  YatX = Fx(X)
  X = X+H
  YatXplusH = Fx(X)
  Dforxh = (YatXplusH-YatX)/H
  X = Xorg
END FUNCTION DForxh

FUNCTION DCenxh(X, H, Xorg)
  IMPLICIT NONE 
  REAL(KIND = 2), INTENT(IN) :: H, Xorg
  REAL(KIND = 2), EXTERNAL:: Fx
  REAL(KIND = 2):: DCenxh, YatXminusP, YatXplusP, X, P

  P = H/2
  X = X-P
  YatXminusP = Fx(X)
  X = X+2*P
  YatXplusP = Fx(X)
  DCenxh = (YatXplusP-YatXminusP)/H
  X = Xorg
END FUNCTION DCenxh

FUNCTION ExactDerivativeAtX(X)
  IMPLICIT NONE
  REAL(KIND = 2), INTENT(IN) :: X
  REAL(KIND = 2):: ExactDerivativeAtX
 
  ExactDerivativeAtX = ((-2.0)*(X)-3.0)/((X**(2.0)+(3.0*X)+2.0)**2.0)

END FUNCTION  ExactDerivativeAtX

FUNCTION DCentTwoxh(X, H, Xorg)
  IMPLICIT NONE 
  REAL(KIND = 2), INTENT(IN) :: H, Xorg
  REAL(KIND = 2), EXTERNAL:: Fx
  REAL(KIND = 2):: DCentTwoxh, YatX, YatXplusH, YatXminusH, X

  YatX = Fx(X)
  X = X+H
  YatXplusH = Fx(X)
  X = X-(2*H)
  YatXminusH = Fx(X)
  DCentTwoxh = (YatXplusH-(2*YatX)+YatXminusH)/(H**2)
  X = Xorg

END FUNCTION 

FUNCTION ItrapeziumRule(A, B, N)
  INTEGER, INTENT (IN) :: N
  INTEGER :: i
  REAL(KIND = 2), INTENT (IN) :: A, B
  REAL(KIND = 2), EXTERNAL :: Fx
  REAL(KIND = 2) :: ItrapeziumRule, Fa, Fb, X, Hi, ItrapeziumRuleTwo
  
  ItrapeziumRuletwo=0

  X = A; Fa = Fx(X); X = B; Fb = Fx(X); Hi=abs(B-A)/N    

  DO i=1,N-1
    X = (A + (i*Hi)); ItrapeziumRuletwo = (Hi*Fx(X))+ ItrapeziumRuleTwo
  END DO
  
  ItrapeziumRule=(Hi/2)*(Fa + Fb)+ItrapeziumRuleTwo
    
END FUNCTION

FUNCTION Iexact(A, B)
  REAL(KIND=2), INTENT (IN) :: A, B
  REAL(KIND=2) :: Iexact

  Iexact = log((B+1)/(B+2))-log((A+1)/(A+2))

END FUNCTION

FUNCTION ISimpson

