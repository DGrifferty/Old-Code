!--------------------------------------------------
!PHY1038 Assignment 3 Finite Difference Methods
!URN: 6408056
!February 2017
!--------------------------------------------------

PROGRAM Finite_Difference_Methods

REAL(KIND = 2):: X, H, YatX, YatXplusH, P, YatXminusP, YatXplusP, Xorg, XplusP, XminusP, Q, YatXminusH
REAL(KIND = 2), EXTERNAL:: Fx, Dforxh, DCenxh, ExactDerivativeAtX, DCentTwoxh
INTEGER, PARAMETER:: DP = 2 !Set so transfers to functions!

INTERFACE
    
  FUNCTION Fx(X)  
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

END INTERFACE

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

END PROGRAM Finite_Difference_Methods

FUNCTION Fx(X)
  IMPLICIT NONE
  REAL(KIND = 2), INTENT (IN) :: X
  REAL(KIND = 2):: Fx
  Fx=1/(X**2.0+3.0*X+2)
END FUNCTION Fx

FUNCTION DForxh(X, H, Xorg)
  IMPLICIT NONE 
  REAL(KIND = 2), INTENT(IN) :: H, Xorg
  REAL(KIND = 2), EXTERNAL:: Fx
  REAL(KIND = 2):: DForxh, YatX, YatXplusH, X
  YatX=Fx(X)
  X=X+H
  YatXplusH=Fx(X)
  Dforxh=(YatXplusH-YatX)/H
  X=Xorg
END FUNCTION DForxh

FUNCTION DCenxh(X, H, Xorg)
  IMPLICIT NONE 
  REAL(KIND = 2), INTENT(IN) :: H, Xorg
  REAL(KIND = 2), EXTERNAL:: Fx
  REAL(KIND = 2):: DCenxh, YatXminusP, YatXplusP, X, P

  P=H/2
  X=X-P
  YatXminusP=Fx(X)
  X=X+2*P
  YatXplusP=Fx(X)
  DCenxh=(YatXplusP-YatXminusP)/H
  X=Xorg
END FUNCTION DCenxh

FUNCTION ExactDerivativeAtX(X)
  IMPLICIT NONE
  REAL(KIND = 2), INTENT(IN) :: X
  REAL(KIND = 2):: ExactDerivativeAtX
 
  ExactDerivativeAtX=((-2.0)*(X)-3.0)/((X**(2.0)+(3.0*X)+2.0)**2.0)

END FUNCTION  ExactDerivativeAtX

FUNCTION DCentTwoxh(X, H, Xorg)
  IMPLICIT NONE 
  REAL(KIND = 2), INTENT(IN) :: H, Xorg
  REAL(KIND = 2), EXTERNAL:: Fx
  REAL(KIND = 2):: DCentTwoxh, YatX, YatXplusH, YatXminusH, X

  YatX=Fx(X)
  X=X+H
  YatXplusH=Fx(X)
  X=X-(2*H)
  YatXminusH=Fx(X)
  DCentTwoxh=(YatXplusH-(2*YatX)+YatXminusH)/(H**2)
  X=Xorg

END FUNCTION 