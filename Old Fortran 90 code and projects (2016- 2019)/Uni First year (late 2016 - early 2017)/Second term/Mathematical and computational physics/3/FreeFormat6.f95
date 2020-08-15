PROGRAM FINITEMAINPART
USE FUNCTIONS
IMPLICIT NONE 
REAL:: X, H, YatX, YatXplusH, P, YatXminusP, YatXplusP, Xorg



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
END PROGRAM


MODULE FUNCTIONS
IMPLICIT NONE 
PUBLIC :: Fx, Dforxh, DCenxh, ExactDerivativeAtX
REAL:: X, H, Xorg
CONTAINS


REAL FUNCTION Fx(X)
IMPLICIT NONE
REAL, INTENT (IN) :: X
REAL:: Fx
Fx=1/(X**2.0+3.0*X+2)
END FUNCTION Fx

REAL FUNCTION DForxh(X, H, Xorg)
IMPLICIT NONE 
REAL, INTENT(IN) :: H, Xorg
REAL, EXTERNAL:: Fx
REAL:: DForxh, YatX, YatXplusH, X
YatX=Fx(X)
X=X+H
YatXplusH=Fx(X)
Dforxh=(YatXplusH-YatX)/H
X=Xorg
END FUNCTION DForxh

REAL FUNCTION DCenxh(X, H, Xorg)
IMPLICIT NONE 
REAL, INTENT(IN) :: H, Xorg
REAL:: DCenxh, YatXminusP, YatXplusP, X, P
REAL, EXTERNAL:: Fx

P=H/2
X=X-P
YatXminusP=Fx(X)
X=X+2*P
YatXplusP=Fx(X)
DCenxh=(YatXplusP-YatXminusP)/H
X=Xorg
END FUNCTION DCenxh

REAL FUNCTION ExactDerivativeAtX(X)
IMPLICIT NONE
REAL, INTENT(IN) :: X
REAL:: ExactDerivativeAtX
 
ExactDerivativeAtX=((-2.0)*(X)-3.0)/((X**(2.0)+(3.0*X)+2.0)**2.0)

END FUNCTION  ExactDerivativeAtX
END MODULE FUNCTIONS
