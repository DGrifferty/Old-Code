PROGRAM FINITEMAINPART

REAL:: X, H, YatX, YatXplusH, P, YatXminusP, YatXplusP
REAL, EXTERNAL:: Fx, Dforxh, DCenxh, ExactDerivativeAtX

INTERFACE 
FUNCTION Fx(X)
REAL, INTENT(IN) :: X
REAL:: Fx
END FUNCTION  Fx
END INTERFACE

INTERFACE 
FUNCTION DForxh(X, H)
REAL, INTENT(IN) :: H
REAL:: DForxh, YatX, YatXplusH, X
REAL, EXTERNAL:: Fx
END FUNCTION DForxh
END INTERFACE

INTERFACE
FUNCTION DCenxh(X, H)
REAL, INTENT(IN) :: H
REAL:: DCenxh, YatXminusP, YatXplusP, X, P
REAL, EXTERNAL:: Fx
END FUNCTION DCenxh
END INTERFACE

INTERFACE
FUNCTION ExactDerivativeAtX(X)
REAL, INTENT(IN) :: X
REAL:: ExactDerivativeAtX
END FUNCTION  ExactDerivativeAtX
END INTERFACE


H=0.01

READ(5,*) X
WRITE(6,*)  Fx(X)
WRITE(6,*) DForxh(X, H)
WRITE(6,*) DCenxh(X, H)
WRITE(6,*) ExactDerivativeAtX(X)
END PROGRAM

FUNCTION Fx(X)
IMPLICIT NONE
REAL, INTENT (IN) :: X
REAL:: Fx
Fx=1/(X**2.0+3.0*X+2)
END FUNCTION Fx

FUNCTION DForxh(X, H)
IMPLICIT NONE 
REAL, INTENT(IN) :: H
REAL, EXTERNAL:: Fx
REAL:: DForxh, YatX, YatXplusH, X
YatX=Fx(X)
X=X+H
YatXplusH=Fx(X)
Dforxh=(YatXplusH-YatX)/H
END FUNCTION DForxh

FUNCTION DCenxh(X, H)
IMPLICIT NONE 
REAL, INTENT(IN) :: H
REAL:: DCenxh, YatXminusP, YatXplusP, X, P
REAL, EXTERNAL:: Fx
P=H/2
X=X-P
YatXminusP=Fx(X)
X=X+P
YatXplusP=Fx(X)
DCenxh=(YatXplusP-YatXminusP)/H
END FUNCTION DCenxh

FUNCTION ExactDerivativeAtX(X)
IMPLICIT NONE
REAL, INTENT(IN) :: X
REAL:: ExactDerivativeAtX

ExactDerivativeAtX=((-2)*X-3)/((X**2+3*X+2)**2)

END FUNCTION  ExactDerivativeAtX