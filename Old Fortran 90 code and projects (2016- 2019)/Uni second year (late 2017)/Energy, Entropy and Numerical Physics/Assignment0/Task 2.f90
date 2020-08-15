PROGRAM Assignment0
IMPLICIT NONE

REAL, Dimension(:), Allocatable:: X, XError, V,  T 
REAL:: h, XDash, dvdt, dxdt, VDash, XDashMid, XMid, VDashmid, Vmid
INTEGER:: i, npts

npts=10001

Allocate(T(0:npts), X(0:npts), XError(0:npts), V(0:npts))

X(0)=0; V(0)=2.0; T(0)=0; h=5E-3 !Setting intial boundary conditions

OPEN(10, FILE='Results.dat')

DO i=1,npts  
  
  dxdt=v(i-1)
  dvdt=-5*(x(i-1))+0.5*Cos(2*T(i-1))
  
  Xdash = dxdt*x(i-1)
  XMid = x(i-1)+(0.5*h*XDash)
  XDashmid = dxdt*XMid
  X(i) = X(i-1)+h*XDashmid 

  VDash = dvdt*V(i-1)
  VMid=V(i-1)+(0.5*h*VDash)
  VDashMid=dvdt*Vmid
  V(i)=V(i-1)+h*Vdashmid

  t(i)=T(i-1)+h


  WRITE(6,*) T(i), X(i), V(i)
  WRITE(10,*) T(i), X(i), V(i)

END DO

CLOSE(10)

DEALLOCATE(T, X, XError, V)

END PROGRAM 



