PROGRAM TaskThree
IMPLICIT NONE

REAL, Dimension(:), Allocatable:: Time, X, XExact, XError, V,  T 
REAL:: h, XDash, LargestError, dvdt, dxdt, VDash, Tdash, XDashMid, XMid, VDash, VDashmid, Vmid
INTEGER:: i, npts

npts=1000

Allocate(T(0:npts), X(0:npts), XExact(0:npts), XError(0:npts), V(0:npts))

X(0)=0.1; V(0)=0.0; T(0)=0; h=0.01 !Setting intial boundary conditions

OPEN(10, FILE='Results.dat')

DO i=1,npts  

  dxdt=v(i-1)
  dvdt=-5*(x(i-1))
  
  Xdash = dxdt*x(i-1)
  XMid = x(i-1)+(0.5*h*XDash)
  XDashmid = dxdt*XMid
  X(i) = X(i-1)+h*XDashmid 

  VDash = dvdt*V(i-1)
  VMid=V(i-1)+(0.5*h*VDash)
  VDashMid=dvdt*Vmid
  V(i)=V(i-1)+h*Vdashmid

  t(i)=T(i-1)+h

  XExact=0.1*Cos((5**0.5)*T(i))
  

  WRITE(6,*) T(i), X(i), XExact(i)
  WRITE(10,*) T(i), X(i), XExact(i)

END DO



WRITE(6,*) 'Largest absolute difference between exact and apporximated mass is:'

CLOSE(10)

DEALLOCATE(T, X, XExact, XError, V)

END PROGRAM 
