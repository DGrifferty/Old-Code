Program Task1
IMPLICIT NONE
! p 'Results.dat' u 1:2 w l, '' u 1:3 w l

REAL, Dimension(:), Allocatable:: Time, Mass, MassExact, MassError
REAL:: h, MassDash, LargestError
INTEGER:: I, npts

npts=300

Allocate(Time(0:npts), Mass(0:npts), MassExact(0:npts), MassError(0:npts))

Mass(0)=0.1; Time(0)=0.0; h=0.1 !Setting intial boundary conditions

OPEN(10, FILE='Results.dat')

DO i=1,npts

  Time(i)=Time(i-1)+h
  MassDash=-0.3*Mass(i-1)
  Mass(i)=Mass(i-1)+h*MassDash
  
  MassExact(i)=0.1*exp(-0.3*Time(i))

  MassError(i) = ABS(MassExact(i)-Mass(i))

  WRITE(10,'(f9.5, f9.5, f9.5, f9.5)') Time(i), Mass(i), MassExact(i), MassError(i)
  
END DO

LargestError = MAXVAL(MassError)

WRITE(6,*) 'Largest absolute difference between exact and apporximated mass is:',LargestError 

CLOSE(10)

DEALLOCATE(Time, Mass, MassExact)

END PROGRAM 






