PROGRAM Assignment0
IMPLICIT NONE

REAL, Dimension(:), Allocatable:: X, V,  T
REAL:: h, dvdt, dxdt, XMid, Vmid, dxdtMid, dvdtmid, Tmid
INTEGER:: i, npts

!This program uses the modified euler method to numerically iterate an ODE between r=0 and t=50
!The ODES model a mass m attached to a spring. The program calculates m's x position and velocity
!It does this in 5E-3 steps of time and plots 10001 steps to a file called results.dat

npts=10001

Allocate(T(0:npts), X(0:npts), V(0:npts))!Creating arrays to store the data

X(0)=0; V(0)=2.0; T(0)=0; h=5.0E-3 !Setting intial boundary conditions

OPEN(10, FILE='Results.dat')

WRITE(10,*) 'set xrange [0:50.006]' !Limiting X axis to only required values, as it auto plots to 60s, leaving a lot of white space
WRITE(10,*) 'set xlabel "Time (s)"' !Labeling X axis
WRITE(10,*) 'p ''Results.dat'' u 1:2 w l t "X Position", '''' u 1:3 w l t "Velocity"' !Comand plots time vs x and time vs v when file is loaded in gnuplot                                                        

DO i=1,npts  
  
  dxdt = v(i-1)                           !dxdt is the velocity of V at that point
  dvdt = -5.0*(x(i-1))+0.5*Cos(2*T(i-1))  !dvdt the velocity acceleration at that point
 
  XMid = x(i-1) + 0.5*h*dxdt             !X mid = previous X value + half interval * velocity at the starting point
  VMid = V(i-1) + 0.5*h*dvdt             !V mid = previous v value + half interval * accelaeration at the starting point

  dxdtmid = VMid                         !gradient at X mid = V mid
  X(i) = X(i-1)+h*dxdtMid                !Final X = X in the middle + interval * gradient at mid point
  Tmid = T(i-1)+0.5*h                    !Tmid is previous T + half interval
  dvdtMid = -5.0*XMid+0.5*Cos(2*Tmid)    !Acceleraton at mid point, means the use of X mid and T mid

  V(i) = V(i-1)+h*dvdtmid                !Final V= previous V + interval * acceleration at mid point
  T(i) = T(i-1)+h                        !Final T is T plus interval

  WRITE(6,'(f6.3, f9.5, f9.5)') T(i), X(i), V(i) !Prints results to screen
  WRITE(10,'(f6.3, f9.5, f9.5)') T(i), X(i), V(i) !Prints resuts to file

END DO

CLOSE(10)

WRITE(6,*) 'Results written to file: ''Results.dat''. '
WRITE(6,*) 'Results are recoreded in three colums, in the order of Time, X value then Velocity'

DEALLOCATE(T, X, V)

END PROGRAM 



