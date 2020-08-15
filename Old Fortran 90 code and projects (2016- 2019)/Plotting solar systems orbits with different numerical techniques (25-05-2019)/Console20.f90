PROGRAM CelestialMech
IMPLICIT NONE

REAL:: Me, Mm, Ms, time
REAL:: G, dt
INTEGER:: i, k, t
Double Precision, DIMENSION(1:3):: Xpos, Ypos, Xacc, Yacc, Xvel, Yvel, MassPlan

Me=5.972E24; Mm=7.34767E20; Ms=1.989E30; time =  1000; dt = 1; G=6.67408E-11
Xpos= (/100000, 11000, 10 /); Xvel=(/10, 10, 0/); Xacc=0!Xpos=Xearth, Xmoon, Xsun 
MassPlan = (/ Me, Mm, Ms /) !Setting masses

OPEN(10, file='ResultsAstro.dat')

DO t = 0, int(time/dt)
WRITE(10,*) t, Xpos(1)
  DO k=1,3 !for all planets 
     Xacc(k)=0
WRITE(6,*) 1 
    DO i=1,3     !calculating acceleration
WRITE(6,*) 2
      IF (i /= k) Then !Avoids calculating acceleation of a planet on itself
WRITE(6,*) 3
      Xacc(k)=((G*MassPlan(i)/(Xpos(k)-Xpos(i))**2))*((Xpos(k)-Xpos(i))/abs(Xpos(k)-Xpos(i)))+Xacc(k)
WRITE(6,*) 4       
      END IF      
    END DO
   Xvel(K) = Xacc(k)*dt + Xvel(k)
WRITE(6,*) 5
   Xpos(k) = Xvel(k)*dt + Xpos(k)!Calculating new velocity and acceleration for plannet k
WRITE(6,*) 6
  END DO
WRITE(6,*) 7
   Xacc=0
END DO
CLOSE (10)
END PROGRAM 
!have an array with x pos y pos z pos and then velocities, and then accelerations+ mass

