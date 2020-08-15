PROGRAM Assignment1
IMPLICIT NONE

INTEGER:: i, j
INTEGER, PARAMETER :: m = 1050
REAL :: Prior(0:m), P(0:m), TauArray(0:m), DecayMeasurments(10), l(1), dtau, NormalisationConstant, Y(1), X(1)

dtau = 0.1; Prior=1.0/99.0; P = Prior

OPEN(10, FILE = 'bayes_data.txt') !Opening decay measurment times
  READ(10,'(f6.3)') DecayMeasurments !Reading 10 decay measurments to array
CLOSE(10)

DO i = 1, 1050
  TauArray(i) = REAL(i)*dtau !Creating tauarray, the possible half life times, in intervals of 0.1s
END DO

DO j = 1, 10
  DO i = 1, 1050
      
    P(i)=P(i)*exp(-DecayMeasurments(j)/TauArray(i))/TauArray(i)
    
  END DO
END DO

NormalisationConstant = (SUM(P) - 0.5*(P(0)+P(m)))*dtau !Calculating normalisation constant

DO i = 1, 1050
  P(i) = P(i)/NormalisationConstant!Normalising probability array
END DO

OPEN(11, FILE = 'Results.dat') !Creating file to print results in
DO i = 1,1050
  WRITE(11,*) 'p ''Results.dat'' u 1:2 w l' 
  WRITE(11,*) TauArray(i), P(i) !Printing half life times are their respective probabaility to file
END DO
CLOSE(11)

WRITE(6,*) 'Results printed to file ''Results.dat''', SUM(P)

l = Maxloc(P); Y = P(l); X = TauArray(l)
WRITE(6,*) L

WRITE(6,*) 'Most probably half life is', X, 'with a probability of:', Y 

END PROGRAM 