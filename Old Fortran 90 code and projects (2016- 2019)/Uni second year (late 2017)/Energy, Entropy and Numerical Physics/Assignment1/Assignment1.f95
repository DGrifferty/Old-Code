PROGRAM Assignment1
IMPLICIT NONE

INTEGER:: i, j
INTEGER, PARAMETER :: m = 1050
REAL ::Prior(0:m), P(0:m), TauArray(0:m), DecayMeasurments(10), dtau, NormalisationConstant

dtau = 0.1; p=1.0/1050

OPEN(10, FILE = 'bayes_data.txt')
  READ(10,'(f6.3)') DecayMeasurments
CLOSE(10)
DO i = 1,10
  WRITE(6,'(f6.3)') DecayMeasurments(i)
END DO
DO i = 1,1050
  TauArray(i) = REAL(i)*dtau
END DO

DO j = 1,10
  DO i = 1,1050

    P(i)=P(i)*exp(-DecayMeasurments(j)/TauArray(i))/TauArray(i)

  END DO
END DO


NormalisationConstant = (Sum(P) - 0.5*(P(0)+P(1050)))*dtau

WRITE(6,*) NormalisationConstant

END PROGRAM 