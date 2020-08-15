PROGRAM Assignment1
IMPLICIT NONE

INTEGER:: i, j, MaxPos(1)
INTEGER, PARAMETER :: m = 1050, n = 10
REAL :: Prior(0:m), P(0:m), TauArray(0:m), DecayMeasurements (10), MaxProb(1)
REAL :: dtau, NormalisationConstant, TauMax, TauMin, TauofMaxProb(1)

!This program takes a prior array of predicted half life times, in intervals of
!0.1s(dtau). It then reads 10 (n) measurements of the half life from a file, 
!bayes_data.txt. The program uses the 10 decay measurements and Bayes theorem to
!calculate a new probability function for the half lives, writing them to a file, 
!results.dat. It then prints the most likely half life and its 
!probability density to the screen.

TauMax = 100.0; TauMin = 1.0; dtau = 0.1 
!Setting initial values, where dtau is the interval increases in the half life                                        
!Taumin is the minimum predicted half life value, Taumax is the maximum predicted value
!Opening decay measurement times to read measurements from file to array
OPEN(10, FILE = 'bayes_data.txt') 
  DO i = 1,n
    READ(10,*) DecayMeasurements(i)  
  END DO      
CLOSE(10)
!Do loop creates tau array, the possible half life times
!in intervals of dtau (0.1s) from 0 to 105
DO i = 0, m 
  TauArray(i) = REAL(i)*dtau 
END DO
!Do loop sets prior array
DO i = 0, m 
  IF (TauArray(I) >= TauMin .and. TauArray(i) <= TauMax) THEN
    Prior(i) = 1.0/(TauMax-TauMin) 
  ELSE 
    Prior(i) = 0.0 
  END IF
END DO

P = Prior 
!Do loops use prior array and decay measurements to apply Bayes Theorem
DO j = 1, n   
  DO i = 1, m 
    P(i)=P(i)*exp(-DecayMeasurements(j)/TauArray(i))/TauArray(i)   
  END DO
END DO

NormalisationConstant = (SUM(P) - 0.5*(P(0)+P(m)))*dtau 
!Normalising probability array, so area under p(i) plotted against tau array equals 1
DO i = 1, m
  P(i) = P(i)/NormalisationConstant
END DO

OPEN(11, FILE = 'Results.dat') 

WRITE(11,*) '#Tau Prob.Density Prior' !Titles for data file
!Printing prior array, half life times are their respective probability to file
DO i = 1, m
  WRITE(11,'(f5.1, e12.4, f10.6)') TauArray(i), P(i), Prior(i) 
END DO
CLOSE(11)

MaxPos = Maxloc(P); MaxProb = P(MaxPos-1); TauofMaxProb = TauArray(MaxPos-1) 
!Calculates the position of the highest probability
!Then finds the maximum probability and its half life value using the max position. 
!The max position is displaced by 1 due to the maxloc function calculating the no. 
!of elements till the max value, and not its position in the array

WRITE(6,*) 'Results printed to file ''Results.dat'''
WRITE(6,'(a28, f5.1, a1)') 'Most probably half life is:', TauofMaxProb, 's'
WRITE(6,'(a31, f7.4)') 'With a probability of density:', MaxProb 

END PROGRAM 