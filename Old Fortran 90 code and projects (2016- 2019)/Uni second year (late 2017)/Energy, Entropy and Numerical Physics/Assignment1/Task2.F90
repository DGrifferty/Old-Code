program test
implicit none
real :: ran, lambda, Distance, SumDistance, SumDistanceSquared, AverageDistanceSquared, AverageDistance, StandardDeviation
REAL :: AverageDistanceProp, McError
real, external :: rand,ran_exp
integer :: first,iseed, n, nscatt, i, itwo

first=0; iseed=97173; ran=rand(iseed,first); lambda=1E-7; Distance=0; SumDistance=0; SumDistanceSquared=0; n=1000000; nscatt=9

DO i = 1,n
  DO itwo = 1,nScatt
    Distance = Distance+ran_exp(iseed,first,lambda)
  END DO
  SumDistance =  SumDistance + Distance
  SumDistanceSquared = SumDistanceSquared + (Distance**2)  
  Distance =  0
END DO

AverageDistance = SumDistance/Real(n)
AverageDistanceProp = nscatt*lambda
McError = (Real(nscatt)*lambda)/(SQRT(Real(n)*Real(nscatt)))
WRITE(6,*) 'Average MC Distance:', AverageDistance
WRITE(6,*) 'MC Error:', McError
WRITE(6,*) 'Average Distance:', AverageDistanceProp
AverageDistanceSquared = SumDistanceSquared/Real(n)
WRITE(6,*) 'Average Distance Squared;', AverageDistanceSquared
StandardDeviation = SQRT(AverageDistanceSquared + (AverageDistance**2))
WRITE(6,*) 'Standard Deviation:', StandardDeviation

END PROGRAM

function ran_exp(iseed,first,lambda)
  implicit none
  REAL:: ran_exp
  integer :: iseed,first
  real :: dum,lambda
  real, external :: rand
1 continue
  dum=rand(iseed,first)
  if(dum == 0.0d00)goto 1
  ran_exp=-log(dum)*lambda
end function ran_exp

real function rand(iseed,first)

  implicit none
  integer, parameter :: MPLIER=16807
  integer, parameter :: MODLUS=2147483647
  integer, parameter :: MOBYMP=127773
  integer, parameter :: MOMDMP=2836
  integer hvlue,lvlue,testv,nextn,first,iseed
  save nextn

  if(first == 0) THEN
    nextn=iseed
    first=1
  endif

  hvlue=nextn/mobymp
  lvlue=mod(nextn,mobymp)
  testv=mplier*lvlue-momdmp*hvlue
  if(testv > 0)then
    nextn=testv
  else
    nextn=testv+modlus
  endif
  rand = real(nextn)/real(modlus)

end function rand