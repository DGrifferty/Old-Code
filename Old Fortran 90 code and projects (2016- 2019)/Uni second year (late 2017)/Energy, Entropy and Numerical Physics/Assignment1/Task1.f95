Program TaskOne
IMPLICIT NONE 

  real :: X, Y, InsideCircCheck, PiEstimate
  integer :: i,iseed, first, n, ncirc
  real, external :: RandNum

  first=0; iseed=971739; nCirc=0
  write(6,'(a,i9)')'seed value that generates this sequence is ',iseed
  WRITE(6,*) 'Please enter number of points, n'
  READ(5,*) n
X = (RandNum(iseed,first)*2.0)-1
Y = (RandNum(iseed,first)*2.0)-1

DO i=1,n
  X = (RandNum(iseed,first)*2.0)-1
  Y = (RandNum(iseed,first)*2.0)-1
  InsideCircCheck = X**2+Y**2

  IF ( InsideCircCheck <= 1) then
    nCirc = nCirc + 1
  END IF

END DO

PiEstimate = 4.0*(REAL(nCirc)/REAL(n))
WRITE(6,*) PiEstimate
 
END PROGRAM

function RandNum(iseed,first) 
  implicit none
  REAL:: RandNum
  integer, parameter :: MPLIER=16807
  integer, parameter :: MODLUS=2147483647
  integer, parameter :: MOBYMP=127773
  integer, parameter :: MOMDMP=2836
  integer :: hvlue,lvlue,testv,nextn,first,iseed
  save nextn
  

  if(first == 0) THEN
    nextn=iseed
    first=1
  end if
  hvlue=nextn/mobymp
  lvlue=mod(nextn,mobymp)
  testv=mplier*lvlue-momdmp*hvlue
  
  if(testv > 0)then
    nextn=testv
  else
    nextn=testv+modlus
  endif
  RandNum = real(nextn)/real(modlus)

end function RandNum