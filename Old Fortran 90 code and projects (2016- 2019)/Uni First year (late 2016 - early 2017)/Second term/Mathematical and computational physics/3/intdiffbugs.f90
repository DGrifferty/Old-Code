Program intdiffbugs

  INTEGER :: i
  REAL(KIND=2):: A, B, N
  REAL(KIND=2) :: ISimpson, ISimpsonOdd, ISimpsonEven
  REAL(KIND = 2), EXTERNAL :: Fx
n=52

  

  DO i=1, N-1, 2
  
WRITE(6,*) i
 end do

End Program intdiffbugs