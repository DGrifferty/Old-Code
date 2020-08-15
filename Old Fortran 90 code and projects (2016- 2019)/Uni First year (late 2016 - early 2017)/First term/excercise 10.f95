PROGRAM machin_pi

  IMPLICIT NONE
  INTEGER :: i
  REAL :: pi, newpi, sign

  i=1

  ! "sign" allows me to add and subtract terms in order since tan^-1
  ! is an alternating series.
  sign=1

  DO
     ! add one new term in the sum for both inverse tangents
     newpi = pi + sign*16*(1/i)/5**i - 4*(1/i)/239**i
     i=i+3
     IF(newpi - pi == 0.0) EXIT
     pi = newpi
     sign = -sign
  END DO

  WRITE(6,*) "p is ",pi, ", which took ",i, "terms of Machin's formula"

END PROGRAM machin_pi