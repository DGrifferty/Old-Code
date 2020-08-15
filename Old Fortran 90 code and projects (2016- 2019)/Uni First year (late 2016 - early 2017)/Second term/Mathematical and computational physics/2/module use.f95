PROGRAM MODULE
USE MOD
IMPLICIT NONE
REAL:: MOD

Write(6,*) F(x)

end program

module mod

IMPLICIT NONE

REAL:: X

FUNCTION f(x)
REAL, INTENT (IN) :: X
 
f=x**2

end function

end module mod