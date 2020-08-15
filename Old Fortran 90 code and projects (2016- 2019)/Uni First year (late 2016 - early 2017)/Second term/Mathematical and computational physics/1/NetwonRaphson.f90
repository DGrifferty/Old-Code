!--------------------------------------------------
!PHY1038 Assignment 1 Newton-Raphson Root Finder
!URN: 6408056
!February 2017
!--------------------------------------------------
PROGRAM ASSIGMENTONENEWTON
IMPLICIT NONE

REAL :: a, b, c, d, atwo, btwo, ctwo, xn, xnplusone, xes, fxder, fx, Tol, itwo
CHARACTER :: fig, figy, figd
CHARACTER(LEN=7):: root
INTEGER::i, q, nplusone
REAL, DIMENSION(:), ALLOCATABLE::t


INTERFACE 
SUBROUTINE Deviatingroot
REAL :: a, b, c, d, atwo, btwo, ctwo, dtwo, xn, xnplusone, xes, fxder, fx
CHARACTER :: figd
INTEGER::i, q, nplusone
END SUBROUTINE Deviatingroot
END INTERFACE
!user inputed significant figure
fig = 'y'
WRITE(6,*)'This program is designed to find the root of a third order polynomial in the form of, ''aX^3+bX^2+cX+d''.'
WRITE(6,*)'It uses the Newton-Raphson method, and requires the input of a, b, c, d and an intial guess at a root.'
DO WHILE (fig=='y' .or. fig=='Y')
figy = 'n'
  DO WHILE(figy=='n' .or.figy=='N')
   WRITE(6,*)'Please enter your values of a, b, c and d.'
   READ(5,*) a, b, c, d
   WRITE(6,*)'You entered',a,'X^3 +',b,'X^2 +',c,'X +',d
   WRITE(6,*)'Is this correct? Type N for no or Y for yes.'
   READ(5,*)figy
   END DO
  atwo=3*a
  btwo=2*b
  ctwo=c

  WRITE(6,*) 'The derivative of your function is', atwo,'X^2 +', btwo, 'X +', ctwo
  WRITE(6,*) 'How many roots would you like to find?'
  DO
    READ(5,*) q
     IF (q <= 0) THEN
     WRITE(6,*) 'The number or roots to be found must be greater than 0, Please try again.'
     CYCLE
     ELSE IF(q > 3) THEN
     WRITE(6,*) 'A third order polynomial can not have more than 3 roots, Please try again.'
     ELSE IF(q > 0 )THEN
        EXIT
     END IF 
   END DO
ALLOCATE(t(q))
  DO i=1,q
    IF (i==1) THEN
     root='first'
     ELSE IF (i==2) THEN
     root='secound'
     ELSE IF (i==3) THEN
     root='third'
     END IF
WRITE(6,*) 'Please enter the tolerance you would like to use,  should be before one is said to be the root'
READ(5,*) Tol
  WRITE(6,*) 'Please enter your intial estimate of the',root,'root you would like to find.'
  READ(5,*) xes
  xn=xes
DO 
nplusone=i+1
fx=a*xn**3+b*xn**2+c*xN+d
fxder= atwo*xn**2+btwo*xn+ctwo

xnplusone= xn-(fx/fxder)

 WRITE (6,*) 'x',i,'=',xn,'x', nplusone,'=', xnplusone

 IF(xn==xnplusone) THEN !userinputedtolerance
  WRITE(6,*) 'Root found.'
  WRITE(6,*) 'Your',root,'root of the function is',xnplusone, 'Your orginal estimate was', xes
  EXIT

  ELSE IF (xnplusone-xes >= 500 .or. xes-xnplusone >=500) THEN
    EXIT
  WRITE(6,*) 'Iteration deviating, root not found, would you like to try again with another intial estimate?'
  WRITE(6,*) 'Type Y for yes or N for no.'
  READ(5,*) figd
   IF (figd=='Y' .or. figd=='y') THEN
    CALL Deviatingroot
   END IF
  END IF
xn=xnplusone
END DO

END DO
WRITE(6,*)'The',q,'roots your found are',t
WRITE(6,*) 'Would you like to run the program again?'
DO
WRITE(6,*) 'Type Y for yes or N for no.'
READ(5,*) fig
IF (fig/='Y' .and. fig/='y' .and. fig/='n' .and. fig/='N') THEN
WRITE(6,*) 'Option not found.'
CYCLE
ELSE EXIT
END IF
END DO
END DO
WRITE(6,*)'Thank you for using the program.'
ENDPROGRAM ASSIGMENTONENEWTON



SUBROUTINE Deviatingroot
REAL :: a, b, c, d, atwo, btwo, ctwo, dtwo, xn, xnplusone, xes, fxder, fx
CHARACTER :: figd
INTEGER::i, q, nplusone


DO WHILE (figd=='y' .or. figd=='Y')
WRITE(6,*) 'Please enter your new estimate for the root.'
READ(5,*) xes
xn=xes
DO i=1, 100
nplusone=i+1

fx=a*xn**3+b*xn**2+c*x+d
fxder= atwo*xn**2+btwo*x+ctwo

xnplusone= xn-(fx/fxder)

 WRITE (6,*) 'x',i,'=',xn,'x', nplusone,'=', xnplusone

 IF(xn==xnplusone) THEN !userinputedtolerance
  WRITE(6,*) 'Root found.'
  WRITE(6,*) 'Your',root,'root of the function is',xnplusone, 'Your orginal estimate was', xes
  
  figd='n'
  ELSE IF (xnplusone-xes >= 500 .or. xes-xnplusone >=500) THEN
  WRITE(6,*) 'Iteration deviating, root not found, would you like to try again with another intial estimate?'
  WRITE(6,*) 'Type Y for yes or N for no.'
  READ(5,*) figd
END IF

xn=xnplusone

END DO
END DO

END SUBROUTINE Deviatingroot
