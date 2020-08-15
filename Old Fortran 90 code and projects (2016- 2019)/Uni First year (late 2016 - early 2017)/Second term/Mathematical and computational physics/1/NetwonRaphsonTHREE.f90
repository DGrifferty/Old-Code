!--------------------------------------------------
!PHY1038 Assignment 1 Newton-Raphson Root Finder
!URN: 6408056
!February 2017
!--------------------------------------------------
PROGRAM ASSIGMENTONENEWTON
IMPLICIT NONE

REAL :: a, b, c, d, atwo, btwo, ctwo, xn, xnplusone, xes, fxder, fx, Tol, dev, comparison
CHARACTER :: fig, figy, figdiv, figval, figtol, figdev, figtoltwo, figdevtwo, figdevroot, figsort, figdevfour, figdevsort
CHARACTER(LEN=7):: root
INTEGER::i, q, nplusone, p, n, itwo, ithree
REAL, DIMENSION(:), ALLOCATABLE::t
!implement special character in do while to make sure something only runs the first time!

!user inputed significant figure

!SET IT SO F(X)<=TOLERANCE, SORT OUT A DEVIATING ROOT, SORT OUT SO TOLERANCE DOESNT HAVE TO BE ENTERED EVERY TIME, SET SO THEY CAN USE THE SAME TOLERANCES IF GOING AGAIN
fig = 'y'
figval='n'
figtoltwo='p'
figdevtwo='p'
WRITE(6,*)'This program is designed to find the root of a third order polynomial in the form of, ''aX^3+bX^2+cX+d''.'
WRITE(6,*)'It uses the Newton-Raphson method, and requires the input of a, b, c, d and an intial guess at a root.'
DO WHILE (fig=='y' .or. fig=='Y')
figy = 'n'
q=1
IF(figval=='n' .or. figval=='N') THEN
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
  ELSE IF (figval=='y' .or. figval=='Y') THEN
    WRITE(6,*) 'Using',a,'X^3 +',b,'X^2 +',c,'X +',d
    END IF

  WRITE(6,*) 'The derivative of your function is', atwo,'X^2 +', btwo, 'X +', ctwo
  WRITE(6,*) 'How many roots would you like to find?'
  DO
    READ(5,*) q
     IF (q <= 0) THEN
     WRITE(6,*) 'The number or roots to be found must be greater than 0, Please try again.'
     CYCLE
     ELSE IF(q > 3) THEN
     WRITE(6,*) 'A third order polynomial can not have more than 3 roots, Please try again.'
     CYCLE
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



     
     			IF (figtoltwo == 'p' .or. figtoltwo == 'p') THEN !Using p so it tells the user what it is the first time, but on latter runs, the user can change, or not change the values quicker
WRITE(6,*) 'The tolerance for the program is by default set to zero, the tolerance determines how close two iterations'
WRITE(6,*) 'must be before the program determines the latter to be a root of the polynomial'
!if implemented so on secound runs of program, after saying they want to change the tolerance, they are not asked again
DO
WRITE(6,*) 'If you would like to change this, type Y, if not type N'
READ(5,*) figtol
IF (figtol /= 'n' .and. figtol /='N' .and. figtol /='Y' .and. figtol/='y') then
  WRITE (6,*) 'Invalid option.'
  CYCLE
  ELSE 
    EXIT
    END IF
    END DO
   				 ELSE IF (figtoltwo == 'n' .or. figtoltwo == 'n') THEN !implemented to ensure that if they say they wanna change the tolerance, they are not asked again
     			 figtol = 'Y'
     ELSE IF(figtoltwo=='y'.or.figtoltwo=='Y') THEN
  WRITE(6,*)'Using the same tolerance'
  figtol = 'n'
  ELSE 
    WRITE(6,*) 'Using set tolerance'
    figtol='p'
  END IF
      
IF (figtol == 'y' .or. figtol=='Y') THEN
  WRITE(6,*)'Please enter tolerance.' !allows user to change tolerance, and esure value is positive
  READ(5,*) Tol
  Tol=abs(Tol)
  ELSE IF (figtol =='n' .or. figtol== 'N') THEN
    WRITE(6,*) 'Tolerance set to zero.'
    Tol=0
    END IF 


  
IF (figdevtwo == 'p' .or. figdevtwo == 'p') THEN
WRITE(6,*) 'The program, by default, assumes that if two iterations are over 500 away then they are deviating,'
WRITE(6,*) 'and cancels the search for the root.' 
DO
WRITE(6,*) 'If you would like to change this, type Y, if not type N' !Allows user to set the program to be more or less sensitive to large changes in iterations, which may be useful for some functions
READ(5,*) figdev
IF (figdev /= 'n' .and. figdev /='N' .and. figdev /='Y' .and. figdev /='y') THEN
  WRITE (6,*) 'Invalid option.'
  CYCLE
  ELSE 
    EXIT
    END IF
    END DO
    Else if (figdevtwo == 'n' .or. figdevtwo =='N') Then
      figdev = 'Y'
      ELSE IF(figdevtwo == 'y'.or.figdevtwo == 'Y') THEN
  WRITE(6,*)'Using the same deviation tolerance'
 ELSE 
    WRITE(6,*) 'Using set deviation tolerance'
    figdev='p'
      END IF
IF (figdev == 'y' .or. figdev=='Y')THEN
  WRITE(6,*)'Please enter deviation tolerance.'
  READ(5,*) dev
  dev=abs(dev)
  ELSE IF (figdev =='n' .or. figdev== 'N')THEN
    WRITE(6,*) 'Deviation tolerance set to fivehundred.'
    dev=500
    
    END IF 





  WRITE(6,*) 'Please enter your intial estimate of the ',root,' root you would like to find.'
  READ(5,*) xes
  xn=xes !sepearates estimate from number so programme can later compare estimate, to final root found
  n=1
  nplusone=2
DO itwo=1, 100
nplusone=itwo+1
fx=a*xn**3+b*xn**2+c*xN+d
fxder= atwo*xn**2+btwo*xn+ctwo

xnplusone= xn-(fx/fxder)

 WRITE (6,*) 'x',n,'=',xn,'x',nplusone,'=', xnplusone !shows user entered vale of x, xn, and value after going through formula, xn+1
n= n+1
nplusone=nplusone+1 !adding values to n, so correct numbers display in next loop
comparison=abs(xn-xnplusone)
 IF(comparison<=tol) THEN !userinputedtolerance
  
  WRITE(6,*) 'Root found.'
  WRITE(6,*) 'Your ',root,'root of the function is ',xnplusone, 'Your orginal estimate was', xes
  t(q) = xnplusone
  EXIT















  
!DEVIATION
ELSE IF(comparison>=dev) THEN
  figsort = 'p'
DO WHILE (figsort == 'p')
  WRITE(6,*) 'Iterations deviating within deviation tolerance, would you like to try again with a different estimate?'
DO 
WRITE(6,*) 'If you would like to use a different esitimate, type Y, if not type N'
READ(5,*) figdevroot
 IF (figdevroot /= 'n' .and. figdevroot /='N' .and. figdevroot /='Y' .and. figdevroot /='y') THEN !do loop and if function to ensure a valid response is recieved
  WRITE (6,*) 'Invalid option.'
  CYCLE
  ELSE 
    EXIT
    END IF
    END DO
  
IF(figdevroot == 'y' .or. figdevroot == 'Y') then
  WRITE(6,*) 'Please type in your new estimate'
     READ(5,*) xes
     WRITE(6,*)'Would you like to change the deviation tolerance?'
     DO
     WRITE(6,*) 'Type Y if you would like change it, if not type N' !Allows user to set the program to be more or less sensitive to large changes in iterations, which may be useful for some functions
READ(5,*) figdevfour
IF (figdevfour /= 'n' .and. figdevfour /='N' .and. figdevfour /='Y' .and. figdevfour /='y') THEN
  WRITE (6,*) 'Invalid option.'
  CYCLE
  
     
Else if (figdevfour == 'y' .or. figdevfour =='Y') Then
      WRITE(6,*) 'Please enter new deviation tolerance.'
      READ(5,*) Dev
      dev=abs(dev)
      exit
      ELSE 
    EXIT
      end if

     END DO





     
     
  xn=xes 
  n=1
  nplusone=2
DO ithree=1, 100
nplusone=itwo+1
fx=a*xn**3+b*xn**2+c*xN+d
fxder= atwo*xn**2+btwo*xn+ctwo

xnplusone= xn-(fx/fxder)

 WRITE (6,*) 'x',n,'=',xn,'x',nplusone,'=', xnplusone 
n= n+1

comparison=abs(xn-xnplusone)
 IF(comparison<=tol) THEN 
  
  WRITE(6,*) 'Root found.'
  WRITE(6,*) 'Your ',root,'root of the function is ',xnplusone, 'Your orginal estimate was', xes
  t(q) = xnplusone
  figsort = 'n'
  EXIT
  EXIT
  ELSE IF(comparison>=dev) THEN
    exit
    Write(6,*)'Root deviating, would you like to try again?' 
   do
 Write(6,*)'Type Y for yes, or N for no.'
   READ(5,*) figdevsort
 IF (figdevsort /= 'n' .and. figdevsort /='N' .and. figdevsort /='Y' .and. figdevsort /='y') THEN !do loop and if function to ensure a valid response is recieved
  WRITE (6,*) 'Invalid option.'
  CYCLE
  ELSE 
    EXIT
    END IF
    END DO
  IF (figdevsort=='n' .or. figdevsort=='N') then
    figsort='e'
    end if
    end if
     xn=xnplusone
     nplusone=nplusone+1 
     t(q) = xnplusone
    END DO
ELSE IF(figdevroot == 'n' .or. figdevroot == 'N') THEN
      EXIT
      END IF
      
      END DO
     
     
 comparison=0       

  
   END IF
























  
xn=xnplusone !so next iteration runs using xn+1
figdevtwo='q'
figtoltwo='q'
END DO

END DO
WRITE(6,*)'The ',q,' roots your found are',t !displaying all roots in allocatable array
WRITE(6,*) 'Would you like to run the program again?'
DO 
WRITE(6,*) 'If you would like to run the program again, type Y, if not type N'
READ(5,*) fig
IF (fig /= 'n' .and. fig /='N' .and. fig /='Y' .and. fig /='y') THEN
  WRITE (6,*) 'Invalid option.'
  CYCLE
  ELSE 
    EXIT
    END IF
    END DO
IF (fig=='y' .or. fig=='Y') THEN
    WRITE(6,*) 'Would you like to use the same values of a, b, c and d again?' !to allow ease of use if the user wants to use the program to find more roots at different tollerances etc.
    DO 
WRITE(6,*) 'If you would like to use the same values again, type Y, if not type N'
READ(5,*) figval
 IF (figval /= 'n' .and. figval /='N' .and. figval /='Y' .and. figval /='y') THEN
  WRITE (6,*) 'Invalid option.'
  CYCLE
  ELSE 
    EXIT
    END IF
    END DO
IF (fig=='y' .or. fig=='Y') THEN
      WRITE(6,*) 'Would you like to use the same tolerance again?'
      DO 
WRITE(6,*) 'If you would like to use the same value again, type Y, if not type N'
READ(5,*) figtoltwo
 IF (figtoltwo /= 'n' .and. figtoltwo /='N' .and. figtoltwo /='Y' .and. figtoltwo /='y') THEN
  WRITE (6,*) 'Invalid option.'
  CYCLE
  ELSE 
    EXIT
    END IF
    END DO
WRITE(6,*) 'Would you like to use the same deviation tolerance again?'
      DO 
WRITE(6,*) 'If you would like to use the same value again, type Y, if not type N'
READ(5,*) figdevtwo
 IF (figdevtwo /= 'n' .and. figdevtwo /='N' .and. figdevtwo /='Y' .and. figdevtwo /='y') THEN
  WRITE (6,*) 'Invalid option.'
  CYCLE
  ELSE 
    EXIT
    END IF
    END DO

    END IF
 END IF
    DEALLOCATE(T)
    END DO
   
      
      

WRITE(6,*)'Thank you for using the program.'
ENDPROGRAM ASSIGMENTONENEWTON





