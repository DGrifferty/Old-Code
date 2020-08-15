!--------------------------------------------------
!PHY1038 Assignment 1 Newton-Raphson Root Finder
!URN: 6408056
!February 2017
!--------------------------------------------------
PROGRAM ASSIGMENTONENEWTON
IMPLICIT NONE

REAL :: a, b, c, d, atwo, btwo, ctwo, xn, xnplusone, xes, fxder, fx, Tol, dev, comparison, fxtwo
CHARACTER :: fig, figy, figval, figtol, figdev, figtoltwo, figdevtwo, figdevroot, figsort, figdevfour, figdevsort
CHARACTER(LEN=7):: root
INTEGER::i, q, nplusone, n, itwo, ithree
REAL, DIMENSION(:), ALLOCATABLE::t, k
fig = 'y'
figval='n'
figtoltwo='p'
figdevtwo='p' !figures set outside do loop so certain aspects only run first time, and later when required
WRITE(6,*)'This program is designed to find the root of a third order polynomial in the form of, ''aX^3+bX^2+cX+d''.'
WRITE(6,*)'It uses the Newton-Raphson method, and requires the input of a, b, c, d and an intial guess at a root.'
DO WHILE (fig=='y' .or. fig=='Y')!This do while loop is here so that the user can later run the program again, or exit
 figy = 'n'
 q=1
  IF (figval=='n' .or. figval=='N') THEN !do while loop here so that if the user wants to run the program again, 
!they can use the the same values of a, b, c and d if they want to
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
    WRITE(6,*) 'Using',a,'X^3 +',b,'X^2 +',c,'X +',d !This is here to show users who want to use the 
!same value of a, b, c and d on their secound run that the program is doing so
  END IF
 
  WRITE(6,*) 'The derivative of your function is', atwo,'X^2 +', btwo, 'X +', ctwo
  WRITE(6,*) 'How many roots would you like to find?'
  
  DO
    READ(5,*) q !Reading how many roots the user wants to find
!so, a later do loop can allow the main part of the program to run multiple times later, and save the user time
   
    
   
    IF (q <= 0) THEN !If function here is to ensure an appropriate number of roots are selected
      WRITE(6,*) 'The number or roots to be found must be greater than 0, Please try again.'
      CYCLE
    ELSE IF(q > 3) THEN
      WRITE(6,*) 'A third order polynomial can not have more than 3 roots, Please try again.'
      CYCLE
    ELSE IF(q > 0 )THEN
      EXIT
    END IF
   
  END DO
  
  ALLOCATE(t(q))!Allocates an array to be able to store the amount of roots they want to find, 
!allowing the program to store, and display them all together at the end
  ALLOCATE(k(q))!This is to allow th user to later see the value of the roots, 
  !and the function at those points, together
  DO i=1,q !Runs main body of the program multiple times so the user can find 
!all the roots that they want to on one run, without having to enter the values again
    IF (i==1) THEN
      root='first' !setting character to allow user to later know what root is being found
      ELSE IF (i==2) THEN
        root='secound'
      ELSE IF (i==3) THEN
         root='third'
      END IF
      
    IF (figtoltwo == 'p' .or. figtoltwo == 'p') THEN !If stops the program writing this message on every run
      WRITE(6,*) 'The tolerance for the program is by default set to zero, the tolerance determines how close two iterations'
      WRITE(6,*) 'must be before the program determines the latter to be a root of the polynomial' 
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
    ELSE IF (figtoltwo == 'n' .or. figtoltwo == 'n') THEN !This is implemented, so that if the user choses
 !they are not asked to input their tolerance again, after going through one run
      figtol = 'Y'
    ELSE IF(figtoltwo=='y'.or.figtoltwo=='Y') THEN
      WRITE(6,*)'Using the same tolerance' !allows user to know that the program is using the tolerance that they want to 
      figtol = 'n' !this can be useful for certain functions
    ELSE
      WRITE(6,*) 'Using set tolerance'
      figtol='p'
    END IF
    
    IF (figtol == 'y' .or. figtol=='Y') THEN
      WRITE(6,*)'Please enter tolerance.' !allows user to change tolerance, and makes sure value is positive
      READ(5,*) Tol
      Tol=abs(Tol)
      figtoltwo='y'
    ELSE IF (figtol =='n' .or. figtol== 'N') THEN
      WRITE(6,*) 'Tolerance set to zero.'
      Tol=0
    END IF
    
      IF (figdevtwo == 'p' .or. figdevtwo == 'p') THEN
        WRITE(6,*) 'The program, by default, assumes that if two iterations are'
        WRITE(6,*) 'over 500 away then they are deviating, and cancels the search for the root.'
        
        DO
          WRITE(6,*) 'If you would like to change this, type Y, if not type N' !Allows user to set the program 
!to be more or less sensitive to large changes in iterations, which may be useful for some functions
          READ(5,*) figdev
          IF (figdev /= 'n' .and. figdev /='N' .and. figdev /='Y' .and. figdev /='y') THEN
            WRITE (6,*) 'Invalid option.'
            CYCLE
          ELSE 
            EXIT
          END IF
       END DO
       
      Else if (figdevtwo == 'n' .or. figdevtwo =='N') Then
        figdev = 'Y' !this means that if the user chooses to use a different deviation tolerance,
        !another figure is set, to allow them to do so
      ELSE IF(figdevtwo == 'y'.or.figdevtwo == 'Y') THEN
        WRITE(6,*)'Using the same deviation tolerance'
      ELSE
        WRITE(6,*) 'Using set deviation tolerance'
        figdev='p'!doing this overrides previous figures, allowing the user to skip 
      END IF !setting another deviation tolerance
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
      xn=xes !sepearates estimate from number used in iteration so programme can later compare estimate
      !The users intial estimate, to the final root
      
      DO itwo=1, 100
        nplusone=itwo+1 !N and Npluone are used so user can see what iteration has what value
        n=itwo
        fx=a*xn**3+b*xn**2+c*xN+d
        fxder= atwo*xn**2+btwo*xn+ctwo
        xnplusone= xn-(fx/fxder)
        WRITE (6,*) 'x',n,'=',xn,'x',nplusone,'=', xnplusone !shows user entered vale of x, xn, 
!and value after going through formula, xn+1
        comparison=abs(xn-xnplusone) !comparison is the value that the user entered tolerance is compared against
        
        IF(comparison<=tol) THEN
          WRITE(6,*) 'Root found.'
          WRITE(6,*) 'Your ',root,'root of the function is ',xnplusone, 'Your orginal estimate was', xes
          fxtwo = a*(xnplusone**3)+b*(xnplusone**2)+c*(xnplusone)+d
          WRITE(6,*)'Value of you fuction at this point is',fxtwo
          t(i) = xnplusone 
          k(i) = fxtwo!saving numbers to an allocated array, to display them all at the end
          EXIT
        ELSE IF(comparison>=dev) THEN
          figsort = 'p'
          DO WHILE (figsort == 'p')
            WRITE(6,*) 'Iterations deviating within deviation tolerance.'
            WRITE(6,*)'Would you like to try again with a different estimate?'
            
              DO
                WRITE(6,*) 'If you would like to use a different esitimate, type Y, if not type N'
                READ(5,*) figdevroot
                IF (figdevroot /= 'n' .and. figdevroot /='N' .and. figdevroot /='Y' .and. figdevroot /='y') THEN 
!do loop and if function to ensure a valid response is recieved
                  WRITE (6,*) 'Invalid option.'
                  CYCLE
                ELSE
                   EXIT
                END IF
              END DO
             
              IF(figdevroot == 'y' .or. figdevroot == 'Y') then
                WRITE(6,*) 'Please type in your new estimate'
                READ(5,*) xes
                WRITE(6,*)'Would you like to change the deviation tolerance?'!allows user to change  
!deviation tolerance again, if previous value was too sensitive for their function
                DO
                  WRITE(6,*) 'Type Y if you would like change it, if not type N'
                  READ(5,*) figdevfour
                  IF (figdevfour /= 'n' .and. figdevfour /='N' .and. figdevfour /='Y' .and. figdevfour /='y') THEN
                    WRITE (6,*) 'Invalid option.'
                    CYCLE
                  ELSE IF (figdevfour == 'y' .or. figdevfour =='Y') THEN
                    WRITE(6,*) 'Please enter new deviation tolerance.'
                    READ(5,*) Dev
                    dev=abs(dev)
                    EXIT
                  ELSE
                    EXIT
                  END IF
                   
                  t(i) = xnplusone 
                END DO
              xn=xes
              DO ithree=1, 100 !runs main part of program if there root is deviating, would be better in a function
                !but it became difficult to implement the allocatable arrays within it etc.            
                nplusone=ithree+1 
                n=ithree
                fx=a*xn**3+b*xn**2+c*xn+d
                fxder= atwo*xn**2+btwo*xn+ctwo
                xnplusone= xn-(fx/fxder)
                WRITE (6,*) 'x',n,'=',xn,'x',nplusone,'=', xnplusone
                comparison=abs(xn-xnplusone)
                IF(comparison<=tol) THEN
                  WRITE(6,*) 'Root found.'
                  WRITE(6,*) 'Your ',root,'root of the function is ',xnplusone, 'Your orginal estimate was', xes
                  fxtwo = a*(xnplusone**3)+b*(xnplusone**2)+c*(xnplusone)+d
                  WRITE(6,*)'Value of you fuction at this point is',fxtwo
                  t(i) = xnplusone
                  k(i) = fxtwo
                  figsort = 'n'
                  EXIT
                  EXIT
                ELSE IF(comparison>=dev) THEN
                  WRITE(6,*)'Root deviating, would you like to try again?'
                  EXIT
                  
                  DO
                    WRITE(6,*)'Type Y for yes, or N for no.'
                    READ(5,*) figdevsort
                    IF (figdevsort /= 'n' .and. figdevsort /='N' .and. figdevsort /='Y' .and. figdevsort /='y') THEN
                      WRITE (6,*) 'Invalid option.'
                      CYCLE
                    ELSE
                       EXIT
                    END IF
                  END DO
                  
                  IF (figdevsort=='n' .or. figdevsort=='N') THEN
                    figsort='e'
                  END IF
                END IF
               
                xn=xnplusone
                t(i) = xnplusone
              END DO
            ELSE IF(figdevroot == 'n' .or. figdevroot == 'N') THEN
            EXIT
          END IF          
        END DO
       comparison=0
      END IF
    xn=xnplusone
    figdevtwo='q'
    figtoltwo='q'
    END DO
  END DO 
  WRITE(6,*)'The',q,' roots you found are',t !displaying all roots in the allocatable array
  WRITE(6,*)'The value of the function at these roots are',k 
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
    WRITE(6,*) 'Would you like to use the same values of a, b, c and d again?'
    DO !Allowing user to use the set values again if they wish, 
       !to make running the program multiple times more user friendly
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
  DEALLOCATE(K)
  END DO
WRITE(6,*)'Thank you for using the program.'
ENDPROGRAM ASSIGMENTONENEWTON
                           
                                                                          
                                   
                                                              

                                                              

                                                  
                                                  

                               
                           
                        
                        
          
    

        
   
     
      
      
      

  
  
