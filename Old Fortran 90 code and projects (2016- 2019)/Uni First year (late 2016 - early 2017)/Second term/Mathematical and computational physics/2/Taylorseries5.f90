!--------------------------------------------------
!PHY1038 Assignment 2 Series Computation
!URN: 6408056
!February 2017
!--------------------------------------------------
PROGRAM Taylorseriesexpansion
IMPLICIT NONE

CHARACTER::figrunone, fig, figninput, fignoption, intchoice, figintervaltwo, figinter, figninputtwo
INTEGER:: n, i, p
DOUBLE PRECISION :: factorial
REAL:: interval, taylorvalueatx, valueofderivative, yexact, x, pi
REAL, DIMENSION(:), ALLOCATABLE::a
figrunone='e' !setting this so introduction to program is only printed on the first run
fig='y'!set to y so do loop allowing users to run the program again runs first time
figninput='n'
fignoption='n' 
intchoice='N' !this allows (on the intial run) for the user to be given a choice as to what the interval increases in x will be
!this is because on further runs of the main do while loop, they are given a choice as to wether they want to use the same value
!so the interval choice part of the program needs to be in an if loop to stop it playing after the user has chosen to use the 
!same interval value
pi=4*atan(1.0)!setting pi for later use
DO WHILE(fig=='y' .or. fig=='Y')!This do while loop allows the user to run the program again at the end, or exit it
 figninputtwo='l' !so on reruns, the user is not told the introduction to the program again
 figintervaltwo='n'
 figninput='n'
 IF (figrunone=='e') THEN !only plays intro on first do loop
   WRITE(6,*) 'This program calculates the Taylor Expansion around zero for f(x)=sin(x)*exp(-x/2), it then'
   WRITE(6,*) 'calulates the values of both the exact function and the Taylor expansion between x=-2pi, and'
   WRITE(6,*) 'x=2pi, and writes the values to a file called taylor.dat. The Taylor function is plotted in'
   WRITE(6,*) 'columns one and two, and the exact function is plotted in columns one and three.'
   WRITE(6,*) 'Thus using: plot "taylor.dat" u 1:2 w l, "taylor.dat" u 1:3 w l'
   WRITE(6,*) 'in gnuplot will allow you to compare the Taylor expansion, of your chosen order, and' 
   WRITE(6,*) 'the exact funtion.'
 END IF

 IF(fignoption=='n' .or. fignoption=='N') THEN!Later users are given a choice if they want to use the same order of the
   !polynomial, this if loop allows for this, if N is selected, saying they don't want to use the same order
   !they are given the following option, thus fignoption is set to N at the beginning, outside the do loop
   !so this plays on the first run
   WRITE(6,*)'Please input the order you would like the program to calculate the polynomial for?'
   DO WHILE (figninput=='n' .or. figninput=='N' )  !if they say the order they entered isn't correct, this allows them to
     !enter it again  
     DO
       READ(5,*) n
       IF(n<=0)THEN!prevents negative orders being chosen
         WRITE(6,*) 'The order of a polynomial should be positive, please try again.'
         CYCLE
       ELSE 
           EXIT
       END IF      
     END DO
     WRITE(6,*)'You inputed',n,' Is the correct? Type N for no, or Y for yes.'
     READ(5,*) figninput
     figninputtwo='q'
     IF (figninput=='n' .or. figninput =='N') THEN
       WRITE(6,*)'Please try again.'
     END IF
   END DO    
 ELSE IF(fignoption=='y' .or. fignoption=='Y') THEN
   WRITE(6,*)'Using same value for order of Taylor expansion.'!here so that if user chooses to use same value of n
 END IF!they know it has worked
 IF (figrunone=='e') THEN !if loop so intro only plays on first run
   WRITE(6,*) 'This program compares the taylor series to the nth degree between -2pi, and 2pi.'
   WRITE(6,*) 'By default the program increase the value of x in steps of 0.01, until 2pi is reached'
 END IF 

 IF(intchoice=='y' .or. intchoice=='Y')THEN
   figinter='p' !later users are given the choice if they want to use the same interval increases in x when they run
   !the program again, if they say yes, figinter is set to p, this later lets them know that the program is using 
   !the same value, if they say no, then  the choice is selected that they 
   
 ELSE IF(intchoice=='N'.or. intchoice=='n') THEN
   figinter='y' !if they pick no, figinter is set to Y, meaning they want to change the value of the interval
   IF(figrunone=='e') THEN !this is so they are not asked twice about changing the interval, this is only played
     !on the first run
     DO
       WRITE(6,*) 'If you would like to change the intervals that X increases in from 0.01, type Y, if not type N.'
       READ(5,*) figinter
       IF (figinter /= 'n' .and. figinter /='N' .and. figinter /='Y' .and. figinter /='y') THEN
         WRITE (6,*) 'Invalid option.' !If prevents wrong figures being picked
         CYCLE
       ELSE
         EXIT
       END IF
     END DO          
   END IF
 END IF

 IF(figinter=='y' .or. figinter=='Y')THEN !if they don't want to keep the same interval value, this runs
   DO WHILE(figintervaltwo=='n' .or. figintervaltwo=='N')!ensures they pick the interval they want
     WRITE(6,*)'Please type in the interval you want to use.'
     READ(5,*) interval
     WRITE(6,*)'You entered',interval,' Is this correct, type Y for yes, or N for no.'
     READ(5,*) figintervaltwo
     interval=abs(interval)
   END DO
 ELSE IF (figinter=='n' .or. figinter=='N') THEN
   WRITE(6,*)'Using 0.01 as interval'
   interval=0.01
 ELSE IF (figinter=='p') THEN
   WRITE(6,*)'Using same interval value.'            
 END IF 
        
 ALLOCATE(a(0:n))!allocates an array to be the same length as the desired polynomial

 factorial=1
 x=-2.0*pi

 OPEN(unit=20,file='taylor.dat')
!(main part of program)
 DO i=1,n  
   factorial=factorial*i!calculating factorial
   valueofderivative = ((-SQRT(5.0)/2.0)**(i)*(sin(-(i)*Atan(2.0))))
   a(i) = valueofderivative/factorial!This calculates the taylor expansion, and storing it in to the array
 END DO

 DO WHILE(x<=2.0*pi)!do while loop to set the interval between -2pi and 2pi
   taylorvalueatx=0.0 !makes sure taylor value is set to 0 inbetween do loops, as otherwise the taylor values are too big

   DO i=1,n !multiples each part of the taylor expansion by X to the desired order, for each x
     taylorvalueatx=(x**(i))*a(i)+taylorvalueatx !calculating the actual value of the taylor expansion, by multiplying
   END DO !x to the order and summing all values

   yexact=sin(x)*exp(-x/2.0) !calculates what the exact value is so the user can compare
   WRITE(20,*) x, taylorvalueatx, yexact !writes the values in a file
   x=x+interval
 END DO 
 
 CLOSE(20)

 WRITE(6,*)'Would you like to run the program again? '
 DO 
   WRITE(6,*) 'Type Y for yes, or N for no.' !this correlates to main do loop
   READ(5,*)fig
   IF (fig /= 'n' .and. fig /='N' .and. fig /='Y' .and. fig/='y') THEN !ensures valid options are entered
     WRITE (6,*) 'Invalid option.'
     CYCLE
   ELSE 
     EXIT
   END IF
 END DO
 IF (fig=='Y' .or. fig=='y') THEN
   WRITE(6,*) 'Would you like to use the same value for the order of the polynomial?'
   DO !allowing user to keep data for last run of program to make running the program multiple times more user friendly
     WRITE(6,*) 'Type Y for yes, or N for no.'
     READ(5,*) fignoption
     IF (fignoption /= 'n' .and. fignoption /='N' .and. fignoption /='Y' .and. fignoption /='y') THEN
       WRITE (6,*) 'Invalid option.'
       CYCLE
     ELSE
       EXIT
     END IF
   END DO
   WRITE(6,*)'Would you like to use the same value for the interval? ie the how much x increases inbetween steps.'
   DO
     WRITE(6,*) 'Type Y for yes, or N for no.'
     READ(5,*) intchoice
     IF (intchoice /= 'n' .and. intchoice /='N' .and. intchoice /='Y' .and. intchoice /='y') THEN
       WRITE (6,*) 'Invalid option.'
       CYCLE
     ELSE
       EXIT
     END IF
   END DO
 END IF
 figrunone='p'!setting this to stop introduction elements playing again
 DEALLOCATE(a) !deallocating a so that new n values can be entered 
 
 END DO
WRITE(6,*)'Thank you for using the program.'
END PROGRAM Taylorseriesexpansion    
     
        
           
           
           
     
   
    
