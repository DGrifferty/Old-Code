!--------------------------------------------------
!PHY1038 Assignment 2 Series Computation
!URN: 6408056
!February 2017
!--------------------------------------------------
PROGRAM Taylorseriesexpansion
IMPLICIT NONE

CHARACTER::figrunone, fig, figninput, fignoption, figinterchoice, figninputtwo, figintervaltwo, figinter
INTEGER:: n, i, p
integer, parameter:: k14 = selected_int_kind(14)
integer(kind=k14) :: factorial
REAL:: interval, taylorvalueatx, valueofderivative, y, x, pi
REAL, DIMENSION(:), ALLOCATABLE::a
figrunone='e' !setting this so introduction to program is only printed on the first run
fig='y'
figninput='n'
fignoption='n' 
figinterchoice='N'
pi=4*atan(1.0)
DO WHILE(fig=='y' .or. fig=='Y')!This do while loop allows the user to run the program again at the end, or exit it
 figninputtwo='l' !so on reruns, the user is not told the introduction to the program again
 figintervaltwo='n'
 figninput='n'
 IF (figrunone=='e') THEN
   WRITE(6,*) 'Introduction'
   END IF
 IF(fignoption=='n' .or. fignoption=='N') THEN
   WRITE(6,*)'Please input the order you would like the program to calculate the polynomial for?'
   DO WHILE (figninput=='n' .or. figninput=='N' )    
     DO
       READ(5,*) n
       IF(n<=0)THEN
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
     WRITE(6,*)'Using same value of n.'
     END IF
 IF (figrunone=='e') THEN 
   WRITE(6,*) 'This program compares the taylor series to the nth degree between -2pi, and 2pi.'
   WRITE(6,*) 'By default the program increase the value of x in steps of 0.01, until 2pi is reached'
   END IF 
 IF(figinterchoice=='y' .or. figinterchoice=='Y')THEN
   figinter='p'
   ELSE IF(figinterchoice=='N'.or. figinterchoice=='n') THEN
     figinter='y'
     IF(figrunone=='e') THEN
       DO
         WRITE(6,*) 'If you would like to change the intervals from 0.01, type y, if not type n'
         READ(5,*) figinter
         IF (figinter /= 'n' .and. figinter /='N' .and. figinter /='Y' .and. figinter /='y') THEN
           WRITE (6,*) 'Invalid option.'
           CYCLE
           ELSE
             EXIT
             END IF
         END DO          
       END IF
     END IF
 IF(figinter=='y' .or. figinter=='Y')THEN
   DO WHILE(figintervaltwo=='n' .or. figintervaltwo=='N')
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
 ALLOCATE(a(n))
 factorial=1
 x=-2*pi
 OPEN(unit=20,file='taylor.dat')
 DO i=1,n
   factorial=factorial*i
   p=i-1
   IF (p>0) THEN
     valueofderivative = ((-(5)**0.5)/2)**(p)*(sin(-(p)*Atan(2.0)))
     a(i) = valueofderivative/factorial
     ELSE IF(p==0) THEN
       valueofderivative=sin(0.0)*exp(-0.0/2.0)
       a(i) = valueofderivative
       END IF
   END DO
 DO WHILE(x<=2*pi)
   taylorvalueatx=0
   DO i=1,n
     p=i-1
     taylorvalueatx=(x**(p))*a(i)+taylorvalueatx
     END DO
   y=sin(x)*exp(-x/2)
   WRITE(6,*) x, taylorvalueatx, y
   x=x+interval
   END DO
   
 WRITE(6,*)'Would you like to run the program again? '
 DO 
   WRITE(6,*) 'Type Y for yes, or N for no.'
   READ(5,*)fig
   IF (fig /= 'n' .and. fig /='N' .and. fig /='Y' .and. fig/='y') then
     WRITE (6,*) 'Invalid option.'
     CYCLE
       ELSE 
         EXIT
         END IF
   END DO
 IF (fig=='Y' .or. fig=='y') THEN
   WRITE(6,*) 'Would you like to use the same value for the order of the polynomial?'
   DO
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
     READ(5,*) figinterchoice
     IF (figinterchoice /= 'n' .and. figinterchoice /='N' .and. figinterchoice /='Y' .and. figinterchoice /='y') THEN
       WRITE (6,*) 'Invalid option.'
       CYCLE
       ELSE
       EXIT
       END IF
     END DO
   END IF
 figrunone='p'
 DEALLOCATE(a)  
 END DO
WRITE(6,*)'Thank you for using the program.'
END PROGRAM Taylorseriesexpansion    
     
        
           
           
           
     
   
     !     
     
     !valueofderivative = ((-5.0**(0.5))/(i-1.0))*sin(-(i-1.0)*atan(2.0))
      ! taylorvalueatx=(valueofderivative*(x**(i-1)))/factorial
 !
 
  
       
           

!figrunone='q'
!if they say yes to wanting to change value figinter=='y' else if they say they want to keep the same value, fig inter should equal p

 !figninputtwo=='q'
    
  
  !so here we are
  !programasd can compare two functions, this to this, input steps ie, 0.1, first would be zero, secound would be 0.1, default is this
    
    
  
  
  
  

