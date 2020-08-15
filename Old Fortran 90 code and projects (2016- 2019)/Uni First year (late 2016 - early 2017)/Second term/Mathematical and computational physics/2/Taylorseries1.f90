!--------------------------------------------------
!PHY1038 Assignment 2 Series Computation
!URN: 6408056
!February 2017
!--------------------------------------------------
PROGRAM Taylorseriesexpansion
IMPLICIT NONE

CHARACTER::
INTEGER::
REAL::

figrunone='e' !setting this so introduction to program is only printed on the first run
fig='y'
figninput='n'
DO WHILE(fig=='y' .or. fig=='Y')!This do while loop allows the user to run the program again at the end, or exit it
  figninputtwo=='l' !so on reruns, the user is not told to input n again
  IF (figrunone='e') THEN
    WRITE(6,*) 'Introduction'
    END IF
    WRITE(6,*)'Please input the order you would like the program to calculate the polynomial for?'
  DO WHILE(figninput=='n' .or. figninput=='N')
    IF(figninputtwo=='q') THEN
      WRITE(6,*) 'Please input n again.'
      END IF
    READ(5,*)n
    WRITE(6,*)'You inputed ',n,'Is the correct? Type N for no, or Y for yes.'
    READ(5,*) figinput
    figinputtwo=='q'
    END DO
  so here we are
  programasd can compare two functions, this to this, input steps ie, 0.1, first would be zero, secound would be 0.1, default is this
    
    
  
  
  
  

