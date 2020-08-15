!--------------------------------------------------
!PHY1038 Assignment 5 Linear Algebra
!URN: 6408056
!MAY 2017
!--------------------------------------------------

PROGRAM Linear_Algebra

IMPLICIT NONE

REAL, DIMENSION(1:3,1:3) :: A, B, C, D, Inverse 
REAL, DIMENSION(1:3,1:1) :: Answers, Solved
CHARACTER :: MainLoop, MatrixChoice, Sect, Choice, First

OPEN(unit=20,file='matrix.dat')  !Reading matrix A from file
READ(20,*) A(1,1), A(1,2), A(1,3)
READ(20,*) A(2,1), A(2,2), A(2,3) 
READ(20,*) A(3,1), A(3,2), A(3,3)
CLOSE(20)

WRITE(6,*) 'This program can calculate the determinant of a 3*3 array, its cofactor and its'
WRITE(6,*) 'inverse, it can also solve simulataneous equations with 3 unknows, given the'
WRITE(6,*) 'coefficents of the unknowns in matrix form, and the asnwers to the equations'
WRITE(6,*) 'It can do this with a user inputed matrix, or the preprogrammed matrix ''A'' that can be'
WRITE(6,*) 'used as an example.' 
!^Intro describing functions of program
MainLoop='Y' !Set so main do loop plays on the first run
Choice='N' !Set so that the programe asks user to enter matrix
First='T'!Set to describe to user in what form to enter the matrix on their first run

DO WHILE(MainLoop=='Y'.or. MainLoop=='y')
  
  IF (Choice=='N' .or. Choice=='n') THEN !This if is here, as at the end of the program the user
                                         !is asked if they want to use the same matrix again
                                         !Or not, if they select no they will be given the option
                                         !To enter a matrix/choose A
    WRITE(6,*) 'Would you like to use the preprogrammed matrix A? Where A='
    WRITE(6,*) A(1,1), A(1,2), A(1,3)
    WRITE(6,*) A(2,1), A(2,2), A(2,3) 
    WRITE(6,*) A(3,1), A(3,2), A(3,3)
    WRITE(6,*) 'Or would you like to use your own matrix?'
    WRITE(6,*) 'Type A to use the preprogrammed matrix, or B to use your own matrix.'
    !^offering user to choose between the preprogrammed matrix or for them to enter their own
    DO
      READ(5,*) MatrixChoice
      IF(MatrixChoice=='A' .OR. MatrixChoice=='a') THEN !Plays if they chose to use matrix A
        B=A
        ANSWERS(1,1)=0 ; ANSWERS(2,1)=3 ; ANSWERS(3,1)=1
        !^Setting matrix to A and the answers to equations to those given in assignment
        EXIT
      ELSE IF (MatrixChoice=='B' .OR. MatrixChoice=='b') THEN
        !^Plays if they chose to enter their own matrix
      
        DO!This Do loop will replay if user says they entered matrix wrong
          IF(First=='T') THEN !Will tell user in what form to enter matrix on their first run   
            WRITE(6,*) 'Please enter your matrix (1st row then 2nd etc.) If you intend to solve a'
            WRITE(6,*) 'simultaneous equation, the different coefficents of each '
            WRITE(6,*) 'unknown must be in the same columns, ie. all of X''s coefficents will'
            WRITE(6,*) 'be in column one, and Y''s in column one.'
            First='F'
            !^Prevents this writing to screen everytime
          ELSE 
            WRITE(6,*) 'Please enter your matrix'
            !^Will play on second, third run etc.
          END IF
              
          READ(6,*) B(1,1), B(1,2), B(1,3)
          READ(6,*) B(2,1), B(2,2), B(2,3) 
          READ(6,*) B(3,1), B(3,2), B(3,3)
          !^Reading users matrix
          WRITE(6,*) 'You entered:'
  
          WRITE(6,*) B(1,1), B(1,2), B(1,3)
          WRITE(6,*) B(2,1), B(2,2), B(2,3) 
          WRITE(6,*) B(3,1), B(3,2), B(3,3)

          WRITE(6,*) 'Is this correct? Type Y for yes, or N to try again.'
          !Checking matrix is correct       
          DO !This do loop will replay till user enters valid choice
            READ(5,*) Choice
            IF (Choice=='Y' .OR. Choice=='y' .OR. Choice=='N' .OR. Choice=='n') THEN
              EXIT
              !Do loop will exit if they enter valid choice
            END IF        
            WRITE(6,*) 'Please enter a valid choice.'          
          END DO
        
          IF (Choice=='Y' .OR. Choice=='y') THEN
            EXIT !The do loop will exit if they say they entered the correct matrix
            !Otherwise it will ask them to enter their matrix again
          END IF                    
        END DO !End of do loop asking them to enter their matrix        
            
        EXIT !Exits do loop asking them to choose between A and or the users matrix
             
       ELSE      
        WRITE(6,*) 'Please enter a valid choice.'
        !^Will play if user enters something other than A or B
      END IF
      !^End of if seperating A and B choices     
    END DO
    !^End of Do ensuring A or B is chosen
  END IF
  !^End of if that allows for the choice to use the same matrix again

  DO!Do loop will replay if a valid choice is not entered
    WRITE(6,*) 'If you would like to calculate the determinant of a 3*3 matrice type D, type C to'
    WRITE(6,*) 'calculate its cofactor, of S to solve simultaneous equations'
    READ(5,*) Sect 
    IF(Sect=='S' .or. Sect=='s' .or. Sect=='D' .or. Sect=='d' .or. Sect=='C' .or. Sect=='c') THEN
      EXIT!Exits do loop when valid choice is entered
    END IF
    WRITE(6,*) 'Please enter a valid choice'
  END DO
   
  IF (Sect=='D' .OR. Sect=='d') THEN!This if loop seperates the calculations possible in the
                                          !Program    
    WRITE(6,*) 'The determinant of your matrix is:', Determinant(B)!Calls on determinant function
                                                                   !And prints to screen    
  ELSE IF (Sect=='C' .OR. Sect=='c') THEN

    WRITE(6,*) 'The cofactor of your matrx is:'  
    CALL Cofactor(C,B)   !Calls on cofactor subroutine and prints cofactor to screen
    WRITE(6,*) C(1,1), C(1,2), C(1,3)
    WRITE(6,*) C(2,1), C(2,2), C(2,3) 
    WRITE(6,*) C(3,1), C(3,2), C(3,3)

  ELSE IF (Sect=='S' .OR. Sect=='s') THEN  

    IF (MatrixChoice=='B'.OR. MatrixChoice=='b') THEN !If user chose to enter their own matrix
                                                      !this plays, asking them to enter the 
                                                      !Answers to their simultaneous equations
      DO  !Will replay if the user says they entered the wrong answers    
        WRITE(6,*) 'Please enter the 3 answers to your simultaenous equations, in the order to'
        WRITE(6,*) 'conform with the order you entered the equations.'       
          READ(5,*) Answers(1,1), Answers(2,1), Answers(3,1) !Reading answers
          WRITE(6,*) 'You entered:'
          WRITE(6,*) Answers(1,1), Answers(2,1), Answers(3,1), 'Is this correct? Type Y for yes, or N'
          WRITE(6,*) 'to try again'
        
          DO!Do loop will replay if user doesnt enter valid choice
            READ(5,*) Choice
            IF(Choice=='Y' .or. Choice=='y' .or. Choice=='N' .or. Choice=='n') THEN
              EXIT!Exits do loop when valid choice is entered
            END IF 
            WRITE(6,*) 'Please enter valid choice'
          END DO
        
          IF(Choice=='Y' .or. Choice=='y') THEN
            EXIT!Exits do loop when they say they entered correct answers
          END IF
        
      END DO
    
    ELSE IF (MatrixChoice=='A' .OR. MatrixChoice=='a') THEN !Plays if user chose to use matrix A
      WRITE(6,*) 'As you chose A as the preprogrammed matrix the answers to the simultaneous'
      WRITE(6,*) 'equations have been set as:'
      WRITE(6,*) Answers(1,1), Answers(2,1), Answers(3,1)
      !Prints answers that were set earlier, as according to assignment script
    END IF

    CALL Cofactor(C, B)!Calculating cofactor
    D = Transpose(C) !Transposing cofactor
    Inverse=(1/Determinant(B))*D  !Calculating inverse
    Solved=Matmul(Inverse,Answers) !Saving unknowns values in matrice, by multiplying inverse by
                                   !The simultaneous equations  

    WRITE(6,*) 'The value of your unknown in your matrices collumn one is', Solved(1,1)
    WRITE(6,*) 'Column two''s unknown is',Solved(2,1)
    WRITE(6,*) 'Column three''s unknown is',Solved(3,1) !Printing answers
  
  END IF!End of if that seperates determinant/cofactor/equations sections
  
  WRITE(6,*) 'Would you like to run the program again?'  
  DO !will replay if valid choice is not entered
    WRITE(6,*) 'Type Y for yes, or N for no.'
    READ(5,*) Mainloop
    IF(Mainloop=='Y' .or. Mainloop=='y' .or. Mainloop=='N' .or. Mainloop=='n') THEN
      EXIT!Exits when valid choice is entered
    END IF 
    WRITE(6,*) 'Please enter valid choice'
  END DO
  IF(Mainloop=='N' .or. Mainloop=='n') THEN
    EXIT!Exits main do loop when N is selected, exiting programme
  END IF

  WRITE(6,*)'Would you like to use the same matrix?'
  DO !Do loop replays when valid choice is not entered
    WRITE(6,*) 'Type Y for yes, or N for no.'
    READ(5,*) Choice !This choice is then used at the start of the programme, if Y is entered the
                     !Option to enter a matrix/use matrix A is skipped, and the programme skips
                     !to asking what calculation they want to do
    IF(Choice=='Y' .or. Choice=='y' .or. Choice=='N' .or. Choice=='n') THEN
      EXIT !Exits do loop when valid choice is entered    
    END IF 
    WRITE(6,*) 'Please enter valid choice'
  END DO
  
END DO
  
WRITE(6,*) 'Thank you for using the programme.'  !Plays on exit  

CONTAINS

FUNCTION Determinant(B)
  IMPLICIT NONE
  REAL, DIMENSION(1:3,1:3) :: B
  REAL :: Determinant, X

  X = B(1,1)*(B(2,2)*B(3,3)-B(2,3)*B(3,2))!calculates the three 2*2 determinants

  X = X - B(2,1)*((B(1,2)*B(3,3))-(B(1,3)*B(3,2)))

  Determinant = X+B(3,1)*((B(1,2)*B(2,3))-(B(1,3)*B(2,2))) !Final sum

END FUNCTION

SUBROUTINE Cofactor(C,B)
  IMPLICIT NONE
  REAL, DIMENSION(1:3,1:3) :: B, C

  C(1,1) = B(2,2)*B(3,3)-B(2,3)*B(3,2)

  C(1,2) = -(B(2,1)*B(3,3)-B(2,3)*B(3,1))

  C(1,3) = B(2,1)*B(3,2)-B(2,2)*B(3,1)

  C(2,1) = -(B(1,2)*B(3,3)-B(1,3)*B(3,2))

  C(2,2) = B(1,1)*B(3,3)-B(1,3)*B(3,1)

  C(2,3) = -(B(1,1)*B(3,2)-B(1,2)*B(3,1))

  C(3,1) = B(1,2)*B(2,3)-B(1,3)*B(2,2)

  C(3,2) = -(B(1,1)*B(2,3)-B(1,3)*B(2,1))

  C(3,3) = (B(1,1)*B(2,2)-B(1,2)*B(2,1))

!Calculates all the cofactor values in normal way, ie calcualtes determinants of values that
!Do not have a matching row or collumn, and multiplying them by -1 to the power of the sum
!of row and column, it saves the values to matrix C, the value is then printed outside the 
!Subroutine so that the cofactor is not printed unnecessarily when calculating inverse
END SUBROUTINE

END PROGRAM