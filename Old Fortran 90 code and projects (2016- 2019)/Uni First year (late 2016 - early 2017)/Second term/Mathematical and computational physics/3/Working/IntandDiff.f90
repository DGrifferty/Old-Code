!--------------------------------------------------
!PHY1038 Assignment 3 Finite Difference Methods
!URN: 6408056
!March 2017
!--------------------------------------------------

PROGRAM Finite_Difference_Methods

INTEGER :: i
Double precision:: X, H, YatX, YatXplusH, P, YatXminusP, YatXplusP, Xorg, XplusP, XminusP, Q, YatXminusH, A, B, Fa, Fb, Hi
Double precision :: ItrapeziumRuleTwo, ERROR, ErrorCD, ErrorFD, ErrorTr, ErrorSi
Double precision, EXTERNAL:: Fx, Dforxh, DCenxh, ExactDerivativeAtX, DCentTwoxh, ItrapeziumRule, Iexact
CHARACTER::Fig, FigIntDiffChoice, XvalueChoice, HvalueChoice, Figrunonediff, ReRun, limitvalueChoice, Figrunone, FigrunoneInt
CHARACTER::NvalueChoice
!Some variables are used only in functions and warnings display
INTERFACE
    
  FUNCTION Fx(X)  
    Double precision, INTENT(IN) :: X
    Double precision:: Fx    
  END FUNCTION  Fx

  FUNCTION DForxh(X, H, Xorg)
    Double precision, INTENT(IN) :: H, Xorg
    Double precision:: DForxh, YatX, YatXplusH, X
    Double precision, EXTERNAL:: Fx
  END FUNCTION DForxh

  FUNCTION DCenxh(X, H, Xorg)
    Double precision, INTENT(IN) :: H, Xorg
    Double precision:: DCenxh, YatXminusP, YatXplusP, X, P
    Double precision, EXTERNAL:: Fx
  END FUNCTION DCenxh

  FUNCTION ExactDerivativeAtX(X)
    Double precision, INTENT(IN) :: X
    Double precision:: ExactDerivativeAtX
  END FUNCTION  ExactDerivativeAtX
 
  FUNCTION DCentTwoxh(X, H, Xorg)
    Double precision, INTENT(IN) :: H, Xorg
    Double precision, EXTERNAL:: Fx
    Double precision:: DCentTwoxh, YatX, YatXplusH, YatXminusH, X
  END FUNCTION 

  FUNCTION ItrapeziumRule(A, B, N)
    INTEGER, INTENT (IN) :: N
    INTEGER :: i
    Double precision, INTENT (IN) :: A, B
    Double precision, EXTERNAL :: Fx
    Double precision :: ItrapeziumRule, Fa, Fb, X, Hi, ItrapeziumRuleTwo
  END FUNCTION

  FUNCTION Iexact(A, B)
    Double precision, INTENT (IN) :: A, B
    Double precision :: Iexact
  END FUNCTION Iexact
 
  FUNCTION ISimpson(A, B, N)
    INTEGER, INTENT (IN) :: N
    INTEGER :: i
    Double precision, INTENT(IN) :: A, B
    Double precision :: ISimpson, ISimpsonOdd, ISimpsonEven, Hi, Fa, Fb
    Double precision, EXTERNAL :: Fx
  END FUNCTION
 
END INTERFACE


Fig='Y'!allows do while loop to run the first time
Figrunone='E'!setting these characters allows intros to play on first run
Figrunonediff='E'
FigrunoneInt='E'
DO WHILE (Fig=='y' .or. Fig=='Y')!allows user to re run program and choose between intergrating and differentiating
  IF (Figrunone=='E') THEN    
    WRITE(6,*) 'This program evaulates the function F(x)=1/(X^2+3X+2), it uses a series of approximation methods to calculate'
    WRITE(6,*) 'the derivative and intergrals of the function at chosen x values, and between chosen limits.'
    WRITE(6,*) 'It also calculates the exact intergrals and dervatives and compares the approximate methods to them.'
    Figrunone='p'!setting this prevents the intro playing unnecessarily after multiple runs
  END IF
 
  WRITE(6,*)'Would you like to intergrate or differentiate the function? Type I for intergrate, and D for differentiate.'
  READ(5,*) FigIntDiffChoice!this choice seperates the program up and allows users to differentiate the function
                            !at multiple x values easily without the program forcing them to intergrate

  IF (FigIntDiffChoice=='D' .or. FigIntDiffChoice=='d') THEN !DIFFERENTIATING PART
    Rerun='Y'!allowing do loop to play on first run
    IF (Figrunonediff=='E') THEN
      WRITE(6,*) 'In ''Forward difference Method'' The derivative at X is calculated by calculating the gradient between Y at X' 
      WRITE(6,*) 'and Y at X plus H, where H is an arbitary constant,  the smaller H, the more accurate the method is.'
      WRITE(6,*) 'In the ''Centred Difference Method'' The derivative at X is calculated by using the difference between Y at X'
      WRITE(6,*) 'minus H/2 and at Y at X plus H/2, again the method is more accurate using a smaller H, and generally more'
      WRITE(6,*) 'accurate than the forward difference method. The program can also use the CD method to approximate the secound'
      WRITE(6,*) 'derviative. The program also compares the exact derviative to the approximation methods and calculates the'
      WRITE(6,*) 'Percentage error in them.'
      Figrunonediff='P' !again set to stop long intro playing on re runs
    END IF
    DO WHILE(Rerun=='Y'.or. Rerun=='y')!this do loop allows the user to rerun the differentiating part multiple times more easily
      WRITE(6,*) 'Please enter the X value you would like the function to be differentiated at.'   
      DO
        READ(5,*) X
        Xorg=X!setting this to make sure that the X values aren't changed during functions
        WRITE(6,*) 'You entered', X,'Is this correct? Type Y for yes or N for no.'
        DO !this do loop will be cycled again if they say they entered the X value wrong
          READ(5,*) XvalueChoice
          IF (XvalueChoice/= 'Y' .and. XvalueChoice /= 'y' .and. XvalueChoice /= 'N' .and. XvalueChoice /= 'n') THEN
            WRITE(6,*) 'Invalid option, try again.'!this if command ensures only accepted values are inputed
            CYCLE
          ELSE 
            EXIT
          END IF
        END DO
        IF (XvalueChoice=='N' .or. XvalueChoice=='n') THEN
          WRITE(6,*) 'Please try again'
          CYCLE !cycling do loop here when they say wrong value is inputed 
        ELSE IF (XvalueChoice=='Y' .or. XvalueChoice=='y') THEN
          EXIT!Exiting do loop when they confirm they have entered correct value
        END IF
      END DO
      WRITE(6,*) 'Please enter the value of H you would like to use for the F.D. and the C.D. method!'
      DO !this do loop will be cycled again if they say they entered the H value wrong
         !thus prevents users having to go through whole program again to input a correct value
        READ(5,*) H
        WRITE(6,*) 'You entered', H,'Is this correct? Type Y for yes or N for no.'
        DO
          READ(5,*) HvalueChoice
          IF (HvalueChoice/= 'Y' .and. HvalueChoice /= 'y' .and. HvalueChoice /= 'N' .and. HvalueChoice /= 'n') THEN
            WRITE(6,*) 'Invalid option, try again.'! if command ensures only accepted values are inputed
            CYCLE
          ELSE 
            EXIT
          END IF
        END DO
        IF (HvalueChoice=='N' .or. HvalueChoice=='n') THEN
          WRITE(6,*) 'Please try again' 
          CYCLE!again cycling do loop here when they say wrong value is inputed 
        ELSE IF (HvalueChoice=='Y' .or. HvalueChoice=='y') THEN
          EXIT !Exiting do loop when they confirm they have entered correct value
        END IF
      END DO!after entering values the program uses functions to calculate the required values
      !it then displays them and calculates their percentage error from the exact value of the derivative at chosen X
      WRITE(6,*) 'At', X, 'Using', H, 'As H:'
      WRITE(6,*) 'The Forward Difference Method calculates the derivative to be', DForxh(X, H, Xorg)
      WRITE(6,*) 'The Centred Difference Method calculates the derivative to be', DCenxh(X, H, Xorg)
      WRITE(6,*) 'The exact derivative is', ExactDerivativeAtX(X)
      WRITE(6,*) 'The apporximate secound derivative is', DCentTwoxh(X, H, Xorg)
      ErrorFD = ((abs(DForxh(X, H, Xorg)-ExactDerivativeAtX(X))/abs(ExactDerivativeAtX(X))))*100
      ErrorCD = ((abs(DCenxh(X, H, Xorg)-ExactDerivativeAtX(X))/abs(ExactDerivativeAtX(X))))*100
      WRITE(6,*) 'The percentage error in the FD method is', ErrorFD
      WRITE(6,*) 'The percentage error in the CD method is', ErrorCD
      IF (ErrorCD>ErrorFD) THEN !letting user easily see which method was more accurate
        Error=ErrorCD-ErrorFD!calculating error difference
        WRITE(6,*) 'FD Method was more accurate in this case by',Error,'%'
      ELSE IF (ErrorFD>ErrorCD) THEN
        Error=ErrorFD-ErrorCD!calculating error difference
        WRITE(6,*) 'CD Method was more accurate in this case by',Error,'%'
      ELSE
        WRITE(6,*) 'Both approximation methods had equal percentage error in this case.'
      END IF 
      WRITE(6,*)'Would you like to try this again with different H and X values? Type Y for yes and N for no.'
      DO
        READ(5,*) ReRun!giving user choice to quickly rerun the differentiating part
        IF (ReRun/= 'Y' .and. ReRun /= 'y' .and. ReRun /= 'N' .and. ReRun /= 'n') THEN
          WRITE(6,*) 'Invalid option, try again.'
          CYCLE
        ELSE 
          EXIT
        END IF
      END DO      
    END DO!do while loop will replay if Y is selected
    
  ELSE IF (FigIntDiffChoice=='i' .OR. FigIntDiffChoice=='I') THEN !INTERGRATING PART
    Rerun='Y'!allowing do loop to play on first run
    DO WHILE(Rerun=='Y'.or. Rerun=='y')
      IF (FigrunoneInt=='E') THEN
        WRITE(6,*) 'This program can use the trapezium and simpson rule with a chosen number of even strips to approximate the'
        WRITE(6,*) 'intergral of the function, then compare the result to the exact intergral and calculate the percentage'
        WRITE(6,*) 'error for both approximation methods.'
        FigrunoneInt='P'!again stopping intro playing too many times
      END IF      
      WRITE(6,*) 'Please enter the upper intergration limit you would like to use.'   
      DO !do loop will only replay if they say they entered the wrong B and A values
        READ(5,*) B
        WRITE(6,*) 'Please enter the lower intergration limit you would like to use.'  
        READ(5,*) A 
        WRITE(6,*) 'You entered', B,'as the upper limit and', A, 'as the lower limit. Is this correct? Type Y for yes or N for no.'
        DO
          READ(5,*) limitvalueChoice
          IF (limitvalueChoice/= 'Y' .and. limitvalueChoice /= 'y' .and. limitvalueChoice /= 'N' .and. limitvalueChoice /= 'n') THEN
            WRITE(6,*) 'Invalid option, try again.'
            CYCLE!again allows user to quickly enter correct values should they make a mistake and preventing invalid choices
          ELSE 
            EXIT
          END IF
        END DO
        IF (limitvalueChoice=='N' .or. limitvalueChoice=='n') THEN
          WRITE(6,*) 'Please enter the upper limit again'
          CYCLE!cycling do loop to allow user to re enter values
        ELSE IF (limitvalueChoice=='Y' .or. limitvalueChoice=='y') THEN
          EXIT!exiting do loop
        END IF
      END DO
      WRITE(6,*) 'Please enter an even number of strips you would like to use.' 
      DO!do loop will cycle if incorrect N value, or odd N value is entered
        READ(5,*) N   
        IF (MOD(N,2)/=0) THEN !checking N is even for simpsons rule, and forcing user to re enter value if their 
                              !number is not even, i.e. remainder when divided by two is not 0
          WRITE(6,*) 'Please enter an even number of strips.'
          CYCLE
        END IF        
        WRITE(6,*) 'You entered', N, ' Is this correct? Type Y for yes or N for no.'
        DO
          READ(5,*) NvalueChoice
          IF (NvalueChoice/= 'Y' .and. NvalueChoice /= 'y' .and. NvalueChoice /= 'N' .and. NvalueChoice /= 'n') THEN
            WRITE(6,*) 'Invalid option, try again.'!checking only correct figures are entered
            CYCLE
          ELSE 
            EXIT
          END IF
        END DO
        IF (NvalueChoice=='N' .or. NvalueChoice=='n') THEN
          WRITE(6,*) 'Please try again'
          CYCLE
        ELSE IF (NvalueChoice=='Y' .or. NvalueChoice=='y') THEN
          EXIT
        END IF
      END DO
      !again uses function to calculate required values then calculates the percentage error of approximation methods compared
      !to exact integral
      WRITE(6,*) 'Using', A,'and', B,'As the limits, and', N ,' strips'
      WRITE(6,*) 'The trapezium rule gives:', ItrapeziumRule (A, B, N)
      WRITE(6,*) 'The Simpson rule gives:', ISimpson(A, B, N)
      WRITE(6,*) 'The exact intergral is:',Iexact(A, B)     
      ErrorTr = ((abs(ItrapeziumRule (A, B, N)-Iexact(A, B))/abs(Iexact(A, B))))*100
      ErrorSi = ((abs(ISimpson(A, B, N)-Iexact(A, B))/abs(Iexact(A, B))))*100
      WRITE(6,*) 'The percentage error in the Trapezium method is', ErrorTr
      WRITE(6,*) 'The percentage error in the Simpson method is', ErrorSi
      IF (ErrorTr>ErrorSi) THEN !if loop tells user which value was more accurate and by what percentage
        Error=ErrorTr-ErrorSi
        WRITE(6,*) 'Simpson Method was more accurate in this case by',Error,'%'
      ELSE IF (ErrorSi>ErrorTr) THEN
        Error=ErrorSi-ErrorTr
        WRITE(6,*) 'CD Method was more accurate in this case by',Error,'%'
      ELSE
        WRITE(6,*) 'Both approximation methods were equally accurate in this case.'
      END IF 
      WRITE(6,*)'Would you like to try this again with different values? Type Y for yes and N for no.'
      DO
        READ(5,*) ReRun!giving user choice to quickly rerun the intergrating part
        IF (ReRun/= 'Y' .and. ReRun /= 'y' .and. ReRun /= 'N' .and. ReRun /= 'n') THEN
          WRITE(6,*) 'Invalid option, try again.'
          CYCLE !do while loop will re play if Y is selected
        ELSE 
          EXIT
        END IF
      END DO       
        
    END DO
    
  ELSE 
    WRITE(6,*) 'Invalid option, please try again.'!will play if user doesn't enter D or I at beginning, 
    CYCLE
  END IF
   
  WRITE(6,*)'Would you to run the program again? This will allow you to choose between intergation and differentiation again.'
  WRITE(6,*) 'Type Y for yes and N for no.'
  DO 
    READ(5,*) fig
    IF (fig/= 'Y' .and. fig /= 'y' .and. fig /= 'N' .and. fig /= 'n') THEN
      WRITE(6,*) 'Invalid option, try again.' !if command again making sure correct characters are entered
      CYCLE
    ELSE 
       EXIT
    END IF
  END DO
  
END DO!end of main do while loop

WRITE(6,*)'Thank you for using the program.'!will play on exit

END PROGRAM Finite_Difference_Methods

FUNCTION Fx(X)!function to define F(x)
  IMPLICIT NONE
  Double precision, INTENT (IN) :: X
  Double precision:: Fx
  
  Fx = 1/(X**2.0+3.0*X+2)
  
END FUNCTION Fx

FUNCTION DForxh(X, H, Xorg)!function to approximate derivative at x using forward difference method
  IMPLICIT NONE 
  Double precision, INTENT(IN) :: H, Xorg
  Double precision, EXTERNAL:: Fx
  Double precision:: DForxh, YatX, YatXplusH, X
  
  YatX = Fx(X)
  X = X+H
  YatXplusH = Fx(X)
  Dforxh = (YatXplusH-YatX)/H
  X = Xorg
  
END FUNCTION DForxh

FUNCTION DCenxh(X, H, Xorg)!function to approximate derivative at x using centred difference method
  IMPLICIT NONE 
  Double precision, INTENT(IN) :: H, Xorg
  Double precision, EXTERNAL:: Fx
  Double precision:: DCenxh, YatXminusP, YatXplusP, X, P

  P = H/2
  X = X-P
  YatXminusP = Fx(X)
  X = X+2*P
  YatXplusP = Fx(X)
  DCenxh = (YatXplusP-YatXminusP)/H
  X = Xorg
  
END FUNCTION DCenxh

FUNCTION ExactDerivativeAtX(X)!function to calculate exact derivative at x
  IMPLICIT NONE
  Double precision, INTENT(IN) :: X
  Double precision:: ExactDerivativeAtX
 
  ExactDerivativeAtX = ((-2.0)*(X)-3.0)/((X**(2.0)+(3.0*X)+2.0)**2.0)

END FUNCTION  ExactDerivativeAtX

FUNCTION DCentTwoxh(X, H, Xorg)!function to calculate second derivative at X using centred difference method
  IMPLICIT NONE 
  Double precision, INTENT(IN) :: H, Xorg
  Double precision, EXTERNAL:: Fx
  Double precision:: DCentTwoxh, YatX, YatXplusH, YatXminusH, X

  YatX = Fx(X)
  X = X+H
  YatXplusH = Fx(X)
  X = X-(2*H)
  YatXminusH = Fx(X)
  DCentTwoxh = (YatXplusH-(2*YatX)+YatXminusH)/(H**2)
  X = Xorg

END FUNCTION 

FUNCTION ItrapeziumRule(A, B, N)!function to calculate integral using trapezium rule
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N
  INTEGER :: i
  Double precision, INTENT (IN) :: A, B
  Double precision, EXTERNAL :: Fx
  Double precision :: ItrapeziumRule, Fa, Fb, X, Hi, ItrapeziumRuleTwo
  
  ItrapeziumRuletwo=0

  X = A; Fa = Fx(X); X = B; Fb = Fx(X); Hi=abs(B-A)/N    

  DO i=1,N-1
    X = (A + (i*Hi)); ItrapeziumRuletwo = (Hi*Fx(X))+ ItrapeziumRuleTwo
  END DO
  
  ItrapeziumRule=(Hi/2)*(Fa + Fb)+ItrapeziumRuleTwo
    
END FUNCTION

FUNCTION Iexact(A, B)!function to calculate exact integral
  IMPLICIT NONE
  Double precision, INTENT (IN) :: A, B
  Double precision :: Iexact

  Iexact = log((B+1)/(B+2))-log((A+1)/(A+2))

END FUNCTION

FUNCTION ISimpson(A, B, N)!function to calculate integral using simpsons rule
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N
  INTEGER :: i
  Double precision, INTENT(IN) :: A, B
  Double precision :: ISimpson, ISimpsonOdd, ISimpsonEven, Hi, Fa, Fb, X
  Double precision, EXTERNAL :: Fx

  ISimpsonEven=0; ISimpsonOdd=0
  
  X = A; Fa = Fx(X); X = B; Fb = Fx(X); Hi=abs(B-A)/N

  DO i=1, N-1, 2
   X = (A + (i*Hi)); ISimpsonOdd = (4*Fx(X)) + ISimpsonOdd !summing up odd strips
  END DO

  DO i=2, N-2, 2
    X = (A + (i*Hi)); ISimpsonEven = (2*Fx(X)) + ISimpsonEven !summing up even strips
  END DO
  
  ISimpson=(Hi/3)*(Fa+Fb+ISimpsonOdd+ISimpsonEven) !

END FUNCTION ISimpson