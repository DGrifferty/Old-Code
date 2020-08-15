!--------------------------------------------------
!PHY1038 Assignment 3 Finite Difference Methods
!URN: 6408056
!March 2017
!--------------------------------------------------

PROGRAM Finite_Difference_Methods

INTEGER, PARAMETER:: DP = 2 !Set so transfers to functions!
INTEGER :: i
REAL(KIND = DP):: X, H, YatX, YatXplusH, P, YatXminusP, YatXplusP, Xorg, XplusP, XminusP, Q, YatXminusH, A, B, Fa, Fb, Hi
REAL(KIND = DP) :: ItrapeziumRuleTwo, ERROR, ErrorCD, ErrorFD, ErrorTr, ErrorSi
REAL(KIND = DP), EXTERNAL:: Fx, Dforxh, DCenxh, ExactDerivativeAtX, DCentTwoxh, ItrapeziumRule, Iexact
CHARACTER::Fig, FigIntDiffChoice, XvalueChoice, HvalueChoice, Figrunonediff, ReRun, limitvalueChoice, Figrunone, FigrunoneInt
CHARACTER::NvalueChoice

INTERFACE !use module
    
  FUNCTION Fx(X)  !removex, xh
    REAL(KIND = DP), INTENT(IN) :: X
    REAL(KIND = DP):: Fx    
  END FUNCTION  Fx

  FUNCTION DForxh(X, H, Xorg)
    REAL(KIND = DP), INTENT(IN) :: H, Xorg
    REAL(KIND = DP):: DForxh, YatX, YatXplusH, X
    REAL(KIND = DP), EXTERNAL:: Fx
  END FUNCTION DForxh

  FUNCTION DCenxh(X, H, Xorg)
    REAL(KIND = DP), INTENT(IN) :: H, Xorg
    REAL(KIND = DP):: DCenxh, YatXminusP, YatXplusP, X, P
    REAL(KIND = DP), EXTERNAL:: Fx
  END FUNCTION DCenxh

  FUNCTION ExactDerivativeAtX(X)
    REAL(KIND = DP), INTENT(IN) :: X
    REAL(KIND = DP):: ExactDerivativeAtX
  END FUNCTION  ExactDerivativeAtX
 
  FUNCTION DCentTwoxh(X, H, Xorg)
    REAL(KIND = DP), INTENT(IN) :: H, Xorg
    REAL(KIND = DP), EXTERNAL:: Fx
    REAL(KIND = DP):: DCentTwoxh, YatX, YatXplusH, YatXminusH, X
  END FUNCTION 

  FUNCTION ItrapeziumRule(A, B, N)
    INTEGER, INTENT (IN) :: N
    INTEGER :: i
    REAL(KIND = 2), INTENT (IN) :: A, B
    REAL(KIND = 2), EXTERNAL :: Fx
    REAL(KIND = 2) :: ItrapeziumRule, Fa, Fb, X, Hi, ItrapeziumRuleTwo
  END FUNCTION

  FUNCTION Iexact(A, B)
    REAL(KIND=2), INTENT (IN) :: A, B
    REAL(KIND=2) :: Iexact
  END FUNCTION Iexact
 
  FUNCTION ISimpson(A, B, N)
    INTEGER, INTENT (IN) :: N
    INTEGER :: i
    REAL(KIND=2), INTENT(IN) :: A, B
    REAL(KIND=2) :: ISimpson, ISimpsonOdd, ISimpsonEven, Hi, Fa, Fb
    REAL(KIND = 2), EXTERNAL :: Fx
  END FUNCTION
 
END INTERFACE


Fig='Y'
Figrunone='E'
Figrunonediff='E'
FigrunoneInt='E'
DO WHILE (Fig=='y' .or. Fig=='Y')
  IF (Figrunone=='E') THEN    
    WRITE(6,*) 'This program evalutes the function F(x)=1/(X^2+3X+2). It can calculate its exact derviative at a chosen value of'
    WRITE(6,*) 'X and calculate it''s aproximate derivative using ''Forward difference method'' and ''Centred Difference Method'''
    WRITE(6,*) 'The program can then calculate the'
  END IF
  Figrunone='p'
  


  WRITE(6,*)'Would you like to intergrate or differentiate the function? Type I for intergrate, and D for differentiate.'
  READ(5,*) FigIntDiffChoice

  IF (FigIntDiffChoice=='D' .or. FigIntDiffChoice=='d') THEN
    Rerun='Y'
    DO WHILE(Rerun=='Y'.or. Rerun=='y')
      WRITE(6,*) 'Please enter the X value you would like the function to be differentiated at.'   
      DO
        READ(5,*) X
        Xorg=X
        WRITE(6,*) 'You entered', X,'Is this correct? Type Y for yes or N for no.'
        DO
          READ(5,*) XvalueChoice
          IF (XvalueChoice/= 'Y' .and. XvalueChoice /= 'y' .and. XvalueChoice /= 'N' .and. XvalueChoice /= 'n') THEN
            WRITE(6,*) 'Invalid option, try again.'
            CYCLE
          ELSE 
            EXIT
          END IF
        END DO
        IF (XvalueChoice=='N' .or. XvalueChoice=='n') THEN
          WRITE(6,*) 'Please try again'
          CYCLE
        ELSE IF (XvalueChoice=='Y' .or. XvalueChoice=='y') THEN
          EXIT
        END IF
      END DO
      IF (Figrunonediff=='E') THEN
        WRITE(6,*) 'In ''Forward difference Method'' The derivative at X is calculated by calculating the gradient between Y at X' 
        WRITE(6,*) 'and Y at X plus H, where H is an arbitary constant,  the smaller H, the more accurate the method is.'
        WRITE(6,*) 'In the ''Centred Difference Method'' The derivative at X is calculated by using the difference between Y at X'
        WRITE(6,*) 'minus H/2 and at Y at X plus H/2, again the method is more accurate using a smaller H, and generally more'
        WRITE(6,*) 'accurate than the forward difference method. The program can also use the CD method to approximate the secound'
        WRITE(6,*) 'derviative. The program also compares the exact derviative to the approximation methods and calculates the'
        WRITE(6,*) 'Percentage error in them.'
      END IF
      Figrunonediff='p'

      WRITE(6,*) 'Please enter the value of H you would like to use for the F.D. and the C.D. method!'
      DO
        READ(5,*) H
        WRITE(6,*) 'You entered', H,'Is this correct? Type Y for yes or N for no.'
        DO
          READ(5,*) HvalueChoice
          IF (HvalueChoice/= 'Y' .and. HvalueChoice /= 'y' .and. HvalueChoice /= 'N' .and. HvalueChoice /= 'n') THEN
            WRITE(6,*) 'Invalid option, try again.'
            CYCLE
          ELSE 
            EXIT
          END IF
        END DO
        IF (HvalueChoice=='N' .or. HvalueChoice=='n') THEN
          WRITE(6,*) 'Please try again'
          CYCLE
        ELSE IF (HvalueChoice=='Y' .or. HvalueChoice=='y') THEN
          EXIT
        END IF
      END DO

      WRITE(6,*) 'At', X, 'Using', H, 'As H:'
      WRITE(6,*) 'The Forward Difference Method calculates the derivative to be', DForxh(X, H, Xorg)
      WRITE(6,*) 'The Centred Difference Method calculates the derivative to be', DCenxh(X, H, Xorg)
      WRITE(6,*) 'The exact derivative is', ExactDerivativeAtX(X)
      WRITE(6,*) 'The apporximate secound derivative is', DCentTwoxh(X, H, Xorg)
      ErrorFD = ((abs(DForxh(X, H, Xorg)-ExactDerivativeAtX(X)/ExactDerivativeAtX(X)))*100
      ErrorCD = ((abs(DCenxh(X, H, Xorg)-ExactDerivativeAtX(X)/ExactDerivativeAtX(X)))*100
      WRITE(6,*) 'The percentage error in the FD method is', ErrorFD
      WRITE(6,*) 'The percentage error in the CD method is', ErrorCD
      IF (ErrorCD>ErrorFD) THEN
        Error=ErrorCD-ErrorFD
        WRITE(6,*) 'FD Method was more accurate in this case by',Error,'%'
      ELSE IF (ErrorFD>ErrorCD) THEN
        Error=ErrorFD-ErrorCD
        WRITE(6,*) 'CD Method was more accurate in this case by',Error,'%'
      ELSE
        WRITE(6,*) 'Both approximation methods were equally accurate in this case.'
      END IF 
      WRITE(6,*)'Would you like to try this again with different H and X values? Type Y for yes and N for no.'
      DO
        READ(5,*) ReRun
        IF (ReRun/= 'Y' .and. ReRun /= 'y' .and. ReRun /= 'N' .and. ReRun /= 'n') THEN
          WRITE(6,*) 'Invalid option, try again.'
          CYCLE
        ELSE 
          EXIT
        END IF
      END DO      
    END DO
    


  ELSE IF (FigIntDiffChoice=='i' .OR. FigIntDiffChoice=='I') THEN 
    Rerun='Y'
    DO WHILE(Rerun=='Y'.or. Rerun=='y')
      IF (FigrunoneInt=='E') THEN
        WRITE(6,*) 'Intergration intro'  
      END IF
      FigrunoneInt='p'
      WRITE(6,*) 'Please enter the upper intergration limit you would like to use.'   
      DO
        READ(5,*) B
        WRITE(6,*) 'Please enter the lower intergration limit you would like to use.'  
        READ(5,*) A 
        WRITE(6,*) 'You entered', B,'as the upper limit and', A, 'as the lower limit. Is this correct? Type Y for yes or N for no.'
        DO
          READ(5,*) limitvalueChoice
          IF (limitvalueChoice/= 'Y' .and. limitvalueChoice /= 'y' .and. limitvalueChoice /= 'N' .and. limitvalueChoice /= 'n') THEN
            WRITE(6,*) 'Invalid option, try again.'
            CYCLE
          ELSE 
            EXIT
          END IF
        END DO
        IF (limitvalueChoice=='N' .or. limitvalueChoice=='n') THEN
          WRITE(6,*) 'Please enter the upper limit again'
          CYCLE
        ELSE IF (limitvalueChoice=='Y' .or. limitvalueChoice=='y') THEN
          EXIT
        END IF
      END DO
      WRITE(6,*) 'Please enter the number of strips you would like to use.' !find a way to make sure number is even!
      DO
        READ(5,*) N   
        WRITE(6,*) 'You entered', N, ' Is this correct? Type Y for yes or N for no.'
        DO
          READ(5,*) NvalueChoice
          IF (NvalueChoice/= 'Y' .and. NvalueChoice /= 'y' .and. NvalueChoice /= 'N' .and. NvalueChoice /= 'n') THEN
            WRITE(6,*) 'Invalid option, try again.'
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
       
      WRITE(6,*) 'Using', A,'and', B,'As the limits, and', N ,' strips'
      WRITE(6,*) 'The trapezium rule gives:', ItrapeziumRule (A, B, N)
      WRITE(6,*) 'The Simpson rule gives:', ISimpson(A, B, N)
      WRITE(6,*) 'The exact intergral is:',Iexact(A, B)     
      ErrorTr = ((abs(ItrapeziumRule (A, B, N)-Iexact(A, B))/abs(Iexact(A, B))))*100
      ErrorSi = ((abs(ISimpson(A, B, N)-Iexact(A, B))/abs(Iexact(A, B))))*100
      WRITE(6,*) 'The percentage error in the Trapezium method is', ErrorTr
      WRITE(6,*) 'The percentage error in the Simpson method is', ErrorSi
      IF (ErrorTr>ErrorSi) THEN
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
        READ(5,*) ReRun
        IF (ReRun/= 'Y' .and. ReRun /= 'y' .and. ReRun /= 'N' .and. ReRun /= 'n') THEN
          WRITE(6,*) 'Invalid option, try again.'
          CYCLE
        ELSE 
          EXIT
        END IF
      END DO       
        
    END DO
    
  ELSE 
    WRITE(6,*) 'Invalid option, please try again.'
    CYCLE
  END IF
   
  WRITE(6,*)'Would you to run the program again? Type Y for yes and N for no.'
  DO
    READ(5,*) fig
    IF (fig/= 'Y' .and. fig /= 'y' .and. fig /= 'N' .and. fig /= 'n') THEN
      WRITE(6,*) 'Invalid option, try again.'
      CYCLE
    ELSE 
       EXIT
    END IF
  END DO
END DO
WRITE(6,*)'Thank you for using the program.'

END PROGRAM Finite_Difference_Methods

FUNCTION Fx(X)
  IMPLICIT NONE
  REAL(KIND = 2), INTENT (IN) :: X
  REAL(KIND = 2):: Fx
  Fx = 1/(X**2.0+3.0*X+2)
END FUNCTION Fx

FUNCTION DForxh(X, H, Xorg)
  IMPLICIT NONE 
  REAL(KIND = 2), INTENT(IN) :: H, Xorg
  REAL(KIND = 2), EXTERNAL:: Fx
  REAL(KIND = 2):: DForxh, YatX, YatXplusH, X
  YatX = Fx(X)
  X = X+H
  YatXplusH = Fx(X)
  Dforxh = (YatXplusH-YatX)/H
  X = Xorg
END FUNCTION DForxh

FUNCTION DCenxh(X, H, Xorg)
  IMPLICIT NONE 
  REAL(KIND = 2), INTENT(IN) :: H, Xorg
  REAL(KIND = 2), EXTERNAL:: Fx
  REAL(KIND = 2):: DCenxh, YatXminusP, YatXplusP, X, P

  P = H/2
  X = X-P
  YatXminusP = Fx(X)
  X = X+2*P
  YatXplusP = Fx(X)
  DCenxh = (YatXplusP-YatXminusP)/H
  X = Xorg
END FUNCTION DCenxh

FUNCTION ExactDerivativeAtX(X)
  IMPLICIT NONE
  REAL(KIND = 2), INTENT(IN) :: X
  REAL(KIND = 2):: ExactDerivativeAtX
 
  ExactDerivativeAtX = ((-2.0)*(X)-3.0)/((X**(2.0)+(3.0*X)+2.0)**2.0)

END FUNCTION  ExactDerivativeAtX

FUNCTION DCentTwoxh(X, H, Xorg)
  IMPLICIT NONE 
  REAL(KIND = 2), INTENT(IN) :: H, Xorg
  REAL(KIND = 2), EXTERNAL:: Fx
  REAL(KIND = 2):: DCentTwoxh, YatX, YatXplusH, YatXminusH, X

  YatX = Fx(X)
  X = X+H
  YatXplusH = Fx(X)
  X = X-(2*H)
  YatXminusH = Fx(X)
  DCentTwoxh = (YatXplusH-(2*YatX)+YatXminusH)/(H**2)
  X = Xorg

END FUNCTION 

FUNCTION ItrapeziumRule(A, B, N)
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N
  INTEGER :: i
  REAL(KIND = 2), INTENT (IN) :: A, B
  REAL(KIND = 2), EXTERNAL :: Fx
  REAL(KIND = 2) :: ItrapeziumRule, Fa, Fb, X, Hi, ItrapeziumRuleTwo
  
  ItrapeziumRuletwo=0

  X = A; Fa = Fx(X); X = B; Fb = Fx(X); Hi=abs(B-A)/N    

  DO i=1,N-1
    X = (A + (i*Hi)); ItrapeziumRuletwo = (Hi*Fx(X))+ ItrapeziumRuleTwo
  END DO
  
  ItrapeziumRule=(Hi/2)*(Fa + Fb)+ItrapeziumRuleTwo
    
END FUNCTION

FUNCTION Iexact(A, B)
  IMPLICIT NONE
  REAL(KIND=2), INTENT (IN) :: A, B
  REAL(KIND=2) :: Iexact

  Iexact = log((B+1)/(B+2))-log((A+1)/(A+2))

END FUNCTION

FUNCTION ISimpson(A, B, N)
  IMPLICIT NONE
  INTEGER, INTENT (IN) :: N
  INTEGER :: i
  REAL(KIND=2), INTENT(IN) :: A, B
  REAL(KIND=2) :: ISimpson, ISimpsonOdd, ISimpsonEven, Hi, Fa, Fb, X
  REAL(KIND = 2), EXTERNAL :: Fx

  ISimpsonEven=0; ISimpsonOdd=0
  
  X = A; Fa = Fx(X); X = B; Fb = Fx(X); Hi=abs(B-A)/N

  DO i=1, N-1, 2
   X = (A + (i*Hi)); ISimpsonOdd = (4*Fx(X)) + ISimpsonOdd
  END DO

  DO i=2, N-2, 2
    X = (A + (i*Hi)); ISimpsonEven = (2*Fx(X)) + ISimpsonEven
  END DO
  
  ISimpson=(Hi/3)*(Fa+Fb+ISimpsonOdd+ISimpsonEven)

END FUNCTION ISimpson
    
  

  

