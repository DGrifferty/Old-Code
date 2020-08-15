!--------------------------------------------------
!PHY1038 Assignment 4 Matrix Geometry
!URN: 6408056
!MAY 2017
!--------------------------------------------------

PROGRAM Matrix_Geometry

IMPLICIT NONE

REAL, DIMENSION(1:3,1:1)::Array, Rot, Tran, Arrayone, Arraytwo, Arraythree
REAL:: Inputangle, Xtranslation, Ytranslation !inputangle is the angle they want to roate it by, Array angle is the Arrays angle in polar
CHARACTER::Mainloop, Intro, Main, Correction
INTEGER:: I, N, Itwo

Mainloop='Y'
Intro='Y'

DO WHILE (Mainloop=='Y' .OR. Mainloop=='y')
  
  IF(Intro=='Y') THEN 
    WRITE(6,*) 'INTRO HERE'!REMEMBER TO PUT AN INTRO
    Intro='N'
  END IF

  WRITE(6,*)'If you would like to rotate a matrix by a paticular angle type, R, for a translation'
  WRITE(6,*)'type T, to rotate a triangle about its centre of mass type, C'

  DO
    READ(5,*) Main

    IF(Main=='r' .or. Main=='R' .or. Main=='t' .or. Main=='T' .or. Main=='c' .or. Main== 'C')THEN
      EXIT
    END IF

    WRITE(6,*) 'Please enter a valid option'
    WRITE(6,*) 'Enter, R, for rotation, T, for translation, or C to rotate a triangle about its'
    WRITE(6,*) 'centre of mass.'
    
  END DO

    
  IF (Main=='r' .or. Main=='R') THEN
    
    DO 

      WRITE(6,*) 'How many points would you like to rotate?'
      READ(5,*) N
      WRITE(6,*) 'By what angle, in radians, would you like to rotate them by?'
      READ(5,*) Inputangle   
      WRITE(6,*) 'You entered',n,'as the amount of points you want to rotate, and', Inputangle 
      WRITE(6,*) 'As the amount you want to rotate them by, is this correct?'      
      DO 
        WRITE(6,*) 'Type Y for yes, or N for no.'  
        READ(5,*) Correction
        IF (Correction=='n' .or. Correction=='y' .or. Correction=='N' .or. Correction=='Y') THEN
          EXIT
        END IF       
        WRITE(6,*) 'Please enter a valid choice.'
      END DO
      
      IF (Correction=='Y' .or. Correction=='y')THEN
        EXIT 
      END IF

    END DO
    
    OPEN(unit=20,file='rotate.dat')
    
    DO I=1,N
    
      
      WRITE(6,*) 'Please enter Array number',i,' that you want to rotate, x then y.'
      READ(5,*) Array(1,1), Array(2,1)
      
      Array(3,1)=1  
          
      Rot = Rotation(Array,Inputangle)
      
      WRITE(6,*)'Your number',i,' rotated array is:'
      WRITE(6,*) Rot(1,1), Rot(2,1)
      WRITE(20,*) Array(1,1), Array(2,1), Rot(1,1), Rot(2,1)
      
      IF (I==1) THEN
        Arrayone=Array; Arraytwo=Rot
      ELSE IF (I==N) THEN
        WRITE(20,*) Arrayone(1,1), Arrayone(2,1), Arraytwo(1,1), Arraytwo(2,1)
      END IF
        
    END DO 
    
    CLOSE(20)

  ELSE IF (Main=='t' .or. Main=='T') THEN
       
    DO 

      WRITE(6,*) 'How many points would you like to translate?'
      READ(5,*) N
      WRITE(6,*) 'Please enter the amount you want to translate x by.'
      READ(5,*) Xtranslation
      WRITE(6,*) 'Please enter the amount you want to translate y by.'
      READ(5,*) Ytranslation
      WRITE(6,*) 'You entered',n,'as the amount of points you want to translate, and', Xtranslation
      WRITE(6,*) Ytranslation, 'as the amount to translate x, then y by.'
      
      DO
        WRITE(6,*) 'Type Y for yes, or N for no.'  
        READ(5,*) Correction
        IF (Correction=='n' .or. Correction=='y' .or. Correction=='N' .or. Correction=='Y') THEN
          EXIT
        END IF       
        WRITE(6,*) 'Please enter a valid choice.'
      END DO
      
      IF (Correction=='Y' .or. Correction=='y')THEN
        EXIT 
      END IF
           
    END DO
    
    OPEN(unit=20,file='rotate.dat')     

    DO Itwo=1,N
           
      WRITE(6,*) 'Please enter Array number',Itwo,' that you want to translate, x then y.'
      READ(5,*) Array(1,1), Array(2,1)
      
      Array(3,1)=1  
          
      Tran = Translation(Array, Xtranslation, Ytranslation)
      
      WRITE(6,*)'Your number',Itwo,' translated array is:'
      WRITE(6,*) Tran(1,1), Tran(2,1)
      WRITE(20,*) Array(1,1), Array(2,1), Tran(1,1), Tran(2,1)

      IF (Itwo==1) THEN
        Arrayone=Array; Arraytwo=Tran
      ELSE IF (Itwo==N) THEN
        WRITE(20,*) Arrayone(1,1), Arrayone(2,1), Arraytwo(1,1), Arraytwo(2,1)
      END IF
       
    END DO 

    CLOSE(20)
    

  ELSE IF (Main=='c' .or. Main=='C') THEN
    
    Arrayone(3,1)=1; Arraytwo(3,1)=1; Arraythree(3,1)=1  
    
    DO
      
      WRITE(6,*) 'Please enter the three x,y coordinates of the vertices of your triangle.'
      READ(5,*) Arrayone(1,1), Arrayone(2,1), Arraytwo(1,1), Arraytwo(2,1), Arraythree(1,1)
      READ(5,*) Arraythree(2,1)
      WRITE(6,*) 'Please enter the angle you want to rotate the triangle by in radians.'
      READ(5,*) Inputangle       
      WRITE(6,*) 'You entered', Arrayone(1,1), Arrayone(2,1),'as one vertice', Arraytwo(1,1)
      WRITE(6,*) Arraytwo(2,1), 'as another and', Arraythree(1,1), Arraythree(2,1), 'as the third.'
      WRITE(6,*) 'You entered the angle of rotation to be', Inputangle     
      WRITE(6,*) 'Is that correct? Type Y for yes, or N for no.' 
      
      DO      
        READ(5,*) Correction
        IF (Correction=='n' .or. Correction=='y' .or. Correction=='N' .or. Correction=='Y') THEN
          EXIT
        END IF       
        WRITE(6,*) 'Please enter a valid choice.'
      END DO
      
      IF (Correction=='Y' .or. Correction=='y')THEN
        EXIT 
      END IF

    END DO
     
        
    Xtranslation=(Arrayone(1,1)+Arraytwo(1,1)+Arraythree(1,1))/3
    Ytranslation=(Arrayone(2,1)+Arraytwo(2,1)+Arraythree(2,1))/3

    WRITE(6,*) 'Centre of mass of the triangle is', Xtranslation, Ytranslation
    
    Xtranslation=-Xtranslation; Ytranslation=-Ytranslation
          
    Array = Arrayone; Arrayone = Translation(Array, Xtranslation, Ytranslation)
    Array = Arraytwo; Arraytwo = Translation(Array, Xtranslation, Ytranslation)
    Array = Arraythree; Arraythree = Translation(Array, Xtranslation, Ytranslation)
    
    Array = Arrayone; Arrayone = Rotation(Array,Inputangle)
    Array = Arraytwo; Arraytwo = Rotation(Array,Inputangle)
    Array = Arraythree; Arraythree = Rotation(Array,Inputangle)
    
    Xtranslation=-Xtranslation; Ytranslation=-Ytranslation

    Array = Arrayone; Arrayone = Translation(Array, Xtranslation, Ytranslation)
    Array = Arraytwo; Arraytwo = Translation(Array, Xtranslation, Ytranslation)
    Array = Arraythree; Arraythree = Translation(Array, Xtranslation, Ytranslation)

    WRITE(6,*) 'The three vertices of your rotated triangle are:'
    WRITE(6,*) Arrayone(1,1), Arrayone(2,1)
    WRITE(6,*) Arraytwo(1,1), Arraytwo(2,1)
    WRITE(6,*) Arraythree(1,1), Arraythree(2,1)

  END IF

  WRITE(6,*) 'Would you like to run the program again?'
  WRITE(6,*) 'Type Y for Yes, or N for No.'
  DO      
    READ(5,*) Mainloop
    IF (Mainloop=='n' .or. Mainloop=='y' .or. Mainloop=='N' .or. Mainloop=='Y') THEN
      EXIT
    END IF       
    WRITE(6,*) 'Please enter a valid choice; Y for Yes, or N for No.'
  END DO
   
END DO 

WRITE(6,*) 'Thank you for using the program.'

CONTAINS

FUNCTION Rotation(Array,Inputangle) 
  REAL, DIMENSION(1:3,1:1):: Rotation, Array
  REAL:: Inputangle, sinput, cinput
  REAL, DIMENSION(1:3,1:3):: RotArray

  Cinput = cos(inputangle)
  Sinput = sin(inputangle)
  RotArray(1,1)=Cinput; RotArray(1,2)=-Sinput; RotArray(1,3)=0
  RotArray(2,1)=Sinput; RotArray(2,2)=Cinput;  RotArray(2,3)=0
  RotArray(3,1)=0;      RotArray(3,2)=0;       RotArray(3,3)=1

  Rotation = Matmul(Rotarray,Array)

END FUNCTION 

FUNCTION Translation(Array, Xtranslation, Ytranslation)
  REAL, DIMENSION(1:3,1:1):: Translation, Array
  REAL::  Xtranslation, Ytranslation
  REAL, DIMENSION(1:3,1:3):: TranArray
    
  TranArray(1,1)=1; TranArray(1,2)=0; TranArray(1,3)=Xtranslation
  TranArray(2,1)=0; TranArray(2,2)=1; TranArray(2,3)=Ytranslation
  TranArray(3,1)=0; TranArray(3,2)=0; TranArray(3,3)=1
  
  Translation = Matmul(TranArray,Array)
  
END FUNCTION
     
END PROGRAM        
      
!X FIRST THEN Y
!Do loop, Explain program, Give options--if loop,look up feedback,  end if, give option to run again