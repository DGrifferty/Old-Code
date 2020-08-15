!--------------------------------------------------
!PHY1038 Assignment 3 Finite Difference Methods
!URN: 6408056
!March 2017
!--------------------------------------------------

PROGRAM Matrix_Geometry

IMPLICIT NONE


REAL, DIMENSION(2)::Array
REAL:: Inputangle, Xtranslation, Ytranslation !inputangle is the angle they want to roate it by, Array angle is the Arrays angle in polar
CHARACTER::Mainloop, Intro, Main, Correction

Mainloop='Y'
Intro='Y'

DO WHILE (Mainloop=='Y' .OR. Mainloop=='y')
  
  IF(Intro=='Y') THEN 
    WRITE(6,*) 'INTRO HERE'
    Intro='N'
  END IF

  WRITE(6,*)'If you would like to rotate a matrix by a paticular angle type, R, for a translation'
  WRITE(6,*)'type T, or to rotate a triangle about its centre of mass type, C.'

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
      
      WRITE(6,*) 'Please enter the Array that you want to rotate, x then y.'
      READ(5,*) Array
      WRITE(6,*) 'Please enter the angle you want to rotate this by in radians.'
      READ(5,*) Inputangle     
      WRITE(6,*) 'You entered', Array, 'as the Array, and', Inputangle,'As the angle you want'
      WRITE(6,*) 'to rotate it by. Is that correct? Type Y for yes, or N for no.' 
      
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
    
    WRITE(6,*) Rotation(Array,Inputangle)
    
    
  ELSE IF (Main=='t' .or. Main=='T') THEN
       
    DO
      
      WRITE(6,*) 'Please enter the Array that you want to rotate, x then y.'
      READ(5,*) Array
      WRITE(6,*) 'Please enter the amout you want to translate x by.'
      READ(5,*) Xtranslation
      WRITE(6,*) 'Please enter the amout you want to translate y by.'
      READ(5,*) Ytranslation
      WRITE(6,*) 'You entered', Array, 'as the Array, and', Xtranslation, Ytranslation, 
      WRITE(6,*) 'as the amount to translate x, then y by.'
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
      WRITE(6,*) Translation(Array, A, B)
      
  END IF
  
END DO 

CONTAINS

FUNCTION Rotation(Array,Inputangle) 
  REAL, DIMENSION(2):: Rotation, Array
  REAL:: Arrayangle, R, Inputangle, Finalangle
  R=((Array(1))**2+(Array(2))**2)**0.5
  Arrayangle=Atan(Array(2)/Array(1))
  Finalangle=Inputangle+Arrayangle
  Rotation(1)=R*cos(Finalangle)
  Rotation(2)=R*sin(Finalangle)
END FUNCTION 

FUNCTION Translation(Array, Xtranslation, Ytranslation)
  REAL, DIMENSION(2):: Translation, Array
  REAL:: Arrayangle, Xtranslation, Ytranslation
  Translation(1)=Array(1)+Xtranslation
  Translation(2)=Array(2)+Ytranslation
END FUNCTION
     
END PROGRAM        
      


!DO NOT GO OVER 100 CHARACTERS
!X FIRST THEN Y
!Do loop, Explain program, Give options--if loop,look up feedback,  end if, give option to run again