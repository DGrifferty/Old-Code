!--------------------------------------------------
!PHY1038 Assignment 4 Matrix Geometry
!URN: 6408056
!MAY 2017
!--------------------------------------------------

PROGRAM Matrix_Geometry

IMPLICIT NONE

REAL, DIMENSION(1:3,1:1)::Array, Rot, Tran, Arrayone, Arraytwo, Arraythree, OArrayone, OArraytwo
REAL, DIMENSION(1:3,1:1)::OArraythree
CHARACTER::Mainloop, Intro, Main, Correction
INTEGER:: I, N, Itwo
REAL:: Inputangle, Xtranslation, Ytranslation 

Mainloop='Y'!Allows main do loop to play intially
Intro='Y'!Allows intro to play on first run

DO WHILE (Mainloop=='Y' .OR. Mainloop=='y')
  
  IF(Intro=='Y') THEN 
    WRITE(6,*) 'This program allows you to enter x and y coordinates and have them translated'
    WRITE(6,*) 'or rotated, it also allows you to rotate a triangle about its centre of mass.'
    WRITE(6,*) 'The program lets you enter your own rotation angle/translations.'
    WRITE(6,*) 'It then prints the translated/rotated coordinates to the screen as well as'
    WRITE(6,*) 'a file, named translate.dat, rotate.dat, or triangle.dat'
    WRITE(6,*) 'It writes the orginal coordinates in lines one and two, and the altered'
    WRITE(6,*) 'coordinates in lines three and four'
    WRITE(6,*) 'When using program please enter your coordiantes x first e.g.: x, y, x, y'
    Intro='N' !Prevents the intro being played too many times
  END IF

  WRITE(6,*)'If you would like to rotate a vector by a paticular angle type, R, for a translation'
  WRITE(6,*)'type T, to rotate a triangle about its centre of mass type, C'

  DO !replays if an invalid choice is entered
    READ(5,*) Main

    IF(Main=='r' .or. Main=='R' .or. Main=='t' .or. Main=='T' .or. Main=='c' .or. Main== 'C')THEN
      EXIT !If loop allows the exit from do loop only when a valid choice is entered
    END IF 

    WRITE(6,*) 'Please enter a valid option'
    WRITE(6,*) 'Enter, R, for rotation, T, for translation, or C to rotate a triangle about its'
    WRITE(6,*) 'centre of mass.'
    
  END DO

    
  IF (Main=='r' .or. Main=='R') THEN !Main if command seperates the main components of the program
    
    DO !Do loop will replay if user says they entered the 'wrong coordinates' 
                                                     
      WRITE(6,*) 'How many points would you like to rotate?'
      READ(5,*) N
      WRITE(6,*) 'By what angle, in radians, would you like to rotate them by?'
      READ(5,*) Inputangle   
      WRITE(6,*) 'You entered',N,' as the amount of points you want to rotate, and'
      WRITE(6,*) Inputangle, 'As the amount you want to rotate them by, is this correct?'      
      DO !Do loop will replay if user enters an 'invalid choice'
        WRITE(6,*) 'Type Y for yes, or N for no.'     
        READ(5,*) Correction
        
        IF (Correction=='n' .or. Correction=='y' .or. Correction=='N' .or. Correction=='Y') THEN
          EXIT !Exits 'invalid choice' do loop when they enter valid choice
        END IF       
        WRITE(6,*) 'Please enter a valid choice.'
      END DO
      
      IF (Correction=='Y' .or. Correction=='y')THEN
        EXIT !Exits 'wrong coordinates' do loop when they say they entered the right coordinates
      END IF

    END DO
    
    OPEN(unit=20,file='rotate.dat')
    
    DO I=1,N !Plays once for every coordinate that they wanted to enter
    
      
      WRITE(6,*) 'Please enter Array number',i,' that you want to rotate, x then y.'
      READ(5,*) Array(1,1), Array(2,1)
      
      Array(3,1)=1  !Setting z coordinate to 1 for rotation matrix to work
          
      Rot = Rotation(Array,Inputangle) !Calls on rotation function, and stores it as a different
                                       !Array allowing program call on just the x and y coordinates      
      WRITE(6,*)'Your number',I,' rotated array is:'
      WRITE(6,*) Rot(1,1), Rot(2,1)
      WRITE(20,*) Array(1,1), Array(2,1), Rot(1,1), Rot(2,1) !Prints orginal, and altered 
                                                             !coordinates to file
      
      IF (I==1) THEN
        Arrayone=Array; Arraytwo=Rot !This stores the first set of coordinates to later print
      ELSE IF (I==N) THEN
        WRITE(20,*) Arrayone(1,1), Arrayone(2,1), Arraytwo(1,1), Arraytwo(2,1)
      END IF !Prints coordinates to file, to allow for conjoined shape when plotting
        
    END DO 
    
    CLOSE(20)

  ELSE IF (Main=='t' .or. Main=='T') THEN
       
    DO !Do loop will replay if user says they entered the 'wrong coordinates'

      WRITE(6,*) 'How many points would you like to translate?'
      READ(5,*) N
      WRITE(6,*) 'Please enter the amount you want to translate x by.'
      READ(5,*) Xtranslation
      WRITE(6,*) 'Please enter the amount you want to translate y by.'
      READ(5,*) Ytranslation
      WRITE(6,*) 'You entered',N,' as the amount of points you want to translate, and', Xtranslation
      WRITE(6,*) Ytranslation, 'as the amount to translate x, then y by.'
      
      DO !Do loop will replay if user enters an 'invalid choice'
        WRITE(6,*) 'Type Y for yes, or N for no.'  
        READ(5,*) Correction
        IF (Correction=='n' .or. Correction=='y' .or. Correction=='N' .or. Correction=='Y') THEN
          EXIT!Exits 'invalid choice' do loop when they enter valid choice
        END IF       
        WRITE(6,*) 'Please enter a valid choice.'
      END DO
      
      IF (Correction=='Y' .or. Correction=='y')THEN
        EXIT !Exits 'wrong coordinates' do loop when they say they entered the right coordinates
      END IF
           
    END DO
    
    OPEN(unit=20,file='translate.dat')     

    DO Itwo=1,N !Plays once for each coordinate
           
      WRITE(6,*) 'Please enter Array number',Itwo,' that you want to translate, x then y.'
      READ(5,*) Array(1,1), Array(2,1)
      
      Array(3,1)=1  !Setting Z coordinate to one so translation equation will work
          
      Tran = Translation(Array, Xtranslation, Ytranslation)!calls on translation function and
      !stores altered coordinates in a different array, allowing program to call on it x and y only
      
      WRITE(6,*)'Your number',Itwo,' translated array is:'
      WRITE(6,*) Tran(1,1), Tran(2,1) !prints altered coordinates to screen
      WRITE(20,*) Array(1,1), Array(2,1), Tran(1,1), Tran(2,1)!writes both orginal and altered 
                                                              !altered coordinates to file
      IF (Itwo==1) THEN
        Arrayone=Array; Arraytwo=Tran !stores first set of coordinates as seperate arrays
      ELSE IF (Itwo==N) THEN
        WRITE(20,*) Arrayone(1,1), Arrayone(2,1), Arraytwo(1,1), Arraytwo(2,1)
      END IF !Prints first set of coordinates to file again, for conjoined shape
       
    END DO 

    CLOSE(20)
    

  ELSE IF (Main=='c' .or. Main=='C') THEN
    
    Arrayone(3,1)=1; Arraytwo(3,1)=1; Arraythree(3,1)=1 !Setting all three z coordinates to 1
                                                        !So equations in functions work   
    DO!Do loop will replay if user says they entered the 'wrong coordinates'
      
      WRITE(6,*) 'Please enter the three x,y coordinates of the vertices of your triangle.'
      READ(5,*) Arrayone(1,1), Arrayone(2,1), Arraytwo(1,1), Arraytwo(2,1), Arraythree(1,1)
      READ(5,*) Arraythree(2,1)
      WRITE(6,*) 'Please enter the angle you want to rotate the triangle by in radians.'
      READ(5,*) Inputangle       
      WRITE(6,*) 'You entered', Arrayone(1,1), Arrayone(2,1),'as one vertice', Arraytwo(1,1)
      WRITE(6,*) Arraytwo(2,1), 'as another and', Arraythree(1,1), Arraythree(2,1), 'as the third.'
      WRITE(6,*) 'You entered the angle of rotation to be', Inputangle     
      WRITE(6,*) 'Is that correct? Type Y for yes, or N for no.' 
      
      DO !Do loop will replay if user enters an 'invalid choice' 
        WRITE(6,*) 'Type Y for yes, or N for no.'     
        READ(5,*) Correction
        IF (Correction=='n' .or. Correction=='y' .or. Correction=='N' .or. Correction=='Y') THEN
          EXIT !Exits 'invalid choice' do loop when they enter valid choice
        END IF       
        WRITE(6,*) 'Please enter a valid choice.'
      END DO
      
      IF (Correction=='Y' .or. Correction=='y')THEN
        EXIT !Exits 'wrong coordinates' do loop when they say they entered the right coordinates
      END IF

    END DO
     
    OArrayone=Arrayone; OArraytwo=Arraytwo; OArraythree=Arraythree
    !Stores orginal arrays with 'o' prefix, so they can later be written to a file
    OPEN(unit=20,file='triangle.dat')
    
    Xtranslation=(Arrayone(1,1)+Arraytwo(1,1)+Arraythree(1,1))/3
    Ytranslation=(Arrayone(2,1)+Arraytwo(2,1)+Arraythree(2,1))/3
    !^Calculates COM

    WRITE(6,*) 'Centre of mass of the triangle is', Xtranslation, Ytranslation
    
    Xtranslation=-Xtranslation; Ytranslation=-Ytranslation
    !^Turns COM in negative translations, so program can set it to the origin
          
    Array = Arrayone; Arrayone = Translation(Array, Xtranslation, Ytranslation)
    Array = Arraytwo; Arraytwo = Translation(Array, Xtranslation, Ytranslation)
    Array = Arraythree; Arraythree = Translation(Array, Xtranslation, Ytranslation)
    !^Translates all coorinates so that COM is at origin
    
    Array = Arrayone; Arrayone = Rotation(Array,Inputangle)
    Array = Arraytwo; Arraytwo = Rotation(Array,Inputangle)
    Array = Arraythree; Arraythree = Rotation(Array,Inputangle)
    !^Rotates coordinates, so that triangle is rotates
    
    Xtranslation=-Xtranslation; Ytranslation=-Ytranslation
    !^Again reverses coordinates so that COM can be returned to original postion

    Array = Arrayone; Arrayone = Translation(Array, Xtranslation, Ytranslation)
    Array = Arraytwo; Arraytwo = Translation(Array, Xtranslation, Ytranslation)
    Array = Arraythree; Arraythree = Translation(Array, Xtranslation, Ytranslation)
    !^Places COM in original postions

    WRITE(6,*) 'The three vertices of your rotated triangle, (x,y), are:'
    
    WRITE(6,*) Arrayone(1,1), Arrayone(2,1) 
    WRITE(20,*) OArrayone(1,1), OArrayone(2,1), Arrayone(1,1), Arrayone(2,1)
    
    WRITE(6,*) Arraytwo(1,1), Arraytwo(2,1) 
    WRITE(20,*) OArraytwo(1,1), OArraytwo(2,1), Arraytwo(1,1), Arraytwo(2,1)
    
    WRITE(6,*) Arraythree(1,1), Arraythree(2,1)
    WRITE(20,*) OArraythree(1,1), OArraythree(2,1), Arraythree(1,1), Arraythree(2,1)
    !^Prints all three rotated coordiantes to screen, as well as printing orginal and altered
    !to file

    WRITE(20,*) OArrayone(1,1), OArrayone(2,1), Arrayone(1,1), Arrayone(2,1)
    !^Prints 1st coordinates again to produce conjoined shape

    CLOSE(20)

  END IF

  WRITE(6,*) 'Would you like to run the program again?'!Allows user to choose between main parts of
                                                       !program again
  WRITE(6,*) 'Type Y for Yes, or N for No.'
  DO !Plays loop again if valid choice is not entered     
    READ(5,*) Mainloop
    IF (Mainloop=='n' .or. Mainloop=='y' .or. Mainloop=='N' .or. Mainloop=='Y') THEN
      EXIT!Exits loop when valid choice is chosen
    END IF       
    WRITE(6,*) 'Please enter a valid choice; Y for Yes, or N for No.'
  END DO
   
END DO !Main do loop, will play if Y is selected

WRITE(6,*) 'Thank you for using the program.'!Will play on exit

CONTAINS

FUNCTION Rotation(Array,Inputangle) 
  REAL, DIMENSION(1:3,1:1):: Rotation, Array
  REAL, DIMENSION(1:3,1:3):: RotArray
  REAL:: Inputangle, Sinput, Cinput
  
  Cinput = cos(Inputangle)!storing cos and sine of input angle for rotation matrix
  Sinput = sin(Inputangle)
  RotArray(1,1)=Cinput; RotArray(1,2)=-Sinput; RotArray(1,3)=0
  RotArray(2,1)=Sinput; RotArray(2,2)=Cinput;  RotArray(2,3)=0
  RotArray(3,1)=0;      RotArray(3,2)=0;       RotArray(3,3)=1 !Records rotation matrix

  Rotation = Matmul(Rotarray,Array) !Multiplies the rotation matrix with the users array

END FUNCTION 

FUNCTION Translation(Array, Xtranslation, Ytranslation)
  REAL, DIMENSION(1:3,1:1):: Translation, Array
  REAL, DIMENSION(1:3,1:3):: TranArray
  REAL::  Xtranslation, Ytranslation
    
  TranArray(1,1)=1; TranArray(1,2)=0; TranArray(1,3)=Xtranslation
  TranArray(2,1)=0; TranArray(2,2)=1; TranArray(2,3)=Ytranslation
  TranArray(3,1)=0; TranArray(3,2)=0; TranArray(3,3)=1            !Records translation array
  
  Translation = Matmul(TranArray,Array) !Multiples the translation matrix with users array
  
END FUNCTION
     
END PROGRAM        
