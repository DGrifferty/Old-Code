Program functionfirst


REAL, DIMENSION(1:3,1:1)::Array, c
Real :: x, y, sinput, cinput
Real, Dimension(1:3,1:3):: RotArray
REAL:: Inputangle
Inputangle=10
x=3.0
y=2.0

array(1,1)=x; array(2,1)=y;array(3,1)=1
cinput = cos(inputangle)
sinput = sin(inputangle)
RotArray(1,1)=cinput; RotArray(1,2)=-sinput; RotArray(1,3)=0
RotArray(2,1)=sinput; RotArray(2,2)=cinput; RotArray(2,3)=0
RotArray(3,1)=0; RotArray(3,2)=0; RotArray(3,3)=1

write(6,*) Array
write(6,*)Rotarray
c=Matmul(Rotarray,array)
write(6,*) Transpose(c)
END PROGRAM

R=((Array(1))**2+(Array(2))**2)**0.5
  Arrayangle=Atan(Array(2)/Array(1))
  Finalangle=Inputangle+Arrayangle
  Rotation(1)=R*cos(Finalangle)
  Rotation(2)=R*sin(Finalangle)