Program areaofshapes

implicit none

Real :: area, a, b, c, d, q
Real, external :: sq
Character :: fig, sure
Write(6,*) 'this program calculates the area of a rectangle, or triangle'
Write(6,*) 'type s to calculate the area of a square, and t for area of triangle'
read (5,*) fig
if(fig == 't' .or. fig == 'T') Then
  Write(6,*) 'please type in two lenghts of square side'
    read(5,*) a, b 
    d = a*b
    write(6,*) d
    else if(fig == 's' .or. fig == 'S') Then
      Write (6,*) 'please type in lengths of triangle'
      read (5,*) a, b, c
       Write (6,*) 'area of triangle is', tri (a,b,c)
      else
        Write (6,*) ' Invalid option, programme permanetley termanated, your computer will shut down now.'
        end if

End program areaofshapes
 


Real, function tri(a, b, c) 
implicit none 
Real, intent(in) :: a, b, c
Real :: tri
q = 0.5*(a + b + c)
      tri = (q * (q - a) * (q - b) * (q - c))**0.5
      Write (6,*) 'sides are' a,b,c
end function tri






 Write (6,*) :: 'are you sure you want to calculate area of square? type yes or no'
  read (5,*) :: sure
  if(sure == 'yes') Then
    