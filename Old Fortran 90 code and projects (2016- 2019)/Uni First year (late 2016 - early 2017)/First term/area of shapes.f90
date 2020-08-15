Program areaofshapes
!subroutine
implicit none

Real :: a, b, c, p, d, t, y
Real, external :: cir, tri, quad, pops
Character(len=4) :: fig

Interface
function tri(a, b, c) 
Real, intent(in) :: a, b, c
Real :: tri, q
End function tri
End interface

Interface 
function cir(a)
Real, intent(in) :: a
real :: cir
End function cir
End interface

interface 
function quad(p, b, a)
real, intent(in) :: p, b, a
Real :: quad
end function quad
end interface

interface 
function pops(t, b, a)
Real, intent(in) :: t, b, a
Real :: pops
End function pops
End interface

Interface
function volsp(a)
Real, intent(in) :: a
Real :: volsp
End function volsp
End interface

Interface
Function sufsp(a)
Real, intent(in) :: a
Real :: sufsp
end function sufsp
end interface

Interface 
function volcon (a, b)
Real, intent(in) :: a,b
Real :: volcon
End function volcon
End interface

Interface
function sufcon(a, y)
Real, intent(in) :: a, y
Real :: sufcon
End function sufcon
End interface

Write(6,*) 'This program calculates the area of rectangles, triangles and circles.'
Write (6,*) 'It can calculate the volume and surafce area of a sphere, and cone,'
Write (6,*) 'as well as give the roots of a secound power quadratic.'

Write(6,*) 'Type R to calculate the area of a rectangle, C for the area of a circle,'
Write(6,*) 'T for area of triangle, VS for the volume of a sphere, SS for the surface area of a sphere,'
Write(6,*) 'VC for the volume of a cone, SC for the suraface area of a cone,' 
write(6,*) 'or Q to calculate the roots and discriminant of a secound order quadratic.'
Write(6,*) 'Or type EXIT to exit the programme'
do 
read (5,*) fig
if(fig == 'r' .or. fig == 'R') Then
  
  Write(6,*) 'Please type in two lengths of the rectangle''s sides'
    read(5,*) a, b 
    Do
If (a*b>0) Exit
IF (a*b<=0) THEN
Write (6,*) 'Sides must have positive values, please try again.'
Read (5,*) a, b
If (a*b>0) Exit
End if
End do
    d = a*b
    write(6,*) d
    write(6,*) 'What else would you like to calculate?'
else if(fig == 't' .or. fig == 'T') Then
      Write (6,*) 'Please type in lengths of the triangle''s sides.'
      read (5,*) a, b, c
      do
      if (a*b*c>0) exit
        if (a*b*c<=0) then
write(6,*) 'Sides must have positive lenghts, please try again.'
          read (5,*) a, b, c
          if (a*b*c>0) exit
            end if
            end do

       Write (6,*) 'area of triangle is', tri (a,b,c)
       write(6,*) 'What else would you like to calculate?'
       
else if (fig == 'c' .or. fig == 'C') then
        Write (6,*) 'Please enter radius.'
        read (5,*) a
        do
          if (a>0) exit 
            if (a<=0) then 
              write (6,*) 'Radius must have positive length, please try again.'
              read (5,*) a
              if (a>0) exit
                end if
                end do 
                write (6,*) 'Area of the circle is', cir (a)
                write(6,*) 'What else would you like to calculate?'
                
Else if (fig == 'Vs' .or. fig == 'vs') then
  Write (6,*) 'Please enter radius of sphere'
  Read (5,*) a
  do
    If(a>0) exit
  if (a<=0) then
    Write (6,*) 'Radii must be positive, please enter a postive value for the radius of your sphere'
    Read (5,*) a
    if (a>0) exit
      end if
      end do
      Write (6,*) 'The volume of your sphere is', volsp(a)
      write(6,*) 'What else would you like to calculate?'

else if (fig == 'VC' .or. fig == 'vc') then
  Write (6,*) 'Please enter radius of cone, then the height of the cone.'
  Read (6,*) a, b
  do
  if (a*b>0) exit
    if (a*b<=0) then 
      Write (6,*) 'The radius and height of a cone must be positive, please try again'
      read (5,*) a,b
      if (a*b>0) exit
        end if
        end do
        write (6,*) 'The volume of the cone is', volcon(a,b)
        write(6,*) 'What else would you like to calculate?'

Else if (fig == 'SC' .or. fig == 'sc') then
  Write(6,*) 'Please enter the radius then the height of the cone.'
  Read (5,*) a, b
  do
  if (a*b>0) exit
    if (a*b<=0) then
      write (6,*) 'The radius and height of a cone must be positive, please try again'
      if (a*b>0) exit
        end if 
        end do
        y = (b*b) + (a*a)
        Write (6,*) 'The surface area of the cone is', sufcon(a,y)
        write(6,*) 'What else would you like to calculate?'

Else if (fig == 'ss' .or. fig == 'ss') Then
  Write (6,*) 'Please enter radius of sphere.'
  Read (5,*) a
  do
    If (a>0) exit
      If (a<=0) then 
        Write (6,*) 'Radii of spheres must be positive, please try again'
        Read (5,*) a
        If (a>0) exit
          end if
          end do
          Write (5,*) 'The surface area of the sphere is', sufsp(a)
          write(6,*) 'What else would you like to calculate?'
    

 else if (fig == 'q' .or. fig == 'Q') then
                  Write (6,*) 'Please enter the prefexes of your quadratic.'
                  read (5,*) a, b, c
                  p = b*b - 4.0*a*c
                  t = b*b - 4.0*a*c
                  if (p>=0) then
                   Write (6,*) 'The roots of your quadratic are', quad(p, b, a), 'and', pops(t, b, a)
                   Write (6,*) 'The value of the discriminant is', p
                   write(6,*) 'What else would you like to calculate?'
                       else if (p<0) then 
                          write (6,*) 'roots are complex' 
                          write (6,*) 'discrimnant''s value is', p
                          write(6,*) 'What else would you like to calculate?'
                           end if 

 else if (fig == 'exit' .or. fig == 'EXIT' )   then
   exit    
       else 
         Write (6,*) 'Invalid option, please try again from the following;'
Write(6,*) 'Type R to calculate the area of a rectangle, C for the area of a circle,'
Write(6,*) ' T for area of triangle, VS for the volume of a sphere, SS for the surface area of a sphere,'
Write(6,*) ' VC for the volume of a cone, SC for the suraface area of a cone,' 
write(6,*) ' or Q to calculate the roots and discriminant of a secound order quadratic.'
Write (6,*) 'Or type EXIT to exit the programme'
        end if

        end do

End program areaofshapes
 
function tri(a, b, c) 
implicit none 

Real, intent(in) :: a, b, c
Real :: q
Real :: tri

q = (a+b+c)/2
 
tri = sqrt(q*(q-a)*(q-b)*(q-c))
      
end function tri

function cir(a)
implicit none

real, intent(in) :: a
real :: cir

cir = 3.141592654*a**2

end function cir

function quad(p, b, a)

implicit none 

Real, intent (in) :: a, b
real :: p
Real :: quad
p = sqrt(p)
quad = (-b + p)/(2.0*a)

end function quad

function pops(t, b, a)

implicit none

real, intent (in) :: a, b
real :: t
Real :: pops
t = sqrt(t)
pops = (-b - t)/(2.0*a)

end function pops

function volsp(a)
implicit none
real, intent(in) :: a
real :: volsp
volsp = (4.0/3.0)*3.141592654*(a**3.0)
end function volsp

function volcon(a,b)
implicit none
Real, intent(in) :: a,b
Real :: volcon
volcon = 3.141592654*(a**2.0)*(b/3.0)
End function volcon

function sufcon (a,y)
implicit none
real, intent (in):: a
real :: sufcon, y
y = sqrt(y)
sufcon = (3.141592654*a)*(a+y)
end function sufcon

Function sufsp(a)
implicit none
Real, intent(in) :: a
Real :: sufsp
sufsp = 4.0*3.141592654*(a**2)
end function sufsp


    