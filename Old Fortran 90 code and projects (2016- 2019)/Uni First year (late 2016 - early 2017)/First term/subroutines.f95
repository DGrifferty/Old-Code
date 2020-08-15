program subroutines

implicit none

Real :: a, b, c, e, f, p

integer :: i, q, q_new
integer, dimension(1:1) :: y, x
character :: fig

Real, Dimension(1:5) :: d, t

Interface 
subroutine sorting (a, b, c, e, f)
Real, intent(in) :: a, b, c, e, f
Real, Dimension(1:5) :: d
Real :: sorted, p, q, q_new
integer, dimension(1:1) :: y, x
integer :: i
end subroutine sorting
end interface

write (6,*) 'This program sorts arrays into order'

write (6,*) 'Please enter array'
read (5,*) a, b, c, e, f
t = (/a, b, c, e, f/)
Write (6,*) 'The array you entered is', t
Write (6,*) 'Is this correct? Type Y for yes, or N for no'
Read (5,*) fig
do
if (fig == 'y' .or. fig == 'Y') exit
else if (fig == 'n' .or. fig == 'N') then
write (6,*) 'Please enter array'
  read (5,*) a, b, c, e, f
  t = (/a, b, c, e, f/)
  Write (6,*) 'The array you entered is', t
 exit
  end if
end do
call sorting (a, b, c, e, f)

end program subroutines

subroutine sorting (a, b, c, e, f)
Real, intent(in) :: a, b, c, e, f
Integer :: i, q, q_new
integer, dimension(1:1) :: y, x
Real :: sorted, p
Real, DIMENSION(1:5) :: d
d = (/a, b, c, e, f/)
write (6, *) d
p = sum(d)
Write (6,*) 'Total sum of array =', p

q_new = 5

do i=1,5
  
q = q_new 


y = Maxloc (d,i) 
j = Maxval (d,i)
x = d(i) 
d(y) = x
d(q) = j
x = y


q_new = q - 1  

end do

Write (6,*) 'Your sorted array is', d

End subroutine sorting