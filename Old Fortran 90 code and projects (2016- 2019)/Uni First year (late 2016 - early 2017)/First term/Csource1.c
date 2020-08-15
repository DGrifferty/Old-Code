program subroutines

implicit none

Real :: a, b, c, e, f, p

integer :: i, q, q_new, k, u
integer, dimension(1:1) :: y, x
character :: fig

Real, Dimension(1:5) :: d, t

Interface 
subroutine sorting (a, b, c, e, f)
Real, intent(in) :: a, b, c, e, f
Real, Dimension(1:5) :: d
Real :: sorted, p, q, q_new
integer, dimension(1:1) :: y, x
integer :: i, k, u
end subroutine sorting
end interface

write (6,*) 'This program sorts arrays into order'

write (6,*) 'Please enter array'
read (5,*) a, b, c, e, f
t = (/a, b, c, e, f/)
Write (6,*) 'The array you entered is', t
Write (6,*) 'Is this correct? Type Y for yes, or N for no'
Read (5,*) fig
do while (fig == 'n' .or. fig == 'N')
if (fig == 'n' .or. fig == 'N') then
write (6,*) 'Please try entering array again'
  read (5,*) a, b, c, e, f
  t = (/a, b, c, e, f/)
  Write (6,*) 'The array you entered is', t
  Write (6,*) 'Is this correct? Type Y for yes, or N for no'
Read (5,*) fig
  if (fig == 'y' .or. fig == 'Y') exit

  end if
end do
call sorting (a, b, c, e, f)

end program subroutines

subroutine sorting (a, b, c, e, f)
Real, intent(in) :: a, b, c, e, f
Integer :: i, q, q_new, k, u
integer, dimension(1:1) :: y, x
Real :: sorted, p
Real, DIMENSION(1:5) :: d
d = (/a, b, c, e, f/)
write (6, *) d
p = sum(d)
Write (6,*) 'Total sum of array =', p
q = 4
do i=1,4
  q = q_new
y = Maxloc (d,q)
k = d(q)
u = maxval (d,q)
d(q) = u
d(y) = k
q_new = q - 1

write (6,*) d
end do
Write (6,*) 'Your sorted array is', d
End subroutine sorting
