program freedomofthearrays

REAL, DIMENSION(:), ALLOCATABLE :: a, b
Real, Dimension(1:3) :: c, s, l
integer :: i, q, p, o, d
Character :: figs, fig, n, figy, y, figo

interface
subroutine crossproduct
Real, Dimension(1:3) :: c, s, l
integer :: i
end subroutine crossproduct
end interface

figy = 'y'
Write(6,*) 'This program is desined to calculate the dot product of two arrays of your choice'
write(6,*) ' and compare if the numbers are larger or smaller than a number of your choice'
do while (figy=='y' .or. figy=='Y')
  Write(6,*) 'Would you like to calculate the dot product or cross product of two arrays?'
  Write(6,*) 'Type C for the cross product or type D for the dot product.'
  Read(6,*) figo
  If (figo == 'C' .or. figo == 'c') then
    
    Call crossproduct
    Write(6,*) 'Would you like to us the program again? Type y for yes, or n for no'
    Read(5,*) figy
    cycle
else if (figo == 'D' .or. figo == 'd') then   
  figs = 'n'
fig = 'n'
write(6,*) 'Please enter size of your first array.'
READ(5,*) i
write(6,*) 'Please enter size of your secound array.'
READ(5,*) o
allocate(b(o))
  ALLOCATE(a(i))
p = o
q = i


 

do while (fig=='n' .or. fig=='N')
do i=1,q
Write(6,*) 'Please enter number ',i,'of your first array'
Read(5,*) a(i)
end do
Write(6,*) 'Your array is',a, 'is this okay? type y for yes, and n for no'
read(5,*) fig
end do
Write(6,*) 'Please enter the number you would like to test your array against.'
Read(5,*) d
Write(6,*) a<d


do while (figs=='n'.or.figs=='N')
do o=1,p
Write(6,*) 'Please enter number ',o,'of your secound array'
Read(5,*) b(o)
end do
Write(6,*) 'Your array is',b, 'is this okay? type y for yes, and n for no'
read(5,*) figs
end do
Write(6,*) 'Please enter the number you would like to test your array against.'
Read(5,*) d
Write(6,*) a<d
Write(6,*) 'Dot product of your entered arrays is', dot_product(a,b)
Write(6,*) 'Would you like to us the program again? Type y for yes, or n for no'
Read(5,*) figy
end if
end do

endprogram freedomofthearrays

subroutine crossproduct
Real, Dimension(1:3) :: c, s, l
integer :: i

Write(6,*) 'Please type in your first array'
    do i=1,3
    Read(5,*) c(i)
    end do
    Write(6,*) 'Please type in your secound array'
    do i=1,3
    Read(5,*) s(i)
    end do
l(1) = (c(2)*s(3)-c(3)*s(2))
l(2) = (c(3)*s(1)-c(1)*s(3))
l(3) = (c(1)*s(2)-c(2)*s(1))
Write(6,*) l
end subroutine crossproduct
    
    