Program geometricmean
implicit none

Real, Dimension(:), allocatable :: A
integer:: i
Real :: p, x, n
Character:: fig

fig='y'
Write(6,*) 'This program lets you calculate the geometric mean of a set of numbers of your choice.'

Do while (fig=='y' .or. fig=='Y')
  Write(6,*) 'Please type in how many numbers there are in your set.'
  Read(5,*) N
  allocate(A(N))
  Write(6,*) 'Please type the numbers in your set.'
  Read(5,*) A
  x = 0
  do i=1,N
    x = x + A(i)
    end do
 p= x**(1/N)
    Write(6,*)'The geometric mean of your set of numbers is', p
Write(6,*) 'Would you like to run the program again? Type Y for yes, or N for no.'
Read(5,*) fig
End do

end program geometricmean