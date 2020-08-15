Program sort
implicit none

Real, dimension(1:6):: a
Real, Dimension(1:1) :: j, k
Integer :: i, q, p, z

Write(6,*) 'This program is designed to sort arrays in accending order'


a=(/2, 5, 6, 7, 3, 4/)
do i=7, 1, -1
  j=Maxloc(a(1:i+1))
  k=Maxval(a(1:i+1))
  a(j(1))=a(i+1)
a(i+1)=k(1)

  end do
  write(6,*) a
  endprogram sort