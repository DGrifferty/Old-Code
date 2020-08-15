Program cardsforo
integer :: i, t, q
Character(Len=3), dimension(1:4) ::  C, S, H, D, o, l, k, g
Character(Len=3), Dimension(1:4) :: j
real :: p

C = (/'TwC', 'ThC', 'FoC', 'FiC'/)
S = (/'TwS', 'ThS', 'FoS', 'FiS'/)
H = (/'TwH', 'ThH', 'FoH', 'FiH'/)
D = (/'TwD', 'ThD', 'FoD', 'FiD'/)
o = (/'TwC', 'ThC', 'FoC', 'FiC'/)
l = (/'TwS', 'ThS', 'FoS', 'FiS'/)
k = (/'TwH', 'ThH', 'FoH', 'FiH'/)
g = (/'TwD', 'ThD', 'FoD', 'FiD'/)

i=0
t=100
  !finding suit
  do q=1,4
      i=0
do while (i > 4 .or. i == 0)

Call Random_number(p)
i = P*10
  end do
  write(6,*) i
  !using random number to assign number to sui
  
  t=100
if (i==4) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
  J(1) = d(t)
o(q) = j(1)


 else if (i==3)then
    do while (t>4 .or. t == 0)
      Call Random_number(p)
      t = P*10
  end do
 
 o(q) = h(t)



t=100
else if (i==2) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
  o(q) = s(t)


 
t=100
else if (i==1) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
  j(1) = C(t)
o(q) = j(1)
Write(6,*) 'got through it'

  end if
end do


Write(6,*) 'got through it'
Write (6,*) o


endprogram cardsforo