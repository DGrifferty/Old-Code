Program dealingcards
implicit none 

Character :: fig
Character(Len=10) :: name1, name3, name4, name2
Character(Len=3) :: TwC, ThC, FoC, FiC
Character(len=3) :: TwS, ThS, FoS, FiS
Character(len=3) :: TwH, ThH, FoH, FiH
Character(len=3) :: TwD, ThD, FoD, FiD
Character(len=3), Dimension(1:4) :: C, S, H, D 
Character(len=3), Dimension(1:4) :: o, l, k, g
Character(Len=3), Dimension(1) :: j, n, m , b
INTEGER :: i, t, q
Real :: p
INTERFACE 
Subroutine Randomnumber
integer :: i, t, q
Character(Len=3), dimension(1:4) ::  C, S, H, D, o, l, k, g
Character(Len=3), Dimension(1:4) :: j
real :: p
END Subroutine Randomnumber
END INTERFACE

C = (/'TwC', 'ThC', 'FoC', 'FiC'/)
S = (/'TwS', 'ThS', 'FoS', 'FiS'/)
H = (/'TwH', 'ThH', 'FoH', 'FiH'/)
D = (/'TwD', 'ThD', 'FoD', 'FiD'/)


Write (6,*) C
Write (6,*) 'This programme deals four random sets of cards.'
Write (6,*) 'The programme deals them to four players called north, south, east, and west'
Write (6,*) 'If you would like to ask the programme to deal to four players of your choice of name, please type Y, if the'
Write (6,*) 'preset names are okay please type N'
Read (5,*) fig

if (fig == 'N' .or. fig == 'n') then
  name1 = 'North'
  name2 = 'East'
  name3 = 'South'
  name4 = 'West'
  end if
do while (fig == 'Y' .or. fig == 'y')
  if (fig== 'y' .or. fig == 'Y') then
  Write (6,*) 'Please enter 2 five character names, and two four character names..'
  Read (5,*) name1, name2, name3, name4
  Write (6,*) 'The names you entered are ',name1,', ',name2,', ',name3,', ',name4
  Write (6,*) 'Would you like to try again, type Y for yes, and N for no' 
  Read (5,*) fig
  if (fig == 'N' .or. fig == 'n') exit
    end if
    end do
    Write(6,*) 'Dealing cards'
    Call randomnumber
    Write (6,*) 'Cards for', name1, 'are', o
 

  End program dealingcards
  

Subroutine randomnumber
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
do while (i > 4 .or. i == 0)
Call Random_number(p)
i = P*10
  end do
  !using random number to assign number to suit 
if (i==4) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
  
  J = d(t)
o(q) = j(1)


 else if (i==3)then
   Call Random_number(p)
    do while (t>4 .or. t == 0)
      
t = P*10
  end do
  j = h(t)
o(q) = j(1)

 t=100
else if (i==2) then
  do while (t>4 .or. t == 0)

Call Random_number(p)
t = P*10
  end do
  j = s(t)
  o(q) = j(1)

 t=100
else if (i==1) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
  j = C(t)
o(q) = j(1)


  end if
end do

Endsubroutine randomnumber

