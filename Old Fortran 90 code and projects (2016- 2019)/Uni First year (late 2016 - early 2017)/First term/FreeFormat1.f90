Program dealingcards
implicit none 

Character :: fig, figs
Character(Len=8) :: name1, name3, name4, name2
Character(Len=3) :: TwC, ThC, FoC, FiC
Character(len=3) :: TwS, ThS, FoS, FiS
Character(len=3) :: TwH, ThH, FoH, FiH
Character(len=3) :: TwD, ThD, FoD, FiD
Character(len=3), Dimension(1:4) :: C, S, H, D 
Character(len=3), Dimension(1:4) :: o, l, k, g
Character(Len=3), Dimension(1) :: j
INTEGER :: i, t, q, f, F_new
Real :: p


INTERFACE 
Subroutine cardsforo
integer :: i, t, q, F, F_new
Character(Len=3), dimension(1:4) ::  C, S, H, D, o
Character(Len=3), Dimension(1:4) :: j
real :: p
END Subroutine  cardsforo
END INTERFACE


INTERFACE 
Subroutine cardsforl
integer :: i, t, q, F, F_new
Character(Len=3), dimension(1:4) ::  C, S, H, D, o
Character(Len=3), Dimension(1:4) :: l
real :: p
END Subroutine  cardsforl
END INTERFACE

INTERFACE 
Subroutine cardsfork
integer :: i, t, q, F, F_new
Character(Len=3), dimension(1:4) ::  C, S, H, D, o
Character(Len=3), Dimension(1:4) :: l
real :: p
END Subroutine  cardsfork
END INTERFACE

INTERFACE 
Subroutine cardsforg
integer :: i, t, q, F, F_new
Character(Len=3), dimension(1:4) ::  C, S, H, D, o
Character(Len=3), Dimension(1:4) :: l
real :: p
END Subroutine  cardsforg
END INTERFACE

INTERFACE 
Subroutine scoreforo
Character(Len=3) :: TwC, ThC, FoC, FiC
Character(len=3) :: TwS, ThS, FoS, FiS
Character(len=3) :: TwH, ThH, FoH, FiH
Character(len=3) :: TwD, ThD, FoD, FiD
Character(len=3), Dimension(1:4) :: o
INTEGER :: i, t, q, f, F_new
Real :: p
END Subroutine  scoreforo
END INTERFACE


C = (/'TwC', 'ThC', 'FoC', 'FiC'/)
S = (/'TwS', 'ThS', 'FoS', 'FiS'/)
H = (/'TwH', 'ThH', 'FoH', 'FiH'/)
D = (/'TwD', 'ThD', 'FoD', 'FiD'/)


Write (6,*) C
Write (6,*) 'This programme deals four random sets of cards.'
Write (6,*) 'The programme deals them to four players called north, south, east, and west'
Write (6,*) 'Would you like to type in your own player names, type Y for yes and N for no.'
Read (5,*) fig

if (fig == 'N' .or. fig == 'n') then
  name1 = 'North'
  name2 = 'East'
  name3 = 'South'
  name4 = 'West'
  end if
do while (fig == 'Y' .or. fig == 'y')
  if (fig== 'y' .or. fig == 'Y') then
  Write (6,*) 'Please enter 4 names.'
  Read (5,*) name1, name2, name3, name4
  Write (6,*) 'The names you entered are ',name1,', ',name2,', ',name3,', ',name4
  Write (6,*) 'Would you like to try again, type Y for yes, and N for no' 
  Read (5,*) fig
  if (fig == 'N' .or. fig == 'n') exit
    end if
    end do
    figs = 'y'
    do while (figs == 'y' .or. figs == 'Y')
    Write(6,*) 'Dealing cards'
    Write(6,*) 'Cards for ', name1, ' are'
    Call cardsforo
    Write(6,*) 'Cards for ', name2, ' are'
    Call cardsforl
     Write(6,*) 'Cards for ', name3, ' are'
    Call cardsfork
     Write(6,*) 'Cards for ', name4, ' are'
    Call cardsforg

 

  


Write(6,*) 'Would you like to deal more cards, type Y for yes, or N for no.'
Read(5,*) figs
    end do
  
End program dealingcards
  

subroutine cardsforo
integer :: i, t, q, F_new , F
Character(Len=3), dimension(1:4) ::  C, S, H, D, o
Character(Len=3), Dimension(1:4) :: j
real :: p

C = (/'TwC', 'ThC', 'FoC', 'FiC'/)
S = (/'TwS', 'ThS', 'FoS', 'FiS'/)
H = (/'TwH', 'ThH', 'FoH', 'FiH'/)
D = (/'TwD', 'ThD', 'FoD', 'FiD'/)


i=0
t=100

  do q=1,4
    i=0
do while (i > 4 .or. i == 0)
Call Random_number(p)
i = P*10
  end do


 t=100
if (i==4) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
o(q) = d(t)
t=100
 
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

o(q) = c(t)

  end if
end do
f=0
    Write (6,*) o(1),' ,', o(2),' ,', o(3),' ,', o(4)
do q=1,4 
if(o(q) == 'TwC' .or. o(q) == 'TwS' .or. o(q) == 'TwH' .or. o(q) == 'TwD')then

F_new = 2+f
f=F_new
else if (o(q) == 'ThC' .or. o(q) == 'ThS' .or. o(q) == 'ThH' .or. o(q) == 'ThD')then
 
F_new = 3+f
f=F_new
else if (o(q) == 'FoC' .or. o(q) == 'FoS' .or. o(q) == 'FoH' .or. o(q) == 'FoD')then
  
F_new = 4+f
f=F_new
else if (o(q) == 'FiC' .or. o(q) == 'FiS' .or. o(q) == 'FiH' .or. o(q) == 'FiD')then
  
F_new = 5+f
f=F_new
end if

end do
Write (6,*) 'score is,', f

 

Endsubroutine cardsforo

subroutine cardsforl
integer :: i, t, q, F, F_new
Character(Len=3), dimension(1:4) ::  C, S, H, D, l
Character(Len=3), Dimension(1:4) :: j
real :: p
Character(len=10) :: name1

C = (/'TwC', 'ThC', 'FoC', 'FiC'/)
S = (/'TwS', 'ThS', 'FoS', 'FiS'/)
H = (/'TwH', 'ThH', 'FoH', 'FiH'/)
D = (/'TwD', 'ThD', 'FoD', 'FiD'/)



i=0
t=100

  do q=1,4
    i=0
do while (i > 4 .or. i == 0)
Call Random_number(p)
i = P*10
  end do


 t=100
if (i==4) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
l(q) = d(t)
t=100
 
else if (i==3)then
    do while (t>4 .or. t == 0)
      Call Random_number(p)
      t = P*10
  end do
 l(q) = h(t)



t=100
else if (i==2) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
  l(q) = s(t)


 
t=100
else if (i==1) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do

l(q) = c(t)

  end if
end do
    Write (6,*) l(1),' ,', l(2),' ,', l(3),' ,', l(4)
    f=0
   
do q=1,4 
if(l(q) == 'TwC' .or. l(q) == 'TwS' .or. l(q) == 'TwH' .or. l(q) == 'TwD')then
F_new = 2+f
f=F_new
else if (l(q) == 'ThC' .or. l(q) == 'ThS' .or. l(q) == 'ThH' .or. l(q) == 'ThD')then
F_new = 3+f
f=F_new
else if (l(q) == 'FoC' .or. l(q) == 'FoS' .or. l(q) == 'FoH' .or. l(q) == 'FoD')then
F_new = 4+f
f=F_new
else if (l(q) == 'FiC' .or. l(q) == 'FiS' .or. l(q) == 'FiH' .or. l(q) == 'FiD')then
F_new = 5+f
f=F_new
end if

end do
Write (6,*) 'score is,', f

 

Endsubroutine cardsforl

subroutine cardsfork
integer :: i, t, q, f, f_new
Character(Len=3), dimension(1:4) ::  C, S, H, D, k
Character(Len=3), Dimension(1:4) :: j
real :: p
Character(len=10) :: name1

C = (/'TwC', 'ThC', 'FoC', 'FiC'/)
S = (/'TwS', 'ThS', 'FoS', 'FiS'/)
H = (/'TwH', 'ThH', 'FoH', 'FiH'/)
D = (/'TwD', 'ThD', 'FoD', 'FiD'/)



i=0
t=100

  do q=1,4
    i=0
do while (i > 4 .or. i == 0)
Call Random_number(p)
i = P*10
  end do
 

 t=100
if (i==4) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
k(q) = d(t)
t=100
 
else if (i==3)then
    do while (t>4 .or. t == 0)
      Call Random_number(p)
      t = P*10
  end do
 k(q) = h(t)



t=100
else if (i==2) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
  k(q) = s(t)


 
t=100
else if (i==1) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do

k(q) = c(t)

  end if
end do
    Write (6,*) k(1),' ,', k(2),' ,', k(3),' ,', k(4)
     f=0
   
do q=1,4 
if(k(q) == 'TwC' .or. k(q) == 'TwS' .or. k(q) == 'TwH' .or. k(q) == 'TwD')then
F_new = 2+f
f=F_new
else if (k(q) == 'ThC' .or. k(q) == 'ThS' .or. k(q) == 'ThH' .or. k(q) == 'ThD')then
F_new = 3+f
f=F_new
else if (k(q) == 'FoC' .or. k(q) == 'FoS' .or. k(q) == 'FoH' .or. k(q) == 'FoD')then
F_new = 4+f
f=F_new
else if (k(q) == 'FiC' .or. k(q) == 'FiS' .or. k(q) == 'FiH' .or. k(q) == 'FiD')then
F_new = 5+f
f=F_new
end if

end do
Write (6,*) 'score is,', f
 

Endsubroutine cardsfork


subroutine cardsforg
integer :: i, t, q, f, f_new
Character(Len=3), dimension(1:4) ::  C, S, H, D, g
Character(Len=3), Dimension(1:4) :: j
real :: p
Character(len=10) :: name1

C = (/'TwC', 'ThC', 'FoC', 'FiC'/)
S = (/'TwS', 'ThS', 'FoS', 'FiS'/)
H = (/'TwH', 'ThH', 'FoH', 'FiH'/)
D = (/'TwD', 'ThD', 'FoD', 'FiD'/)



i=0
t=100

  do q=1,4
    i=0
do while (i > 4 .or. i == 0)
Call Random_number(p)
i = P*10
  end do
  

 t=100
if (i==4) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
g(q) = d(t)
t=100
 
else if (i==3)then
    do while (t>4 .or. t == 0)
      Call Random_number(p)
      t = P*10
  end do
 g(q) = h(t)



t=100
else if (i==2) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
  g(q) = s(t)


 
t=100
else if (i==1) then
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do

g(q) = c(t)

  end if
end do
f=0
    Write (6,*) g(1),' ,', g(2),' ,', g(3),' ,', g(4)
do q=1,4 
if(g(q) == 'TwC' .or. g(q) == 'TwS' .or. g(q) == 'TwH' .or. g(q) == 'TwD')then

F_new = 2+f
f=F_new
else if (g(q) == 'ThC' .or. g(q) == 'ThS' .or. g(q) == 'ThH' .or. g(q) == 'ThD')then
 
F_new = 3+f
f=F_new
else if (g(q) == 'FoC' .or. g(q) == 'FoS' .or. g(q) == 'FoH' .or. g(q) == 'FoD')then
  
F_new = f+4
f=F_new
else if (g(q) == 'FiC' .or. g(q) == 'FiS' .or. g(q) == 'FiH' .or. g(q) == 'FiD')then
  
F_new = 5+f
f=F_new
end if

end do
Write (6,*) 'score is,', f
   

 

Endsubroutine cardsforg

Subroutine scoreforo

Character(len=3), Dimension(1:4) :: o
INTEGER :: q, f, F_new
Real :: p

  Write (6,*) o(1),' ,', o(2),' ,', o(3),' ,', o(4)
do q=1,4
  
  if(o(q) == 'TwC' .or. o(q) == 'TwS' .or. o(q) == 'TwH' .or. o(q) == 'TwD')then
    write(6,*) 'two points'
F_new = F+2
else if (o(q) == 'ThC' .or. o(q) == 'ThS' .or. o(q) == 'ThH' .or. o(q) == 'ThD')then
  write(6,*) 'two points'
F_new = F+3
else if (o(q) == 'FoC' .or. o(q) == 'FoS' .or. o(q) == 'FoH' .or. o(q) == 'FoD')then
  write(6,*) 'two points'
F_new = F+4
else if (o(q) == 'FiC' .or. o(q) == 'FiS' .or. o(q) == 'FiH' .or. o(q) == 'FiD')then
  write(6,*) 'two points'
F_new = F+5
end if
Write(6,*) F
F_new = F
end do
Write(6,*) f
end subroutine scoreforo