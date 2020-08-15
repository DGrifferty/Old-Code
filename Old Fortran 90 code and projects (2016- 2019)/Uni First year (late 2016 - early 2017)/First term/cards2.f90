Program dealingcards
implicit none 

Character :: fig, figs
Character(Len=10) :: name1, name3, name4, name2
Character(Len=3) :: TwC, ThC, FoC, FiC
Character(len=3) :: TwS, ThS, FoS, FiS
Character(len=3) :: TwH, ThH, FoH, FiH
Character(len=3) :: TwD, ThD, FoD, FiD, ppp
Character(len=3), Dimension(1:4) :: C, S, H, D 
Character(len=3), Dimension(1:4) :: o, l, k, g
Character(Len=3), Dimension(1) :: j
INTEGER :: i, t, q, f, F_new
Real :: p


INTERFACE 
Subroutine cardsforo
Character :: fig, figs
Character(Len=10) :: name1, name3, name4, name2
Character(Len=3) :: TwC, ThC, FoC, FiC
Character(len=3) :: TwS, ThS, FoS, FiS
Character(len=3) :: TwH, ThH, FoH, FiH
Character(len=3) :: TwD, ThD, FoD, FiD, ppp
Character(len=3), Dimension(1:4) :: C, S, H, D 
Character(len=3), Dimension(1:4) :: o, l, k, g
Character(Len=3), Dimension(1) :: j
INTEGER :: i, t, q, f, F_new, z, z_new, x, x_new, b, b_new
Real :: p

END Subroutine  cardsforo
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
  Write (6,*) 'Please enter 2 five character names, and two four character names..'
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

    Call cardsforo
    
 

  


Write(6,*) 'Would you like to deal more cards, type Y for yes, or N for no.'
Read(5,*) figs
    end do
  
End program dealingcards
  

subroutine cardsforo

Character :: fig, figs
Character(Len=10) :: name1, name3, name4, name2
Character(Len=3) :: TwC, ThC, FoC, FiC
Character(len=3) :: TwS, ThS, FoS, FiS
Character(len=3) :: TwH, ThH, FoH, FiH
Character(len=3) :: TwD, ThD, FoD, FiD, ppp
Character(len=3), Dimension(1:4) :: C, S, H, D 
Character(len=3), Dimension(1:4) :: o, l, k, g
Character(Len=3), Dimension(1) :: j
INTEGER :: i, t, q, f, F_new, z, z_new, x, x_new, b, b_new
Real :: p

C = (/'TwC', 'ThC', 'FoC', 'FiC'/)
S = (/'TwS', 'ThS', 'FoS', 'FiS'/)
H = (/'TwH', 'ThH', 'FoH', 'FiH'/)
D = (/'TwD', 'ThD', 'FoD', 'FiD'/)


i=0
t=100

  do q=1,4
    i=0
    o(q) = 'ppp'
do while (i > 4 .or. i == 0)
Call Random_number(p)
i = P*10
  end do


 t=100
if (i==4) then
   o(q) = 'ppp'
  do while (o(q) == 'ppp')
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
o(q) = d(t)
   d(t) = 'ppp'
   end do
t=100
 
else if (i==3)then
  o(q) = 'ppp'
     
  do while (o(q) == 'ppp')
    do while (t>4 .or. t == 0)
      Call Random_number(p)
      t = P*10
  end do
 o(q) = h(t)
 h(t) = 'ppp'
 end do
t=100

else if (i==2) then
  o(q) = 'ppp'
  do while (o(q) == 'ppp')
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
  o(q) = s(t)
  s(t) = 'ppp'
  end do
t=100

else if (i==1) then
o(q) = 'ppp'
  do while (o(q) == 'ppp')
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
o(q) = c(t)
   c(t) = 'ppp'
   end do

  end if
end do

    Write (6,*) o(1),' ,', o(2),' ,', o(3),' ,', o(4)
 f=0   
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

write(6,*) 'got this far'

i=0
t=100

  do q=1,4
    i=0
    l(q) = 'ppp'
do while (i > 4 .or. i == 0)
Call Random_number(p)
i = P*10
  end do
write(6,*) 'got this far'

 t=100
if (i==4) then
  l(q) = 'ppp'
  do while (l(q) == 'ppp')
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
l(q) = d(t)
d(t) = 'ppp'
   end do
t=100

 
else if (i==3)then
  l(q) = 'ppp'
    do while (l(q) == 'ppp')
    do while (t>4 .or. t == 0)
      Call Random_number(p)
      t = P*10
  end do
 l(q) = h(t)
 h(t) = 'ppp'
 end do
t=100

else if (i==2) then
  l(q) = 'ppp'
   do while (l(q) == 'ppp')
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
  l(q) = s(t)
  s(t) = 'ppp'
 end do


 
t=100
else if (i==1) then
  l(q) = 'ppp'
  do while (l(q) == 'ppp')
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
l(q) = c(t)
  c(t) = 'ppp'
 end do
t=100
  end if
end do
write(6,*) 'got this far'
    Write (6,*) l
   

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



i=0
t=100

  do q=1,4
    i=0
    k(q) = 'ppp'
do while (i > 4 .or. i == 0)
Call Random_number(p)
i = P*10
  end do
 

 t=100
if (i==4) then
  k(q) = 'ppp'
  do while (k(q) == 'ppp')
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
k(q) = d(t)
d(t) = 'ppp'
 end do
t=100
 
else if (i==3)then
   k(q) = 'ppp'
  do while (k(q) == 'ppp')
    do while (t>4 .or. t == 0)
      Call Random_number(p)
      t = P*10
  end do
 k(q) = h(t)
 h(t) = 'ppp'
 end do



t=100
else if (i==2) then
   k(q) = 'ppp'
   do while (k(q) == 'ppp')
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
  k(q) = s(t)
   s(t) = 'ppp'
 end do



 
t=100
else if (i==1) then
   k(q) = 'ppp'
   do while (k(q) == 'ppp')
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
k(q) = c(t)
  c(t) = 'ppp'
  
 end do
 t=100

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







i=0
t=100

  do q=1,4
    i=0
    g(q) = 'ppp'
do while (i > 4 .or. i == 0)
Call Random_number(p)
i = P*10
  end do
  

 t=100
if (i==4) then
   g(q) = 'ppp'
   do while (g(q) == 'ppp')
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
g(q) = d(t)
   d(t) = 'ppp'
 end do
t=100
 
else if (i==3)then
   g(q) = 'ppp'
  do while (g(q) == 'ppp')
    do while (t>4 .or. t == 0)
      Call Random_number(p)
      t = P*10
  end do
 
 g(q) = h(t)
  h(t) = 'ppp'
 end do



t=100
else if (i==2) then
   g(q) = 'ppp'
   do while (g(q) == 'ppp')
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do
  g(q) = s(t)
 s(t) = 'ppp'
 end do
t=100
else if (i==1) then
   g(q) = 'ppp'
  do while (g(q) == 'ppp')
  do while (t>4 .or. t == 0)
Call Random_number(p)
t = P*10
  end do

g(q) = c(t)
   c(t) = 'ppp'
 end do
 t=100

  end if
end do
    Write (6,*) g(1),' ,', g(2),' ,', g(3),' ,', g(4)
    f=0
 do q=1,4 
if(g(q) == 'TwC' .or. g(q) == 'TwS' .or. g(q) == 'TwH' .or. g(q) == 'TwD')then

F_new = 2+f
f=F_new
else if (g(q) == 'ThC' .or. g(q) == 'ThS' .or. g(q) == 'ThH' .or. g(q) == 'ThD')then
 
F_new = 3+f
f=F_new
else if (g(q) == 'FoC' .or. g(q) == 'FoS' .or. g(q) == 'FoH' .or. g(q) == 'FoD')then
  
F_new = 4+f
f=F_new
else if (g(q) == 'FiC' .or. g(q) == 'FiS' .or. g(q) == 'FiH' .or. g(q) == 'FiD')then
  
F_new = 5+f
f=F_new
end if


end do
Write (6,*) 'score is,', f
endsubroutine cardsforo
 

 
