program dealinglotsofcards
implicit none
Character :: fig, figs
Character(Len=8) :: name1, name3, name4, name2
Character(Len=3) :: TwC, ThC, FoC, FiC, SiC, SeC, EiC, NiC, TeC, JaC, QuC, KiC, AcC 
Character(len=3) :: TwS, ThS, FoS, FiS, SiS, SeS, EiS, NiS, TeS, JaS, QuS, KiS, AcS
Character(len=3) :: TwH, ThH, FoH, FiH, SiH, SeH, EiH, NiH, TeH, JaH, QuH, KiH, AcH 
Character(len=3) :: TwD, ThD, FoD, FiD, SiD, SeD, EiD, NiD, TeD, aJD, QuD, KiD, AcD, ppp
Character(len=3), Dimension(1:13) :: C, S, H, D 
Character(len=3), Dimension(1:6) :: o, l, k, g
Character(Len=3), Dimension(1) :: j
INTEGER :: i, t, q, f, F_new, x, z, v, b
Real :: p

c = (/'TwC', 'ThC', 'FoC', 'FiC', 'SiC', 'SeC', 'EiC', 'NiC', 'TeC', 'JaC', 'QuC', 'KiC', 'AcC'/)
S = (/'TwS', 'ThS', 'FoS', 'FiS', 'SiS', 'SeS', 'EiS', 'NiS', 'TeS', 'JaS', 'QuS', 'KiS', 'AcS'/)
H = (/'TwH', 'ThH', 'FoH', 'FiH', 'SiH', 'SeH', 'EiH', 'NiH', 'TeH', 'JaH', 'QuH', 'KiH', 'AcH'/)
D = (/'TwD', 'ThD', 'FoD', 'FiD', 'SiD', 'SeD', 'EiD', 'NiD', 'TeD', 'JaD', 'QuD', 'KiD', 'AcD'/)

Write (6,*) 'This programme deals six cards to four players, then choses the winner based on the value of the cards.'
Write (6,*) 'By default this programme deals them to four players called north, south, east, and west'
Write (6,*) 'Would you like to type in your own player names, type Y for yes or N for no.'
Read (5,*) fig

if (fig == 'N' .or. fig == 'n') then
  name1 = 'North'
  name2 = 'East'
  name3 = 'South'
  name4 = 'West'
  end if
do b=1,7
  if (fig== 'y' .or. fig == 'Y') then
  Write (6,*) 'Please enter 4 names.'
  Read (5,*) name1, name2, name3, name4
  Write (6,*) 'The names you entered are ',name1,', ',name2,', ',name3,', ',name4
  Write (6,*) 'Would you like to try again, type Y for yes, and N for no' 
  Read (5,*) fig
  if (fig == 'N' .or. fig == 'n') exit
    end if
    end do
    Write(6,*) 'Dealing cards'
  

figs = 'y'
do while (figs == 'y' .or. figs == 'Y')
c = (/'TwC', 'ThC', 'FoC', 'FiC', 'SiC', 'SeC', 'EiC', 'NiC', 'TeC', 'JaC', 'QuC', 'KiC', 'AcC'/)
S = (/'TwS', 'ThS', 'FoS', 'FiS', 'SiS', 'SeS', 'EiS', 'NiS', 'TeS', 'JaS', 'QuS', 'KiS', 'AcS'/)
H = (/'TwH', 'ThH', 'FoH', 'FiH', 'SiH', 'SeH', 'EiH', 'NiH', 'TeH', 'JaH', 'QuH', 'KiH', 'AcH'/)
D = (/'TwD', 'ThD', 'FoD', 'FiD', 'SiD', 'SeD', 'EiD', 'NiD', 'TeD', 'JaD', 'QuD', 'KiD', 'AcD'/)

  
do q=1,5
    i=0
    t=100
do while (i > 52 .or. i == 0)
Call Random_number(p)
i = P*100
  end do

  if (i>=1 .and. i<=13) then
   o(q) = 'ppp'
  do while (o(q) == 'ppp')
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  o(q) = d(t)
  d(t) = 'ppp'
   end do
t=100
else  if (i>=14 .and. i<=26) then
   o(q) = 'ppp'
  do while (o(q) == 'ppp')
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  o(q) = h(t)
  h(t) = 'ppp'
   end do
t=100

 else if (i>=27 .and. i<=39) then
   o(q) = 'ppp'
  do while (o(q) == 'ppp')
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  o(q) = c(t)
  c(t) = 'ppp'
   end do
t=100


 else if (i>=40 .and. i<=52) then
   o(q) = 'ppp'
  do while (o(q) == 'ppp')
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  o(q) = s(t)
  s(t) = 'ppp'
   end do
t=100
    end if
end do

Write (6,*) name1,'''s cards are ', o(1),' ,', o(2),' ,', o(3),' ,', o(4),' ,', o(5),' ,', o(6)

f=0

do q=1,5
 
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
else if (o(q) == 'SiC' .or. o(q) == 'SiS' .or. o(q) == 'SiH' .or. o(q) == 'SiD')then
  
F_new = 6+f
f=F_new
else if (o(q) == 'SeC' .or. o(q) == 'SeS' .or. o(q) == 'SeH' .or. o(q) == 'SeD')then
  
F_new = 7+f
f=F_new
else if (o(q) == 'EiC' .or. o(q) == 'EiS' .or. o(q) == 'EiH' .or. o(q) == 'EiD')then
  
F_new = 8+f
f=F_new
else if (o(q) == 'NiC' .or. o(q) == 'NiS' .or. o(q) == 'NiH' .or. o(q) == 'NiD')then
  
F_new = 9+f
f=F_new
else if (o(q) == 'JaC' .or. o(q) == 'JaS' .or. o(q) == 'JaH' .or. o(q) == 'JaD')then
  
F_new = 10+f
f=F_new
else if (o(q) == 'QuC' .or. o(q) == 'QuS' .or. o(q) == 'QuH' .or. o(q) == 'QuD')then
  
F_new = 11+f
f=F_new
else if (o(q) == 'KiC' .or. o(q) == 'KiS' .or. o(q) == 'KiH' .or. o(q) == 'KiD')then
  
F_new = 12+f
f=F_new
else if (o(q) == 'AcC' .or. o(q) == 'AcS' .or. o(q) == 'AcH' .or. o(q) == 'AcD')then
  
F_new = 13+f
f=F_new
end if
end do
z=f

Write (6,*) name1,'''sscore is,', z

 
   
    
    
    
     do q=1,5
    i=0
    t=100
do while (i > 52 .or. i == 0)
Call Random_number(p)
i = P*100
  end do

  if (i>=1 .and. i<=13) then
   l(q) = 'ppp'
  do while (l(q) == 'ppp')
    t=100
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  l(q) = d(t)
  d(t) = 'ppp'
   end do
t=100
else  if (i>=14 .and. i<=26) then
   l(q) = 'ppp'
  do while (l(q) == 'ppp')
    t=100
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  l(q) = h(t)
  h(t) = 'ppp'
   end do
t=100

 else if (i>=27 .and. i<=39) then
   l(q) = 'ppp'
  do while (l(q) == 'ppp')
    t=100
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  l(q) = c(t)
  c(t) = 'ppp'
   end do
t=100


 else if (i>=40 .and. i<=52) then
   l(q) = 'ppp'
  do while (l(q) == 'ppp')
    t=100
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  l(q) = s(t)
  s(t) = 'ppp'
   end do
t=100
    end if
end do
Write (6,*) name2,'''s cards are ', l(1),' ,', l(2),' ,', l(3),' ,', l(4),' ,', l(5),' ,', l(6)
 f=0

do q=1,5
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
else if (l(q) == 'SiC' .or. l(q) == 'SiS' .or. l(q) == 'SiH' .or. l(q) == 'SiD')then
  
F_new = 6+f
f=F_new
else if (l(q) == 'SeC' .or. l(q) == 'SeS' .or. l(q) == 'SeH' .or. l(q) == 'SeD')then
  
F_new = 7+f
f=F_new
else if (l(q) == 'EiC' .or. l(q) == 'EiS' .or. l(q) == 'EiH' .or. l(q) == 'EiD')then
  
F_new = 8+f
f=F_new
else if (l(q) == 'NiC' .or. l(q) == 'NiS' .or. l(q) == 'NiH' .or. l(q) == 'NiD')then
  
F_new = 9+f
f=F_new
else if (l(q) == 'JaC' .or. l(q) == 'JaS' .or. l(q) == 'JaH' .or. l(q) == 'JaD')then
  
F_new = 10+f
f=F_new
else if (l(q) == 'QuC' .or. l(q) == 'QuS' .or. l(q) == 'QuH' .or. l(q) == 'QuD')then
  
F_new = 11+f
f=F_new
else if (l(q) == 'liC' .or. l(q) == 'liS' .or. l(q) == 'liH' .or. l(q) == 'liD')then
  
F_new = 12+f
f=F_new
else if (l(q) == 'AcC' .or. l(q) == 'AcS' .or. l(q) == 'AcH' .or. l(q) == 'AcD')then
  
F_new = 13+f
f=F_new
end if
end do
x=f


Write (6,*) name2,'''sscore is,', x




     do q=1,5
     

    i=0
    t=100
do while (i > 52 .or. i == 0)
Call Random_number(p)
i = P*100
  end do

  if (i>=1 .and. i<=13) then
   k(q) = 'ppp'
  do while (k(q) == 'ppp')
    t=100
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  k(q) = d(t)
  d(t) = 'ppp'
   end do
t=100
else  if (i>=14 .and. i<=26) then
   k(q) = 'ppp'
  do while (k(q) == 'ppp')
    t=100
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  k(q) = h(t)
  h(t) = 'ppp'
   end do
t=100

 else if (i>=27 .and. i<=39) then
   k(q) = 'ppp'
  do while (k(q) == 'ppp')
    t=100
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  k(q) = c(t)
  c(t) = 'ppp'
   end do
t=100


 else if (i>=40 .and. i<=52) then
   k(q) = 'ppp'
  do while (k(q) == 'ppp')
    t=100
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  k(q) = s(t)
  s(t) = 'ppp'
   end do
t=100
    end if
end do
Write (6,*)name3,'''s cards are ', k(1),' ,', k(2),' ,', k(3),' ,', k(4),' ,', k(5),' ,', k(6)
 f=0

do q=1,5
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
else if (k(q) == 'SiC' .or. k(q) == 'SiS' .or. k(q) == 'SiH' .or. k(q) == 'SiD')then
  
F_new = 6+f
f=F_new
else if (k(q) == 'SeC' .or. k(q) == 'SeS' .or. k(q) == 'SeH' .or. k(q) == 'SeD')then
  
F_new = 7+f
f=F_new
else if (k(q) == 'EiC' .or. k(q) == 'EiS' .or. k(q) == 'EiH' .or. k(q) == 'EiD')then
  
F_new = 8+f
f=F_new
else if (k(q) == 'NiC' .or. k(q) == 'NiS' .or. k(q) == 'NiH' .or. k(q) == 'NiD')then
  
F_new = 9+f
f=F_new
else if (k(q) == 'JaC' .or. k(q) == 'JaS' .or. k(q) == 'JaH' .or. k(q) == 'JaD')then
  
F_new = 10+f
f=F_new
else if (k(q) == 'QuC' .or. k(q) == 'QuS' .or. k(q) == 'QuH' .or. k(q) == 'QuD')then
  
F_new = 11+f
f=F_new
else if (k(q) == 'KiC' .or. k(q) == 'KiS' .or. k(q) == 'KiH' .or. k(q) == 'KiD')then
  
F_new = 12+f
f=F_new
else if (k(q) == 'AcC' .or. k(q) == 'AcS' .or. k(q) == 'AcH' .or. k(q) == 'AcD')then
  
F_new = 13+f
f=F_new
end if
end do
v=f


Write (6,*) name3,'''sscore is,', v

do q=1,5
     

    i=0
    t=100
do while (i > 52 .or. i == 0)
Call Random_number(p)
i = P*100
  end do

  if (i>=1 .and. i<=13) then
   g(q) = 'ppp'
  do while (g(q) == 'ppp')
    t=100
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  g(q) = d(t)
  d(t) = 'ppp'
   end do
t=100
else  if (i>=14 .and. i<=26) then
   g(q) = 'ppp'
  do while (g(q) == 'ppp')
    t=100
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  g(q) = h(t)
  h(t) = 'ppp'
   end do
t=100

 else if (i>=27 .and. i<=39) then
   g(q) = 'ppp'
  do while (g(q) == 'ppp')
    t=100
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  g(q) = c(t)
  c(t) = 'ppp'
   end do
t=100


 else if (i>=40 .and. i<=52) then
   g(q) = 'ppp'
  do while (g(q) == 'ppp')
    t=100
  do while (t>13 .or. t == 0)
Call Random_number(p)
t = P*100
  end do
  g(q) = s(t)
  s(t) = 'ppp'
   end do
t=100
    end if
end do
Write (6,*)name4,'''s cards are ', g(1),' ,', g(2),' ,', g(3),' ,', g(4),' ,', g(5),' ,', g(6)
 f=0

do q=1,5
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
else if (g(q) == 'SiC' .or. g(q) == 'SiS' .or. g(q) == 'SiH' .or. g(q) == 'SiD')then
  
F_new = 6+f
f=F_new
else if (g(q) == 'SeC' .or. g(q) == 'SeS' .or. g(q) == 'SeH' .or. g(q) == 'SeD')then
  
F_new = 7+f
f=F_new
else if (g(q) == 'EiC' .or. g(q) == 'EiS' .or. g(q) == 'EiH' .or. g(q) == 'EiD')then
  
F_new = 8+f
f=F_new
else if (g(q) == 'NiC' .or. g(q) == 'NiS' .or. g(q) == 'NiH' .or. g(q) == 'NiD')then
  
F_new = 9+f
f=F_new
else if (g(q) == 'JaC' .or. g(q) == 'JaS' .or. g(q) == 'JaH' .or. g(q) == 'JaD')then
  
F_new = 10+f
f=F_new
else if (g(q) == 'QuC' .or. g(q) == 'QuS' .or. g(q) == 'QuH' .or. g(q) == 'QuD')then
  
F_new = 11+f
f=F_new
else if (g(q) == 'giC' .or. g(q) == 'giS' .or. g(q) == 'giH' .or. g(q) == 'giD')then
  
F_new = 12+f
f=F_new
else if (g(q) == 'AcC' .or. g(q) == 'AcS' .or. g(q) == 'AcH' .or. g(q) == 'AcD')then
  
F_new = 13+f
f=F_new
end if
end do



Write (6,*) name4,'''sscore is,', f


if (z>x .and. z>v .and. z>f) then
  Write (6,*) name1, 'wins!'
 else if (x>z .and. x>v .and. x>f) then
  Write (6,*) name2, 'wins!'
  else if (v>x .and. v>z .and. v>f) then
  Write (6,*) name3, 'wins!'
 else if (f>x .and. f>v .and. f>z) then
  Write (6,*) name4, 'wins!'
  else 
    Write(6,*) 'It''s a draw!'
    end if
     Write (6,*) 'Would you like to try again, type Y for yes, and N for no'
  Read (5,*) figs

end do

end program dealinglotsofcards