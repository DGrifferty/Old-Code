Program dealingcards
implicit none 

Character :: fig, p
Character(Len=10) :: name1, name3, name4, name2
Character(Len=3) :: TwC, ThC, FoC, FiC, SiC, SeC, EC, NC, TeC, JC, QC, KC, AC 
Character(len=3) :: TwS, ThS, FoS, FiS, SiS, SeS, ES, NS, TeS, JS, QS, KS, AS
Character(len=3) :: TwH, ThH, FoH, FiH, SiH, SeH, EH, NH, TeH, JH, QH, KH, AH 
Character(len=3) :: TwD, ThD, FoD, FiD, SiD, SeD, ED, ND, TeD, JD, QD, KD, AD
Character(len=100), Dimension (1:13) :: Clubs, Spades, Hearts, Diamonds
integer, Dimension(1) :: a, b, c, d

INTEGER :: i, Randomnumber
INTERFACE 
FUNCTION Randomnumber
INTEGER :: i, Randomnumber
END FUNCTION Randomnumber
END INTERFACE

Clubs = (/TwC, ThC, FoC, FiC, SiC, SeC, EC, NC, TeC, JC, QC, KC, AC/)
Spades = (/TwS, ThS, FoS, FiS, SiS, SeS, ES, NS, TeS, JS, QS, KS, AS/)
Hearts = (/TwH, ThH, FoH, FiH, SiH, SeH, EH, NH, TeH, JH, QH, KH, AH/)
Diamonds = (/TwD, ThD, FoD, FiD, SiD, SeD, ED, ND, TeD, JD, QD, KD, AD/)
Write (6,*) Clubs
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

  End program dealingcards
  

FUNCTION randomnumber
integer :: i


Endfunction randomnumber