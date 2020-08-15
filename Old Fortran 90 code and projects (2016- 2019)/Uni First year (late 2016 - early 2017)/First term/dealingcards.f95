Program dealingcards
implicit none 

Character :: fig, p
Character(Len=10) :: name1, name3, name4, name2
Character (Len=6) :: TwoC, ThreeC, FourC, FiveC, SixC, SevenC, EightC, NineC, TenC, JC, QC, KC, AC
Character (Len=6) :: TwoS, ThreeS, FourS, FiveS, SixS, SevenS, EightS, NineS, TenS, JS, QS, KS, AS
Character (Len=6) :: TwoH, ThreeH, FourH, FiveH, SixH, SevenH, EightH, NineH, TenH, JH, QH, KH, AH
Character (Len=6) :: TwoD, ThreeD, FourD, FiveD, SixD, SevenD, EightD, NineD, TenD, JD, QD, KD, AD

Character, Dimension(2) :: a
INTEGER :: i, Randomnumber


INTERFACE 
FUNCTION Randomnumber
INTEGER :: i, Randomnumber
END FUNCTION Randomnumber
END INTERFACE
a = (/TwoC, ThreeC, FourC, FiveC, SixC, SevenC, EightC, NineC, TenC, JC, QC, KC, AC/)
Write (6,*) a
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