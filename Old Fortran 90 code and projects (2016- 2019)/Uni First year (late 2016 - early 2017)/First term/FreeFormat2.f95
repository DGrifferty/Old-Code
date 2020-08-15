
program randomnumber
Real :: p
integer :: i
i=0
do while (i>4 .or. i==0)
Call Random_number(p)
i = p * 10
write (6,*) i
end do 
end program randomnumber

