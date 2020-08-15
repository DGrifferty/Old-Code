Program evaluatingafunction
Implicit none
Real:: X, fx, q, ex
Character:: fig
Write(6,*) 'This program is designed to evaluate a rational polynomial approximation'
Write(6,*) 'that is an approximation of the exponential function.'
Write(6,*) 'It does this for the range of -1=<x=<1, and provides the value of the polynomial,'
Write(6,*) 'denoted f(x), the value of x and the true value of exp(x).'
fig = 'y'
Do while (fig=='y' .or. fig=='Y')
x=-1
Write(6,*) 'Please enter the increments you would like the polynomaial to evaluate the polynomial for'
Read(5,*) q
Do while(q<=0)
Write(6,*) 'The program starts at -1 so increments must be positive and greater than 0, please try again'
Read(5,*) q
End do
do while (x<=1)
fx = (x**(2)+6*x+12)/(x**(2)-6*x+12)
ex=exp(x)
Write(6,*) 'x=',x,'f(x)=',fx,'Exp(x)=',ex
X=x+q
end do
Write(6,*) 'X has past its range, would you like to run the program again? Type Y for yes and N for no'
Read(5,*) fig
end do
end program evaluatingafunction

  
  