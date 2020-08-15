program interesting
implicit none
real::y, taylorvalueatx, factorial, x, interval, pi, valueofderivative
integer:: i, n
pi=4*atan(1.0)
n=15
interval=0.1
 x=-2*pi
 DO WHILE(x<=pi/2)
   taylorvalueatx=0
   factorial=1
   DO i=1,n
     factorial=factorial*i
     IF (i>1) THEN
       valueofderivative = ((-5.0**(0.5))/(i-1.0))*sin(-(i-1.0)*atan(2.0))
       taylorvalueatx=(valueofderivative*x**i)/factorial
       ELSE IF(i==1) THEN
         taylorvalueatx=sin(0.0)*exp(-0.0/2.0)
         END IF
     y=sin(x)*exp(-x/2)
     WRITE(6,*) x, taylorvalueatx, y
     x=x+interval
     WRITE(6,*) factorial
   END DO
 END DO

end program interesting

 x=-2*pi
 DO WHILE(x<=2*pi)
   taylorvalueatx=0
   factorial=1
   DO i=1,n
     factorial_new=factorial*i
     factorial=factorial_new
     p=i-1
     IF (p>0) THEN
       valueofderivative = ((-(5)**0.5)/2)**(p)*(sin(-(p)*Atan(2.0)))
       taylorvalueatx=taylorvalueatx+(x)**(p)*(valueofderivative/factorial)

       ELSE IF(p==0) THEN
         taylorvalueatx=sin(0.0)*exp(-0.0/2.0)
         END IF
     y=sin(x)*exp(-x/2)     
   END DO
   WRITE(6,*) x, taylorvalueatx, y
     x=x+interval
 END DO