!--------------------------------------------------
!PHY1038 Assignment 5 Linear Algebra
!URN: 6408056
!MAY 2017
!--------------------------------------------------

PROGRAM Linear_Algebra

IMPLICIT NONE

REAL, DIMENSION(1:3,1:3) :: A, B, C, D, Inverse 
REAL, DIMENSION(1:3,1:1) :: ANSWERS, SOLVED
REAL :: Inversecheck

































ANSWERS(1,1)=0 ; ANSWERS(2,1)=3 ; ANSWERS(3,1)=1
WRITE(6,*) ANSWERS


OPEN(unit=20,file='matrix.dat')

READ(20,*) A(1,1), A(1,2), A(1,3)
READ(20,*) A(2,1), A(2,2), A(2,3) 
READ(20,*) A(3,1), A(3,2), A(3,3)

CLOSE(20)

B=A

WRITE(6,*) B(1,1), B(1,2), B(1,3)!TEMP
WRITE(6,*) B(2,1), B(2,2), B(2,3) 
WRITE(6,*) B(3,1), B(3,2), B(3,3)

WRITE(6,*) Determinant(B)!Temp

CALL Cofactor(C, B)
D = Transpose(C)
INVERSE=(1/Determinant(B))*D
WRITE(6,*) Inverse(1,1), Inverse(1,2), Inverse(1,3)!TEMP
WRITE(6,*) Inverse(2,1), Inverse(2,2), Inverse(2,3) 
WRITE(6,*) Inverse(3,1), Inverse(3,2), Inverse(3,3)
Inversecheck = MATMUL(B,Inverse)
WRITE(6,*) Inversecheck

WRITE(6,*)'HERE'
Solved=Matmul(Inverse,Answers)
WRITE(6,*) Transpose(solved)


CONTAINS

FUNCTION Determinant(B)
IMPLICIT NONE
REAL, DIMENSION(1:3,1:3) :: B
REAL, DIMENSION(1:2,1:2) :: MTBT
REAL :: Determinant, X

MTBT(1,1) = B(2,2); MTBT(1,2) = B(2,3)
MTBT(2,1) = B(3,2); MTBT(2,2) = B(3,3)

X = B(1,1)*(MTBT(1,1)*MTBT(2,2)-MTBT(1,2)*MTBT(2,1))


MTBT(1,1) = B(1,2); MTBT(1,2) = B(1,3)
MTBT(2,1) = B(3,2); MTBT(2,2) = B(3,3)

X = X -B(2,1)*(MTBT(1,1)*MTBT(2,2)-MTBT(1,2)*MTBT(2,1))


MTBT(1,1) = B(1,2); MTBT(1,2) = B(1,3)
MTBT(2,1) = B(2,2); MTBT(2,2) = B(2,3)

Determinant = X+B(3,1)*(MTBT(1,1)*MTBT(2,2)-MTBT(1,2)*MTBT(2,1))

END FUNCTION

SUBROUTINE Cofactor(C,B)
IMPLICIT NONE
REAL, DIMENSION(1:3,1:3) :: B, C
REAL, DIMENSION(1:2,1:2) :: MTBT


MTBT(1,1) = B(2,2); MTBT(1,2) = B(2,3)
MTBT(2,1) = B(3,2); MTBT(2,2) = B(3,3)

C(1,1) = MTBT(1,1)*MTBT(2,2)-MTBT(1,2)*MTBT(2,1)

MTBT(1,1) = B(2,1); MTBT(1,2) = B(2,3)
MTBT(2,1) = B(3,1); MTBT(2,2) = B(3,3)

C(1,2) = -(MTBT(1,1)*MTBT(2,2)-MTBT(1,2)*MTBT(2,1))

MTBT(1,1) = B(2,1); MTBT(1,2) = B(2,2)
MTBT(2,1) = B(3,1); MTBT(2,2) = B(3,2)

C(1,3) = MTBT(1,1)*MTBT(2,2)-MTBT(1,2)*MTBT(2,1)

MTBT(1,1) = B(1,2); MTBT(1,2) = B(1,3)
MTBT(2,1) = B(3,2); MTBT(2,2) = B(3,3)

C(2,1) = -(MTBT(1,1)*MTBT(2,2)-MTBT(1,2)*MTBT(2,1))

MTBT(1,1) = B(1,1); MTBT(1,2) = B(1,3)
MTBT(2,1) = B(3,1); MTBT(2,2) = B(3,3)

C(2,2) = MTBT(1,1)*MTBT(2,2)-MTBT(1,2)*MTBT(2,1)

MTBT(1,1) = B(1,1); MTBT(1,2) = B(1,2)
MTBT(2,1) = B(3,1); MTBT(2,2) = B(3,2)

C(2,3) = -(MTBT(1,1)*MTBT(2,2)-MTBT(1,2)*MTBT(2,1))

MTBT(1,1) = B(1,2); MTBT(1,2) = B(1,3)
MTBT(2,1) = B(2,2); MTBT(2,2) = B(2,3)

C(3,1) = MTBT(1,1)*MTBT(2,2)-MTBT(1,2)*MTBT(2,1)

MTBT(1,1) = B(1,1); MTBT(1,2) = B(1,3)
MTBT(2,1) = B(2,1); MTBT(2,2) = B(2,3)

C(3,2) = -(MTBT(1,1)*MTBT(2,2)-MTBT(1,2)*MTBT(2,1))

MTBT(1,1) = B(1,1); MTBT(1,2) = B(1,2)
MTBT(2,1) = B(2,1); MTBT(2,2) = B(2,2)

C(3,3) = (MTBT(1,1)*MTBT(2,2)-MTBT(1,2)*MTBT(2,1))

WRITE(6,*) C(1,1), C(1,2), C(1,3)
WRITE(6,*) C(2,1), C(2,2), C(2,3) 
WRITE(6,*) C(3,1), C(3,2), C(3,3)

END SUBROUTINE

END PROGRAM