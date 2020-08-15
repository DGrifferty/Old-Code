PROGRAM DETERMINANT_TEST

IMPLICIT NONE

REAL, DIMENSION(1:3,1:3) :: MatrixOne
REAL, DIMENSION(1:2,1:2) :: DOne
REAL, DIMENSION(1,1) :: X

MatrixOne(1,1)=5 ; MatrixOne(1,2)=6; MatrixOne(1,3)=7
MatrixOne(2,1)=10 ;MatrixOne(2,2)=15 ; MatrixOne(2,3)=20
MatrixOne(3,1)=2; MatrixOne(3,2)=3; MatrixOne(3,3)=5


DOne(1,1) = MatrixOne(2,2); DOne(1,2) = MatrixOne(2,3)
DOne(2,1) = MatrixOne(3,2); DOne(2,2) = MatrixOne(3,3)


X=(MatrixOne(1,1)*ABS(DOne))

WRITE(6,*) X

END PROGRAM