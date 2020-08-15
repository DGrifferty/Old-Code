PROGRAM TAX
IMPLICIT NONE
REAL:: Salary, SalaryTaxed
READ(5,*) Salary
Salary=Salary-9000
if (salary<=30000) THEN
  SalaryTaxed=Salary*0.8
  ELSE IF (Salary>=30000) THEN
    SalaryTaxed=Salary*0.6
END IF

Salary=SalaryTaxed+9000

Write(6,*) Salary

End Program
  