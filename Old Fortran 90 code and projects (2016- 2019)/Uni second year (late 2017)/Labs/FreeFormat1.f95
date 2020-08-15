PROGRAM Photoluminescence_of_Semiconductors
IMPLICIT NONE
REAL:: Et, Eg, h, e, r, Me, Mh, lambda, X, test, electronmass, Difference

h = 6.6261E-34; e = 1.6022E-19; electronmass = 9.1094E-31; X=0.0 !Setting constants

r = 6.51772E-9 !R to be user inputted
lambda = 639.02E-9 !wavelength to be user inputted
Et = (h*(3E8))/lambda
Et = Et/e !total energy in ev

DO
  X = X+0.00001

  Me=X*(0.13*electronmass)+(1-X)*(0.21*electronmass)
  Mh=X*(0.8*electronmass)+(1-X)*(0.45*electronmass)
  Eg=X*(2.45)+((1-X)*1.74)-(0.24*(X)*(1-X))!Useing equations as in labscript
  test=Eg+((h**2)/(8*e*r**2))*(1/Me+1/Mh)

  Difference = abs(Et-test)
  WRITE(6,*) Difference
   
  If (Difference<=0.00001) THEN
    EXIT
  END IF
!When x value gives an anwer close enough to the total energy, do loop exits
END DO

WRITE(6,*) 'X', X
WRITE(6,*) 'me', Me
WRITE(6,*) 'mh', Mh
WRITE(6,*) 'Eg', Eg
WRITE(6,*) 'TEST', Test
WRITE(6,*) 'ET', Et
!Program writes answers to screen
END PROGRAM
  