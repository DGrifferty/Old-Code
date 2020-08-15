Program Birthdayproblem

Real:: rk = selected_real_kind(16)
integer:: i
rk=1 
do i=1,365
  rk = rk * i
  end do
  Write(6,*) rk
  endprogram Birthdayproblem
  