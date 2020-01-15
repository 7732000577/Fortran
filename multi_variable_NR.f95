program NR_multi_variable
implicit none
	real,dimension(2,1)::initial_values,multi,func,error_values,x,error=(1,1),y
	real,dimension(2,2)::f_deriv,inv
	integer::j=0

	write(*,*)"enter x values"
	read(*,*) initial_values
	write(*,*)"enter error values"
	read(*,*) error_values

	call function(initial_values,func)
	call function_deriv(initial_values,f_deriv)
	call inversee(f_deriv,inv)
	call multiplication(inv,func,multi)
	x = initial_values - multi

	do while(error(1,1) .ge. error_values(1,1) .or. error(2,1) .ge. error_values(2,1))
		call function(x,func)
		call function_deriv(x,f_deriv)
		call inversee(f_deriv,inv)
		call multiplication(inv,func,multi)
		x = x - multi
		
		error(1,1) = abs(func(1,1))
		error(2,1) = abs(func(2,1))

		j = j + 1
	end do

	print*,j
	print*,x
	call function(x,func)
	print*,func
end program NR_multi_variable



subroutine inversee(a,inv)
implicit none
	real,dimension(2,2),intent(in)::a
	real,dimension(2,2),intent(out)::inv
	real::det
	integer::i,j,k

	det = a(1,1)*a(2,2) - a(2,1)*a(1,2)

	inv(1,1) = a(2,2)/det
	inv(1,2) = -1*a(1,2)/det
	inv(2,1) = -1*a(2,1)/det
	inv(2,2) = a(1,1)/det
end subroutine inversee



subroutine function(x,func)
implicit none
	real,dimension(2,1),intent(in)::x
	real,dimension(2,1),intent(out)::func
	real::y2,y3

	 y2 = x(1,1) 
	 y3 = x(2,1) 

	func(1,1) = 4 - 8*y2 + 4*y3 - 2*(y2**3)
	func(2,1) = 1 - 4*y2 + 3*y3 + y3**2
end subroutine function



subroutine function_deriv(x,f_deriv)
implicit none	
	real,dimension(2,1),intent(in)::x
	real,dimension(2,2),intent(out)::f_deriv
	real::y2,y3

	y2 = x(1,1) 
	y3 = x(2,1)

	f_deriv(1,1) = -8 - 6*(y2**2)
	f_deriv(1,2) = 4
	f_deriv(2,1) = -4
	f_deriv(2,2) = 3 + 2*y3
end subroutine function_deriv



subroutine multiplication(inv,func,multi)
implicit none
	real,dimension(2,1),intent(in)::func
	real,dimension(2,2),intent(in)::inv
	real,dimension(2,1),intent(out)::multi

	multi(1,1) = inv(1,1)*func(1,1) + inv(1,2)*func(2,1)
	multi(2,1) = inv(2,1)*func(1,1) + inv(2,2)*func(2,1)
end subroutine multiplication