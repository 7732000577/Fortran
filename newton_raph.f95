program test
implicit none

	real::initial_point,error_value,f_deriv,func,x,y,x_previous,error
	integer::j=0

	write(*,*)"intital_point,error_value"
	read(*,*) initial_point,error_value

	call function(initial_point,func)
	call function_deriv(initial_point,f_deriv)
	x = initial_point - func/f_deriv
	x_previous = initial_point

	do while((error .ge. error_value).or.(j<=1))
		call function(x,func)
		call function_deriv(x,f_deriv)
		x = x - func/f_deriv
		error = abs(x-x_previous)
		x_previous = x 
		j = j+1
	end do

	print*,j
	print*,x
	print*,error
	call function(x,func)
	print*,func

end program test



subroutine function(x,func)
implicit none

	real,intent(in)::x
	real,intent(out)::func

	func = x - exp(x)*(1/3.0)

end subroutine function



subroutine function_deriv(x,f_deriv)
implicit none
	
	real,intent(in)::x
	real,intent(out)::f_deriv

	f_deriv = 1 - exp(x)*(1/3.0)

end subroutine function_deriv