subroutine function_deriv(x,f_deriv)
implicit none	
	real,dimension(2,1),intent(in)::x
	real,dimension(2,1),intent(out)::f_deriv
	real::y2,y3

	y2 = x(1,1) 
	y3 = x(2,1)

	f_deriv(1,1) = 200*(x(1,1)**2- x(2,1))*(2*x(1,1)) + 2*(1-x(1,1))*(-1*x(1,1))
	f_deriv(2,1) = -200*(x(1,1)**2- x(2,1))

end subroutine function_deriv

subroutine function(x,func)
implicit none
	real,dimension(2,1),intent(in)::x
	real,intent(out)::func
	real::y2,y1

	 y1 = x(1,1) 
	 y2 = x(2,1) 

	func = 100*(y1**2-y2)**2 + (1-y1)**2

end subroutine function

program second
implicit none

	real,dimension(2,1)::x,f_deriv,y
	real::func,tol=0.0000001,lemda=0.001
	integer::i=0

	print*,"enter value of x1 and x2"
	read(*,*) x(1,1),x(2,1)

	do i=1,1000
	call function_deriv(x,f_deriv)
	call function(x,func)

	print*,"values of x "
	print*, x
	print*,"func value" 
	print*,func

	y = (0,0)
	if((f_deriv(1,1).eq.0) .and. (f_deriv(1,1).eq.0) .and. (abs(x(1,1)-x(2,1)).le.tol))exit
	x = x - lemda*f_deriv

	end do

    print*,x
    call function(x,func)
    print*,func



end program second