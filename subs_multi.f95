program subs_multi
implicit none
	real::tol=0.000001
	real,dimension(2)::x,f
	integer::i
	read(*,*) x(1),x(2)

	do i=1,1000
		call func(f,x)
		print*,x,f
		if((abs(x(1)-f(1)).le.tol).and.(abs(x(2)-f(2)).le.tol))exit
		x = f
	end do

	print*,x

end program subs_multi

subroutine func(f,x)
implicit none
	real,intent(in),dimension(2)::x
	real,intent(out),dimension(2)::f

	f(1) = (-2*x(1)**3- 8*x(1) +4*x(2) + 4)/20.0 + x(1)
	f(2) = -(-4*x(1) + x(2)**2 + 3*x(2) + 1)/24.0 + x(2)
	end subroutine func