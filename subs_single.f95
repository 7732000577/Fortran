program substitution
implicit none
	
	real::x,f,tol = 0.00001
	integer::i
	read(*,*) x
	

	do i=1,1000
		call func(x,f)
		print*,x,f
		if(abs(x-f).le.tol)exit
		x=f
	end do

	print*,x


	
end program substitution

subroutine func(x,f)
implicit none
	real,intent(in)::x
	real,intent(out)::f
		f = (1.0/3.0)*exp(x)
end subroutine func

