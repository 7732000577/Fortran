program linear_regression
implicit none

	real::a0,a1
	real::add_x,add_y,add_xy,add_xx
	integer::n=7,i
	real,dimension(7,1)::x,y

	open(10,file = "input_data.txt")

	do i=1,n
		read(10,*) x(i,1) ,y(i,1)
	end do
	close(10)

	call func(x,y,n,add_x,add_y,add_xy,add_xx)
	call  final_values(n,add_x,add_y,add_xy,add_xx,a0,a1)

	print*,a0,a1

end program linear_regression

subroutine func(x,y,n,add_x,add_y,add_xy,add_xx)
implicit none
	integer,intent(in)::n
	real,intent(in),dimension(n,1)::x,y
	real,intent(out)::add_x,add_y,add_xy,add_xx
	
	add_x = sum(x)
	add_y = sum(y)
	add_xy = sum(x*y)
	add_xx = sum(x*x)

end subroutine func

subroutine final_values(n,add_x,add_y,add_xy,add_xx,a0,a1)
implicit none

	real,intent(in)::add_x,add_y,add_xy,add_xx
	integer::n
	real,intent(out)::a0,a1

	a1 = (n*(add_xy) - add_x*add_y)/(n*add_xx - add_x**2)
	a0 = (a1*add_x - add_y)/(-1*n)


end subroutine final_values