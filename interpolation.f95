program interpolation
implicit none

	real,dimension(4,1)::x,y
	real,dimension(3)::dely
	real,dimension(2)::dely2
	real::x_new
	real::y_new
	real::dely3	
	integer::i
 
	open(10,file = "interpolation_data.txt")

	do i=1,4
		read(10,*) x(i,1) ,y(i,1)
	end do
	close(10)

	call DEL(y,dely,dely2,dely3)

	
		read(*,*) x_new
		call func(x,y,x_new,y_new)
		print*, y_new

end program interpolation


subroutine DEL(y,dely,dely2,dely3)
implicit none
	integer::i
	real,intent(in),dimension(4,1)::y
	real,intent(out),dimension(3)::dely
	real,intent(out),dimension(2)::dely2
	real,intent(out)::dely3

	do i=1,3
		dely(i)=y(i+1,1) - y(i,1)
	end do

	do i=1,2
		dely2(i) = dely(i+1) - dely(i)
	end do
	
		dely3 = dely2(2) - dely2(1)

end subroutine DEL

subroutine func(x,y,x_new,y_new)
implicit none
	real,intent(in),dimension(4,1)::x,y
	real,intent(in)::x_new
	real,intent(out)::y_new
	real,dimension(3)::dely
	real,dimension(2)::dely2
	real::dely3
	real::delx,a

	call DEL(y,dely,dely2,dely3)

	delx = x(2,1) - x(1,1)

	a = (x_new - x(1,1))/delx

	y_new = y(1,1) + dely(1)*a + dely2(1)*(a**2 - a)/2 + dely3*((a**2-a)*(a-2))/6
end subroutine func