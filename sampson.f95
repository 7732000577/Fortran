subroutine cal_theta(r,z,theta)
implicit none
	real,intent(in),dimension(5)::r
	real,intent(in)::z
	real,intent(out),dimension(5)::theta

	theta = -4*z - r**2 + r**4/4.0 + 7.0/24.0

end subroutine cal_theta

subroutine cal_y(theta,r,y)
implicit none
	real,intent(in),dimension(5)::theta,r
	real,intent(out),dimension(5)::y

	y = 4*theta*(1 - r**2)*r

end subroutine cal_y

subroutine integrate(y,delta,integral)
implicit none
	real,intent(in),dimension(5)::y
	real,intent(out)::integral
	real,intent(in)::delta

	integral = (delta/3.0)*(y(1) + 4*y(2) + 2*y(3) + 4*y(4) + y(5))


end subroutine integrate

program func
implicit none

	real::z,integral,delta
	real,dimension(5)::theta,y,r
	integer::i

	z = 0.5
	delta = 0.25
	do i=1,5
		r(i) = (i-1)*0.25
	end do


	call cal_theta(r,z,theta)
	call cal_y(theta,r,y)
	call integrate(y,delta,integral)

	print*,integral


end program func