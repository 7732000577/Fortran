program gauss_elim
implicit none

	real,dimension(3,4)::a
	real,dimension(3,1)::x 
	a(1,1) = 2
	a(1,2) = 1
	a(1,3) = 0
	a(1,4) = 1
	a(2,1) = 1
	a(2,2) = 2
	a(2,3) = 1
	a(2,4) = 2
	a(3,1) = 0
	a(3,2) = 1
	a(3,3) = 1
	a(3,4) = 4
	call elimination(a)
	call substitution(a,x)

	print*,x

end program gauss_elim 



subroutine elimination(a)
implicit none

	real,intent(inout),dimension(3,4)::a
	!real,intent(out),dimension(3,1)::g_a
	integer::i,j,k
	real::x,y

	do i=1,2
		x = a(i,i)
		do j=i+1,3
			y = a(j,i)/x
		do k = i,4
			a(j,k) = a(j,k) - y*a(i,k)
		end do
		end do
	end do
end subroutine elimination



subroutine substitution(a,x)
implicit none
	
	real,intent(in),dimension(3,4)::a
	real,intent(out),dimension(3,1)::x

	x(3,1) = a(3,4)/a(3,3)
	x(2,1) = (a(2,4) - (x(3,1)*a(2,3)))/a(2,2)
	x(1,1) = (a(1,4) - (x(3,1)*a(1,3) + x(2,1)*a(1,2)))/a(1,1)

end subroutine substitution