subroutine gaussElimination(a)
real, dimension(3,4), intent(out):: a
real, dimension(1,4) :: dummy
real:: pivot, factor, max =0
integer :: pivotColumn, pivotRow, rowCount, columnCount, step,i, index

max= 0
do step = 1,2,1
if(a(step,step) .eq. 0) then
do i=step+1,3
if(a(i,step) .ge. max) then
max = a(i, step)
index = i
end if
end do
dummy(1, 1:4:1) = a(index, 1:4:1)
a(index, 1:4:1) = a(step, 1:4:1)
a(step,1:4:1) = dummy(1,1:4:1)
end if

pivot = a(step, step)
pivotRow = step
pivotColumn = step

do rowCount = pivotRow+1, 3
factor = a(rowCount, pivotColumn)/pivot
if(factor .ne. 0) then
do columnCount = pivotColumn, 4
a(rowCount,columnCount) = a(rowCount,columnCount)-factor*a(pivotRow, columnCount)
end do
end if

end do
end do
end subroutine

subroutine backSubstitution(a,x)
implicit none
real, dimension(3,4), intent(in) :: a
real,dimension(3), intent(out) :: x
integer :: rowCount, columnCount
real :: summation
summation = 0
x(3) = a(3,4)/a(3,3)

do rowCount = 2,1,-1
do columnCount = 3,rowCount+1,-1
summation = summation +a(rowCount,columnCount) * x(columnCount)

end do
x(rowCount) = (a(rowCount,4) -summation)/a(rowCount,rowCount)

summation = 0
end do
end subroutine backSubstitution

program main
real, dimension(3,4) :: a
real, dimension(3) :: x
integer :: i,j
a(1,1) = 1
a(1,2) = -1
a(1,3) = 3
a(1,4) = -8
a(2,1) = 2
a(2,2) = -2
a(2,3) = 3
a(2,4) = -20
a(3,1) = 1
a(3,2) = 1
a(3,3) = 1
a(3,4) = -2

call gaussElimination(a)

call backSubstitution(a,x)
do i=1,3
print *, x(i)
end do
end program