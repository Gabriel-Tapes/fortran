program arranjos05
implicit none

integer, dimension(5) :: A = (/1,2,4,8,18/)
integer, dimension(0:4) :: B = (/1, 2, 3, 4, 5 /)
integer, dimension(-2:2) :: C
integer i

   C = A+B
   do i=-2, 2
      print '("C(",I3,")=",I2)', i, C(i)
   end do

end program arranjos05
