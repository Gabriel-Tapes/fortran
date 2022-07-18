program arranjos02
implicit none

integer, dimension(4) :: A, B, C

   A = (/1, 2, 3, 4/)
   B = (/5, 6, 7, 8/)
   C = A * B

   print '(4I4)', C

end program arranjos02
