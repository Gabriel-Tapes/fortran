program maclaurin
implicit none

integer :: M=10, n
real :: x, aux, seno=0, eps
   
   print *, 'Digite o valor de x: '; read *, x
   print *, 'Digite o valor de Epson: '; read *, eps

   aux = abs(((-1)**M) * ((x**(M+M+1)) / FAT(M+M+1)))
   
   do while (aux >= eps .AND. M < 17)
      M = M + 1

      aux = abs(((-1)**M) * ((x**(M+M+1)) / FAT(M+M+1)))
      print *, 'aux =', aux
      print *, 'M =', M
   end do

   print *, 'M =', M

   do n=0, M
      seno = seno + ((-1)**n) * ((x**(n+n+1)) / FAT(n+n+1))
   end do

   print *, 'Seno:', seno
   print *, 'Sin:', sin(x)

contains
   recursive integer(kind=8) function FAT (x) result (N)
      integer :: x

      if (x == 0) then
         N = 1
      else
         N = x * FAT(x-1)
      end if
   end function

end program maclaurin
