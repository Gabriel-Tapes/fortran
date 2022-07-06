implicit none

integer :: M=1, n, sinal=1
real :: x, aux, seno=0, eps, Pi
   
   print *, 'Digite o valor de x: '; read *, x
   print *, 'Digite o valor de Epson: '; read *, eps

   Pi = acos(-1.)
   x = x * (Pi / 180)

   aux = abs(((-1)**M) * ((x**(M+M+1)) / FAT(M+M+1)))
   
   do while (aux >= eps)
      M = M + 1
      aux = abs(((-1)**M) * ((x**(M+M+1)) / FAT(M+M+1)))
   end do

   do n=0, M
      seno = seno + sinal * ((x**(n+n+1)) / FAT(n+n+1))
      sinal = -sinal
   end do

   print *, 'M:', M
   print *, ''
   print '("Seno(x) = ", 1F23.18)', seno
   print '("Sin(x) = ", 1F24.18)', sin(x)

contains
   recursive real function FAT (x) result (N)
      integer :: x

      if (x == 0) then
         N = 1
      else
         N = x * FAT(x-1)
      end if
   end function

end program maclaurin
