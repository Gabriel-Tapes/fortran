program maclaurin
implicit none

integer :: M, n
real :: x, aux, seno=0
   
   print *, 'Digite o valor de x: '; read *, x
   print *, 'Digite o valor de M: '; read *, M

   do n=0, M
      aux = ((-1)**n) * ((x**(n+n+1)) / FAT(n+n+1))
      seno = seno + aux
   end do

   print *, seno

contains
   recursive integer(kind=8) function FAT (x) result (N)
      integer :: x
      
      if (x < 0) print *, 'Digite um valor maior que 0!'

      if (x == 0) then
         N = 1
      else
         N = x * FAT(x-1)
      end if
   end function

end program maclaurin
