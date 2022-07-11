program maclaurin_exp
implicit none

integer :: z, M=0, k
real :: ex, x, f, i, eps=1e-16, aux

   print *, 'Digite o valor de x:'; read *, x
   
   z = x
   f = x - z
   
   aux = 1

   do while (aux > eps)
      aux = (f ** M) / (fat(M))
      M = M + 1
   end do
   print *, 'M:', M
   
   aux = 0
   do k=0, M
      aux = aux + ((f**k) / fat(k))
   end do

   ex = eZ(z) * aux

   print *, 'ex =', ex
   print *, 'exp=', exp(x)
   
   contains
      recursive real function fat (n) result (f)
         integer :: n
          
         if (n == 0) then
            f = 1
         else
            f = n * fat(n-1)
         end if

      end function fat

      real function eZ (z) result (r)
         integer :: z, k
         real :: e
         
         e = exp(1.)
         r = 1

         if (z > 0) then
            do k=1, z
              r = r * e
            end do
         else
            do k=1, abs(z)
               r = r * e
            end do
            r = 1 / r
         end if
         
      end function eZ

end program maclaurin_exp
