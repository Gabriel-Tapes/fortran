program phi_aprox
implicit none

integer :: n=5
real :: phi, rk, eps=1e-20, aux=1

   phi = (1 + sqrt(5.)) / 2

   do
      rk = fibonacci(n) / fibonacci(n-1)
      aux = abs(phi - rk) / phi

      if (aux < eps) then
         exit
      end if
   
      n = n + 1
   end do

   print '("Phi: ", F30.28)', phi
   print '("rk:  ", F30.28)', rk
   print '("Iteracoes:", I25 )', n
   
contains
   real function fibonacci (n) result (f)
   implicit none

      integer, intent(in) :: n
      integer :: i
      real :: f1, f2
      
      if (n == 0) then
         f = 0
      else if (n == 1) then
         f = 1
      else
         f2 = 0
         f1 = 1

         do i=2, n
            f = f1 + f2
            f2 = f1
            f1 = f
         end do
      end if
   end function
end program phi_aprox
