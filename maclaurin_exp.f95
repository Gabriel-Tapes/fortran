program maclaurin_exp
implicit none

integer :: z, k
real :: ex, x, f, eps=1e-14, aux, s

   print *, 'Digite o valor de x:'; read *, x
   
   z = x
   f = x - z
   
   k = 0
   s = 0.0
   
   if (f /= 0.0) then
     do 
     	aux = f ** k / fat(k)

     	if (abs(aux) < eps) then
			exit
     	end if

     	s = s + aux
     	k = k + 1
   	 end do

   else
     s = 1.0
   end if

   ex = eZ(z) * s

   print '("ex =", 1F20.14)', ex
   print '("exp=", 1F20.14)', exp(x)
   
   contains
      real function fat (n) result (f)
         implicit none

		 integer, intent(in) :: n
         integer :: i
         
		 f = 1

         do i=2, n
           f = f*i
         end do

      end function fat

      real function eZ (z) result (r)
         integer :: z, k
         real, parameter :: e = 2.7182818284590452353602874713526624977572470936999595749669676277
         
         r = 1
		
		 if (z /= 0) then
         	do k=1, abs(z)
            	r = r * e
         	end do

            	if (z <0) then
            		r = 1 / r
         		end if
         end if
         
      end function eZ

end program maclaurin_exp
