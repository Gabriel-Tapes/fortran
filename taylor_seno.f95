program taylor

real :: x_g, x, pi
real(kind=8) :: sen

   pi = acos(-1.)
   !print *, 'Digite um angulo x: '; read *, x_g
   
   x =  (pi / 2) + pi
   sen = x - ((x**3)/fat(3)) + ((x**5)/fat(5)) - ((x**7)/fat(7)) + ((x**9)/fat(9))

   print '("seno(x):", 1F15.10, " sin(x):", 1F15.10)',  sen, sin(x)

   contains
      recursive integer function fat(n) result (f)
         integer :: n

         if (n < 0) then
            print *, 'Digite um valor maior que 0!'; stop
         end if

         if (n == 0) then
            f = 1
         else
            f = n * fat(n - 1)
         end if
      end function fat

end program taylor
