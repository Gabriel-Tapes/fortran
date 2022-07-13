program crivo_de_eratostenes
implicit none

integer(kind = 4) :: i, k=0, lim
logical, dimension(:), allocatable :: a
real :: n, j

   print *, 'Digite o valor de n: '; read *, n

   allocate(a(2:int(n)))
   lim = int(sqrt(n))

   a = .TRUE.

   do i=2, lim
      if (a(i)) then
         do k=0, int(n)
            j = i*(i+k)
            if (j > n) then
               a(int(n)) = .FALSE.
               exit
            end if
            a(int(j)) = .FALSE.
         end do
      end if
   end do

   do i=2, int(n)
      if (a(i)) then
         print *, i
      end if
   end do

   deallocate(a)

end program crivo_de_eratostenes
