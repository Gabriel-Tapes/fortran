program crivo_de_eratostenes
implicit none

integer :: i, j, k, lim
logical, dimension(:), allocatable :: a
real :: n

   print *, 'Digite o valor de n: '; read *, n

   allocate(a(2:int(n)))
   lim = int(sqrt(n))

   a = .TRUE.

   do i=2, lim
      if (a(i)) then
         do k=0, int(n)
            j = i*(i+k)
            if (j > n) then
               j = int(n)
            end if
            a(j) = .FALSE.
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
