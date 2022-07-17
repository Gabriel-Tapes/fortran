program pontos_na_reta
implicit none

integer :: n, i
real :: origin, L, dist
real, dimension(:), allocatable :: points

   print *, 'Digite a coordenada da origem da reta: '; read *, origin
   print *, 'Digite o comprimento da reta: '; read *, L
   print *, 'Digite o numero de pontos sobre a reta: '; read *, n

   dist = L / (n + 1)
   allocate(points(1:n))
   
   do i=1, n
      points(i) = dist * i
   end do

   print *, 'Distancia entre os pontos: ', dist
   print '(F5.2)', points

   deallocate(points)

end program pontos_na_reta
