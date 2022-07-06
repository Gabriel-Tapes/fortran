program aproximaRaiz
implicit none

integer :: k=0
real :: x, x_novo, a, eps

   print *, 'Digite o valor que deseja aproximar a raiz:)'; read *, a
   print *, 'Digite o valor de erro maximo desejado: '; read *, eps
   
   x = a/2
   x_novo = (x + (a/x))/2

   do while(abs(x_novo - x) > eps)
      x = x_novo
      x_novo = (x + (a/x))/2
      k = k + 1
   end do

   print *, 'Valor aproximado:  ', x_novo
   print *, 'Valor da func sqrt:', sqrt(a)
   print *, 'Numero de iteracoes: ', k

end program aproximaRaiz
