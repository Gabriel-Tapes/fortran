program arranjos01
implicit none

real, dimension(4) :: X = (/1.0, 3.0, 4.0, 10.0/)

   X = X * (-5)

   print '(4F8.2)', X

   print '("Soma:", 1F8.2, /, "Produto:" 1F10.2)', sum(X), product(X)

end program arranjos01
