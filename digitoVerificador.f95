program digitoVerificador

INTEGER, dimension(11) :: cpf
INTEGER :: d10=0, d11=0, i, aux=0

    PRINT *, 'Digite o seu cpf: '; READ '(11I1)',cpf

    do i=1, 9
         aux = aux + (i * cpf(i))
    end do

    d10 = MOD(aux, 11)
    if (d10 == 0) then
       d10 = 0
    end if
   
    aux = 0
    do i=1, 10
         aux = aux + (i * cpf(i))
    end do

    d11 = MOD(aux, 11)
    if (d11 == 10) then
       d11 = 0
    end if

    if (d10 == cpf(10) .AND. d11 == cpf(11)) then
       print '("Digitos verificadores: ", 2I1)', d10, d11
       print *, 'cpf valido!'
    else
       print *, 'cpf invalido!'
    end if

end program digitoVerificador
