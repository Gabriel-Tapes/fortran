program intercala_arranjos
implicit none

integer :: m, n, i=1, j=1, k=1
integer, dimension(:), allocatable :: A, B, C

	print *, 'Digite o tamanho do arranjo A:'; read *, m
    allocate(A(m))
    
    print *, 'Digite o tamanho do arranjo B:'; read *, n
    allocate(B(n))

    print *, 'Digite os elementos do arranjo A:'; read *, A
	call ordena_arranjo(A)
    
    print *, 'Digite os elementos do arranjo B:'; read *, B
    call ordena_arranjo(B)

    allocate(C(m+n))

    do while (k <= m+n)
		if (i > m) then

        	do while (j <= n)
            	C(k) = B(j); j = j+1; k=k+1
            end do

        elseif (j>n) then
        
			do while (i<=n)
            	C(k) = A(i); i = i + 1; k = k + 1
            end do
            
        elseif (A(i) <= B(j)) then
			C(k) = A(i); i = i + 1; k = k + 1
        else
          	C(k) = B(j); j = j + 1; k = k + 1
        end if
    end do
	
    print *, C

    contains
    	subroutine ordena_arranjo (A)
		implicit none
			
			integer :: i, j, max, fim, aux
            integer, dimension(:) :: A
			
			max = size(A)
            fim = max - 1
            do i=1, max
              do j=1, fim
                if (A(j) > A(j+1)) then
                  aux = A(j)
                  A(j) = A(j+1)
                  A(j+1) = aux
                else
                  fim = j
                end if
              end do
            end do
			
        end subroutine ordena_arranjo

end program intercala_arranjos