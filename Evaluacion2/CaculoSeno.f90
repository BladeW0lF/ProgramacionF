!
!! CaculoSeno.f90
!! 
!! Made by (Eduardo Castillo Bastida)
!! Login   <batman@ltsp165.example.com>
!! 
!! Started on  Fri Dec  1 14:57:30 2017 Eduardo Castillo Bastida
!! Last update Time-stamp: <2017-dic-01.viernes 15:02:06 (batman)>

program CalculoSeno
	double precision, dimension (10000) :: f, x, sen, y
	integer :: i, j, n
	double precision :: fi, fj, term, partial_sum, sign, pot, fact
	

     open (1, file = 'senos.dat', status = 'unknown')
	fi = -3.1d0
	do i=1, 60
	write (1,*) fi, fi
	fi = fi + 0.1d0
	
end do

	write (1,*) ' '
	do n=1, 15, 2
	  fi = -3.1d0
	do i=1, 60
	fi = fi + 0.1d0
	call seno (n, j, fi, fj, sen, sign, pot, fact)
	y(n) = sen(n)
	write (1,*) fi, y(n)

	end do
	write (1,*) ' '
	end do
     close (1)

   end program CalculoSeno

   subroutine seno (n, j, fi, fj, sen, sign, pot, fact)
	integer, intent (in)      :: n
	double precision, intent (in) :: fi
	integer :: j
	double precision, dimension (10000), intent(out) :: sen
	double precision :: fj, term, partial_sum, sign, pot, fact

	
	sign = 1.0d0
	term = fi
	partial_sum = term
	pot = fi
	fact = 1
	do j = 1, n
	 fj = dble(j)
	 pot = fi**(j + 2)
	 fact = fact * (j + 1) * (j + 2)
	 sign = sign * (-1.0d0)
	 term = pot / fact
	 term = term * sign
	 partial_sum = partial_sum + term
	 sen(j) = partial_sum
	 
	end do

	 
end subroutine seno
	 
