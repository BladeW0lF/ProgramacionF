!
!! Taylor.f90
!! 
!! Made by (Eduardo Castillo Bastida)
!! Login   <batman@ltsp165.example.com>
!! 
!! Started on  Fri Dec  1 10:45:16 2017 Eduardo Castillo Bastida
!! Last update Time-stamp: <2017-dic-01.viernes 14:30:54 (batman)>
! ----------- Begin ------------

program Taylor

    double precision, dimension (15) :: f
	integer :: i, j, n
	double precision, dimension (100)   :: x, y, exp_real
	double precision :: fi, fj, term, partial_sum

     open (1, file = 'datos.dat', status = 'unknown')
	
	do n=1, 15, 2
	do i=0, 100, 1
	  fi = dble(i)
	  fi = fi / 10.0d0
	call exptaylor (nmax, j, fi, fj, y)
	exp_real(n) = y(n)
	write(1,*) fi, exp_real(n)

	end do
	write (1,*) ' '
	end do
     close (1)


end program Taylor

!====================================
subroutine exptaylor(nmax, j, fi, fj, y)
!====================================

  ! argumentos de la subrutina:
  integer, intent (in) :: nmax
  double precision, intent (in) :: fi
  integer ::j
  double precision, dimension (100), intent (out) :: y
  double precision ::fj, nterm, partial_sum


    term = 1.
    partial_sum = nterm

    do j=1,nmax
       ! El termino #j es  x**j / j! que es el termino anterior (x10 x/j):
       fj=dble(j)
       nterm = fi/fj   
        ! Sumamos este termino al anterior:
        partial_sum = partial_sum + nterm
        end do
     nterm = j       ! numero de terminos utilizados
     y = partial_sum  ! Este es el valor regresado
end subroutine exptaylor
