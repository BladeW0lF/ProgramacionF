
 function fx(g) result (x)
	double precision, intent(in) :: g
	double precision 	     :: x
	 x = 1.496d8 * dcos(g)
end function fx
function fy(g) result (y)
	double precision, intent(in) :: g
	double precision 	     :: y
	 y = 1.496d8 * dsin(g)
end function fy

program MCU
	implicit none
	integer :: i
	double precision :: g, fx, fy
	double precision, parameter :: r = 1.496d8, pi=3.141592d0&
      & !Siendo expresado en kilometros
	double precision, dimension(1000) :: x, y

 
open (1, file = 'datos.dat', status = 'unknown')
 do i=1, 360, 1
 g = dble(i)
 g = g * pi / 180.0d0
 x(i) = fx(g)
 y(i) = fy(g)
 write (1,*) x(i), y(i)
 write (1,*) ' '
 end do
 close (1)



end program MCU
   
 
