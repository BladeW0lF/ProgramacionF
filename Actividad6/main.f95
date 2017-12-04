function solx(asol) result (x)
	double precision, intent(in) :: asol
	double precision 	     :: x
        double precision, parameter :: rsol = 1.496d8
	 x = rsol * dcos(asol)
end function solx
function soly(asol) result (y)
	double precision, intent(in) :: asol
	double precision 	     :: y
	double precision, parameter :: rsol = 1.496d8
	 y = rsol * dsin(asol)
end function soly

subroutine moon(rsol, rluna, posx, posy, alun, asol)
   double precision, intent (in) :: rsol, alun, asol
   double precision, intent (out) :: posx, posy
   double precision :: rluna
   rluna = rsol / 4.0d0
   posx = (rsol * dcos(asol)) +(rluna * dcos(alun))
   posy = (rsol * dsin(asol)) +(rluna * dsin(alun))

end subroutine moon


program luna
	implicit none
	integer :: i
	double precision :: g, dia, rsol, rluna, posx, posy, alun
	double precision :: rad, velluna, velsol, solx, soly, asol
	double precision, parameter :: pi=3.1416d0, month = 27.3217d0, year = 365.26d0
	double precision, dimension(360) :: totx,toty
	double precision, dimension(360) :: x, y
  rsol = 1.496d8
  rad = pi / 180.0d0
  dia = 365.26d0/(360.0d0*rad) !para saber cuantos dias pasan por radian
  velluna = 2.0d0 * (pi / month) !Es lo que recorre diariamente la luna en radianes
  velsol = 2.0d0 * (pi / year)

open (1, file = 'LunaVtierra.dat', status = 'unknown')
open (2, file = 'TierrAsol.dat', status = 'unknown')
 do i=1, 360, 1
 g = dble(i)
 asol = g * velsol
 alun = g * velluna  !para saber la posicion actual en radianes
 x(i) = solx(asol)
 y(i) = soly(asol) !Las posiciones dadas por la funcion, para la posicion de la tierra respecto al sol
 call moon(rsol, rluna, posx, posy, alun, asol)  !para calcular la posicion de la luna respecto a la tierra y el sol
 totx(i) = posx
 toty(i) = posy
 write (1,*) totx(i), toty(i)
 write (1,*) ' '
 write (2,*) x(i), y(i)
 write (2,*) ' '

 end do
 close (1)
 close (2)
end program luna
