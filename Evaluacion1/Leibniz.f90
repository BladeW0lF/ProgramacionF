!
!! Leibniz.f90
!! 
!! Made by (Eduardo Castillo Bastida)
!! Login   <batman@ltsp117.example.com>
!! 
!! Started on  Mon Oct 30 13:39:56 2017 Eduardo Castillo Bastida
!! Last update Time-stamp: <2017-oct-30.lunes 13:47:33 (batman)>
!

Program Liebniz
  ! Este programa calcula el valores del numero pi usando la serie de
  ! Leibniz
  ! declaracion de variables
  implicit none
  integer:: i, n  
  real :: pi, serie

  ! Pregunta por el numero de terminos de la serie
  write(*,*) 'Escribe el valor de n, número de terminos de la serie'
  read (*,*) n

  serie = 0.
  
  do i=0,n

     serie = serie + (((-1)**i)/(2*real(i) + 1))

  end do
  
pi= serie * 4.

  write (*,*) 'cuando n =',n, 'pi=',pi
end program 
