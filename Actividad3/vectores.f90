!! vectores.f90
!! 
!! Made by (Eduardo Castillo Bastida)
!! Login   <batman@ltsp21.example.com>
!! 
!! Started on  Mon Sep 25 14:21:00 2017 Eduardo Castillo Bastida
!! Last update Time-stamp: <2017-sep-25.lunes 14:35:51 (batman)>
  !
  program Vector
    implicit none

  ! definimos constantes
    real, parameter :: g = 9.81
    real, parameter :: pi = 3.1415927
  
  ! definimos las variables
    integer::i, k, nps
    real :: a, vi, tv
    real, dimension(20):: t=0.,x=0., y=0.
  

  ! Leer valores para el ángulo a, y la velocidad inicial u desde la terminal
  
    write(*,*) 'MOVIMIENTO PARABÓLICO'
    
    write(*,*) 'DATOS DESPLAZAMIENTO VERTICAL Y HORIZONTAL'
    
    write(*,*) 'Introduzca los valores de  la velocidad inicial (vi) en m&
       &/s y el número de datos'
  
    read(*,*) vi,nps

  !Definimos el ciclo
    cicloenangulo:do k=15,90,15
      ! convirtiendo ángulo a radianes
       a = k * pi / 180.0

      ! las ecuación para el cálculo del tiempo de vuelo
       tv=2*vi*sin(a)/g
       
  ciclodeposicion: do i=0,nps
     t(i)=t(i)+i*(tv/real(nps))
     x(i)=x(i)+vi*t(i)*cos(a)
     y(i)=y(i)+(2*vi*t(i)*sin(a)-g*t(i)**2)/2
  ! output data to a file
     open(1, file='datos.dat', status='unknown')
     write(1,1000) x(i), y(i)
     1000 format(f15.10,5x,f15.10)
  end do ciclodeposicion
  write(1,1100)
  1100 format(/)
 
  do i=0,nps
     t(i)=0
     x(i)=0
     y(i)=0
     end do
  end do cicloenangulo
   close(1)

end program Vector

