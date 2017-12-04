program projectile
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927

  ! definimos las variables
  real :: a, t, v

  ! Leer valores para el ángulo a, y la velocidad inicial u desde la terminal
  
  write(*,*) 'MOVIMIENTO PARABÓLICO'
  write(*,*) 'APLICACIÓN PARA CALCULAR EL TIEMPO DE VUELO'
  write(*,*) 'Introduzca los valores de ángulo (a) en grados y la velocidad inicial (v) en m/s'
  read(*,*) a, v

  ! convirtiendo ángulo a radianes
  a = a * pi / 180.0
  
  ! las ecuación para el cálculo del tiempo de vuelo

  t=2*v*sin(a)/g
   
 ! escribiendo el resultado en la pantalla
  write(*,*) 'El tiempo de vuelo es: ', t
  

end program projectile
