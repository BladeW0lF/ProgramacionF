program projectile
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927

  ! definimos las variables
  real :: a, t, u, x, y
  real :: theta, v, vx, vy

  ! Leer valores para el ángulo a, el tiempo t, y la velocidad inicial u desde la terminal
  write(*,*) 'APLICACIÓN PARA DESCRIBIR UN MOVIMIENTO PARABÓLICO'
  write(*,*) 'Introduzca los valores de ángulo (a) en grados  y velocidad inicial (v) en m/s'
  read(*,*) a, u

  ! convirtiendo ángulo a radianes
  a = a * pi / 180.0
  
  ! las ecuaciones de la posición en x y y
  t=2*u*sin(a)/g
  x = u * cos(a) * t
  y = u*u * sin(a)*sin(a)/(2*g)

  ! La velocidad al tiempo t
  vx = u * cos(a)
  vy = u * sin(a) - g * t
  v = sqrt(vx * vx + vy * vy)
  theta = atan(vy / vx) * 180.0 / pi
 
 ! escribiendo el resultado en la pantalla
  write(*,*) 'tv:',t,'Xmax: ',x,'  Ymax: ',y
  write(*,*) 'Vt: ',v,'  theta: ',theta

end program projectile
