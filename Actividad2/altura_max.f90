program proyectilAltura

implicit none

!Definiendo Constantes
real, parameter :: g = 9.8
real, parameter :: pi=3.1415927

!Definiendo variables

real::a,v,y

!Leer los valores en pantalla

write(*,*) 'MOVIMIENTO PARABÓLICO'
write(*,*) 'APLICACIÓN PARA CALCULAR LA ALTURA MÁXIMO (Ymáx)'
write(*,*) 'Introduzca los datos de velocidad inicial (v) en m/s y el ángulo (a) en grados'
read(*,*) v,a

!Cálculo del angulo en radianes
a=a*pi/180.0
!Cálculo del alcance máximo

y=v*v*sin(a)*sin(a)/(2*g)

!Escribiendo los resultados en pantalla
write(*,*) 'Xmáx corresponde a: ',y,'metros '

end program proyectilAltura
