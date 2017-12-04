program proyectilAltura

implicit none

!Definiendo Constantes
real, parameter :: g = 9.8
real, parameter :: pi=3.1415927

!Definiendo variables

real::a,v,x,t

!Leer los valores en pantalla

write(*,*) 'MOVIMIENTO PARABÓLICO'
write(*,*) 'APLICACIÓN PARA CALCULAR EL ALCANCE MÁXIMO (Xmáx)'
write(*,*) 'Introduzca los datos de velocidad inicial (v) en m/s y el ángulo (a) en grados'
read(*,*) v,a

!Cálculo del ángulo en radianes
a=a*pi/180.0
!Cálculo del alcance máximo

x=(v*v/g)*sin(a)

!Escribiendo los resultados en pantalla
write(*,*) 'Xmáx corresponde a: ',x,'metros '

end program proyectilAltura
