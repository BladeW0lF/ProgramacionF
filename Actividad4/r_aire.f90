program Resistans                                                      
  use parametros  
  implicit none
  real :: vt,v0,m,r

  ! Leer valores para la velocidad inicial desde la terminal
  
  print*, 'Movimiento parabolico con resistencia de aire'
  print*, 'con trayectoria a funcion del angulo'
  write(*,*) 'Por Favor: introduzca los valores de  la velocidad inicial (v) en m/s'
  read(*,*)   v0
  write(*,*) 'Introduzca la masa del proyectil en kg'
  read(*,*)   m
  write(*,*) 'Introduzca el radio del proyectil en metros'
  read(*,*)   r
  call v_terminal(vt)
  call friccion(vt, v0, fy, fx, x, a,  y, t)
  
end program Resistencia
 module parametros 
   implicit none

   !Calcularemos la velocidad terminal usando estas variables 
   !                                                                                                 *
   !     h -------------- incremento del tiempo                                                      *
   !     rho_a ----------Densidad del aire kg/m**3                                                   *
   !     size -----------Tamaño del array                                                            *
   !     g --------------Aceleración gravitacional                                                   *
   !     C --------------Constante de arrastre                                                    

  !definimos parametros y variables 
  real, parameter :: g = 9.81, pi = 3.1415927, rho_a=1.128, C=0.45, h=0.01
  integer:: size = 100
    
end module parametros

*********************************************
  subroutine v_terminal(vt)
  use parametros 
  implicit none 
  real, intent(in)::m,r
  real, intent(out):: vt
  !Calculo de la velocidad terminal
  
  vt= sqrt((2*m*g)/(rho_a*pi*r**2*C))

end subroutine v_terminal

***********************************************
  subroutine friccion(a,vt,v0,x,y,t,fx,fy)

    
   !     v0--------------Velocidad inicial del proyectil                                             *
   !     vt -------------Velocidad terminal del proyectil vt = (d**2 *g)*( rho_p-rho_a)/18*mu        *
   !     a -------------- Angulo del proyectil                                                       *
   !     h --------------Incremento del tiempo                                                       *
   !     x_t y y_t ------Desplazamientos horizontal y vertical                                       *
   !     fx y  fy------Velocidades horizontales y verticales f_x = dx(t)/dt y f_y =dy(t)           
  use parametros 
  implicit none
  real, intent(in) :: v0
  real :: a,vt
  real, dimension(size):: t, fx, fy, x, y
  integer::i,j

  !Definimoos el el ciclo de angulo
   do j = 0,90,15

  ! Utilizaremos el angulo en radianes
     a = j * pi / 180.0
  
    
  !Definimos nuestro ciclo de posicion 
   do i = 0,size
     fx(0) =v0*cos(a)
     fy(0) =v0*sin(a)
     x(0)=0.
     y(0)=0.
     t(0)=0.   

     t(i+1)=t(i)+h
     x(i+1)=x(i)+h*fx(i)
     y(i+1)=y(i)+h*fy(i)

     fx(i+1) = fx(i) + (2*vt/(g*t(i+1)**2))*(v0*cos(a)-x(i+1))
     fy(i+1) = fy(i) + (2*vt/(g*t(i+1)**2))*(v0*sin(a)-y(i+1))-vt
     
     if (y(i)<0) exit 
   !Escribiendo datos 
     open(1, file='r_aire.dat', status='unknown')
     write(1,1000) x(i+1), y(i+1)
     1000 format(f18.10,5x,f18.10)
  end do 
  write(1,1100)
  1100 format(/)
 
  do i=0,size
     t(i)=0.
     x(i)=0.
     y(i)=0.
     fx(i)=0.
     fy(i)=0.
     end do
  end do 
   close(1)
end subroutine 

