program ResAire
  !Este programa calcularemos el movimiento de un proyectil tomando
  !en cuenta la resistencia del aire.
  
  implicit none
  real, parameter:: g=9.81, pi=3.1415927, rho_a=1.128, dt=0.01, cd =0.45
  real, parameter::theta= 45.0, r= 0.5,  m=0.5
  real, dimension(0:size) ::x, y, t, vx, vy
  integer, parameter:: size=1000
  real::a, f, ct, v0, C
  integer::i, j
  

  !Calcularemos primeramente la velocidad terminal
  
  vt= sqrt((2*m*g)/(rho_a*pi*r**2*cd))
  
  C = m*g / vt 
   
  f=(1-(dt*C)/m)
  
  write (*,*)f
  !Utilizaremos los angulos como radianes; por lo que:
  a = theta * pi / 180.0
  
  !Determinaremos las condiciones iniciales
   
  open(1, file='friccion.dat', status='unknown')  
   
    do j=2,10,2
    
       v0=real(j)
       t(0)=0.
       x(0)=0.
       y(0)=0.
       vx(0)=v0*cos(a)
       vy(0)=v0*sin(a) 
    write(1,1000) t(0), x(0), y(0), vx(0), vy(0)   
    
    t(1)= t(0)+dt

    x(1) = x(0) + v0*t(1)*cos(a)
    y(1) = y(0) + v0*t(1)*sin(a)-0.5*g*t(1)**2
       
    vx(1) = v0*cos(a)
    vy(1) = v0*sin(a)-g*t(1)
     
    write(1,1000) t(1), x(1), y(1),vx(1),vy(1)
     
    do i=2,size

       t(i) = t(i-1) + dt
       
       x(i) = x(i-1) + dt*vx(i-1)
       y(i) = y(i-1) + dt*vy(i-1)
     
       vx(i)= vx(i-1)*(1-(dt*C)/m)
       vy(i)= vy(i-1)*(1-(dt*C)/m)-dt*g
    
       if (y(i)<0.) exit 
      
       write(1,1000) t(i), x(i), y(i),vx(i),vy(i)
1000   format(f18.15,5x,f18.15, 5x, f18.15, 5x, f18.15,5x,f18.15) 
    end do
    write(1,1100)
1100 format(/)
    do i=2,size
       t(i)=0.
       x(i)=0.
       y(i)=0.
       vx(i)=0.
       vy(i)=0.
    end do

 end do
 close(1)
  

end program ResAire

