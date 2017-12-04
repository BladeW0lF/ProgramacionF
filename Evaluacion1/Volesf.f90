program Sphere

! Calcular el volumen de una esfera.
!
! Declarar las variables.
! Constantes pi
! variables= radio al cuadrado y altura

  implicit none    

  integer :: ierr
  character(1) :: yn
  real :: radius, area, vol
  real, parameter :: pi = 3.141592653589793

  interactive_loop: do

!   Le pediremos al usuario por el valor del radio
!   y las leeremos.

    write (*,*) 'Ingrese por favor el radio a utilizar'
    read (*,*,iostat=ierr) radius

!   If radius and height could not be read from input,
!   then cycle through the loop.

    if (ierr /= 0) then
      write(*,*) 'Error, invalid input.'
      cycle interactive_loop
    end if

!   Compute area.  The ** means "raise to a power."

    area = 4*pi * (radius**2)

    vol=(4/3)*pi * (radius**3)

!   Write the input variables (radius, height)
!   and output (area) to the screen.

    write (*,'(1x,a7,f14.2,5x,a7,f14.2,5x,a9,f14.2)') &
         'radius=',radius,'area=',area
    
    write (*,'(1x,a7,f14.2,5x,a7,f14.2,5x,a9,f14.2)') &
         'radius=',radius, 'volume=',vol
        
    

    yn = ' '
    yn_loop: do
      write(*,*) 'Perform another calculation? y[n]'
      read(*,'(a1)') yn
      if (yn=='y' .or. yn=='Y') exit yn_loop
      if (yn=='n' .or. yn=='N' .or. yn==' ') exit interactive_loop
    end do yn_loop

 end do interactive_loop

 

end program Sphere
