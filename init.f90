subroutine init(ndel)

	use variables
	implicit none
	integer :: i,j,k,ndel
	real*8 :: avenxj,avenyj,avenxi,avenyi,avesi,avesj,g1,g2,a
	real*8 :: pi,m0,aoa,c0

	open(1,file='initial.txt')
	read(1,*) m0 		! Mach number
	read(1,*) aoa		! Angle of attack
	read(1,*) r0		! Air density
	read(1,*) p0		! Ambient pressure 	
	read(1,*) t0		! Ambient temperature
	read(1,*) cfl		! CFL
	read(1,*) error		! Error tolerance
	read(1,*) nt		! Iterations
	read(1,*) ndel		! Delta iterations
	close(1)
	pi=4.0*atan(1.0_8)
	aid=sqrt(gamma*rspecific*t0)
	u0=m0*aid*cos(aoa*pi/180.0)
	v0=m0*aid*sin(aoa*pi/180.0)

!***************************** Dimensionless Variables ***********************!
	! critical speed of sound
	!ptot = p0
	!ttot = t0
	!rtot = r0
	!acr = sqrt(2*gamma*(gamma-1)/(gamma+1)*specificCv*ttot)
	!uin = u0/acr
	!vin = v0/acr 
	!aux = (1-(gamma-1)/(gamma+1)*(1+tan(aoa)**2)*(uin/acr)**2)	
	!tin = (ttot*aux)/ttot
	!pin = (ptot*aux**(gamma/(gamma-1)))/(rtot*acr**2)
	!rin = (rtot*aux**(1/(gamma-1)))/rtot
	!ein = (ptot/(gamma-1) + 0.5*rtot*(uin**2 + vin**2))/(rtot*acr**2)	

	! speed of sound
	ptot = p0
	ttot = T0
	rtot = r0 !ptot/(rspecific*ttot)
	e0 = rtot*((1.0/(gamma-1))*rspecific*ttot + 0.5*(u0**2+v0**2))
	uin = u0/aid
	vin = v0/aid
	!aux = (1-(gamma-1)/(gamma+1)*(1+tan(aoa)**2)*(uin/aid)**2)
	!tin = (ttot*aux)/ttot
	tin = ttot/ttot
	pin = ptot/(rtot*(aid**2))
	!rin = (rtot*aux**(1/(gamma-1)))/rtot
	rin = rtot/rtot
	ein = e0/(rtot*aid**2)

!******************************************************************************!
	Open(10,file='initial condition.txt')
	Write(10,*)"rhoin = ", rin
	Write(10,*)"uin = ", uin
	Write(10,*)"vin = ", vin
	Write(10,*)"Mach = ", m0
	Write(10,*)"ein = ", ein
	Write(10,*)"Tin = ", tin
	Write(10,*)"pin = ", pin
	Write(10,*)"aid = ", aid
	Write(10,*)"aux = ", aux
	Write(10,*)"AoA = ", aoa
	Close(10)

	Print*,"rin = ", rin
	Print*,"uin = ", uin
	Print*,"vin = ", vin
	Print*,"ein = ", ein
	Print*,"tin = ", tin
	Print*,"pin = ", pin
	Print*,"aid = ", aid
	!Print*,"acr = ", acr
	Print*,"AoA = ", aoa
	
	do j=1,nj1m
   		do i=1,ni1m
		!      r(j,i)=r0
		!      u(j,i)=u0
		!      v(j,i)=v0
		!      e(j,i)=0.5*r0*(u0*u0+v0*v0)+p0/(gamma-1.0)
		!      cv(1,j,i)=r0 
		!      cv(2,j,i)=r0*u0 
		!      cv(3,j,i)=r0*v0 
		!      cv(4,j,i)=e(j,i) 
		!      pv(1,j,i)=r0
		!      pv(2,j,i)=u0 
		!      pv(3,j,i)=v0
		!      pv(4,j,i)=e(j,i)
			r(j,i)=rin
			u(j,i)=uin
			v(j,i)=vin
			e(j,i)=ein
			cv(1,j,i)=rin 
			cv(2,j,i)=rin*uin 
			cv(3,j,i)=rin*vin 
			cv(4,j,i)=e(j,i) 
			pv(1,j,i)=rin
			pv(2,j,i)=uin 
			pv(3,j,i)=vin
			pv(4,j,i)=e(j,i)  
   		enddo
	enddo

end subroutine init
