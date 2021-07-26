program main

use variables
implicit none
integer :: i,j,k,nrk,niter,ndel
real*8 :: al,ar,pl,pr,ul,ur,rl,rr,vl,vr,el,er
real*8 :: f1,f2,f3,f4
real*8 :: resr,resu,resv,rese
real*8 :: g1,a,p,cl,cd,cp
real :: start, finish, time

call cpu_time(start)
print *,"Processing..."

call grid

call init(ndel)
niter=ndel
open(10,file='Error.txt')
open(11,file='CPUTime.txt')
open(12,file='Coefficient.txt')

10 continue
do j=1,nj1m
   do i=1,ni1m
      p=(gamma-1.0)*(pv(4,j,i)-0.5*pv(1,j,i)*(pv(2,j,i)*pv(2,j,i)+pv(3,j,i)*pv(3,j,i)))
      a=sqrt(gamma*p/pv(1,j,i))	
      g1=(sqrt(pv(2,j,i)*pv(2,j,i)+pv(3,j,i)*pv(3,j,i))+a)
      dt(j,i)=cfl*vol(j,i)/(g1*(s(2,j,i)+s(4,j,i)+s(3,j,i)+s(1,j,i)))
   enddo
enddo
do nrk=1,3
   call bcpv
   do j=1,nj1m
      do i=1,ni1m
         do k=2,3
            call high(k,i,j,rl,rr,ul,ur,vl,vr,el,er)
            call firs(k,i,j,rl,rr,ul,ur,vl,vr,el,er)
            pl=(gamma-1.0)*(el-0.5*rl*(ul*ul+vl*vl))
            pr=(gamma-1.0)*(er-0.5*rr*(ur*ur+vr*vr))
            al=sqrt(gamma*pl/rl)
            ar=sqrt(gamma*pr/rr)
           
            call hllc(rl,rr,ul,ur,vl,vr,pl,pr,el,er,al,ar,f1,f2,f3,f4)
            if(k.eq.2) then
               f(1,j,i)=f1*s(k,j,i)
               f(2,j,i)=(f2*snx(k,j,i)-f3*sny(k,j,i))*s(k,j,i)
               f(3,j,i)=(f3*snx(k,j,i)+f2*sny(k,j,i))*s(k,j,i)
               f(4,j,i)=f4*s(k,j,i)
            elseif(k.eq.3) then
               g(1,j,i)=f1*s(k,j,i)
               g(2,j,i)=(f2*snx(k,j,i)-f3*sny(k,j,i))*s(k,j,i)
               g(3,j,i)=(f3*snx(k,j,i)+f2*sny(k,j,i))*s(k,j,i)
               g(4,j,i)=f4*s(k,j,i)
            endif
         enddo
      enddo
   enddo
   call bcfx
   call solv(nrk,resr,resu,resv,rese)
enddo
print*,"ITERATION:", n
print'(X,"RESIDUAL ERROR:",2X,"DENSITY ="E30.19,2X,"U-VELOCITY ="E30.19,2X,"V-VELOCITY ="E30.19,2X,"ENERGY ="E30.19)',&
      resr,resu,resv,rese
print*,"============================================================================&
         ==============================================================================================================="
write(10,*) n,resr,resu,resv,rese
n=n+1
if(n.eq.niter) then
   cd=0.0
   cl=0.0
   do i=1,ni1m
      j=1
      p=(gamma-1.0)*(pv(4,j,i)-0.5*pv(1,j,i)*(pv(2,j,i)*pv(2,j,i)+pv(3,j,i)*pv(3,j,i)))
      cd=cd+p*snx(1,j,i)*s(1,j,i)
      cl=cl+p*sny(1,j,i)*s(1,j,i)
      !cp=p
   enddo
   cd=cd/(0.5*r0*(u0*u0+v0*v0))
   cl=cl/(0.5*r0*(u0*u0+v0*v0))
   cp=(p*ptot-p0)/(0.5*r0*(u0*u0+v0*v0))
   write(12,*) x(j,i),cp,cl,cd
   niter=niter+ndel
endif
if(rese.le.error.or.n.eq.nt) then
   call post
   !stop
else
   goto 10
endif
   close(2)
   !stop
call cpu_time(finish)
time=finish-start
print '(X"Time="F10.3," seconds.")',time
write(11,*) time
end program main
