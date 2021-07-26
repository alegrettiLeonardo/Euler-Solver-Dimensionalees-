subroutine post

use variables
implicit none
integer :: i,j,k
real*8 :: p,cp

call bcpv
pv(1,0,0)=pv(1,0,ni1m)
pv(2,0,0)=pv(2,0,ni1m)
pv(3,0,0)=pv(3,0,ni1m)
pv(4,0,0)=pv(4,0,ni1m)

do j=1,nj
   do i=1,ni
      r(j,i)=0.25*(pv(1,j,i)+pv(1,j,i-1)+pv(1,j-1,i-1)+pv(1,j-1,i))
      u(j,i)=0.25*(pv(2,j,i)+pv(2,j,i-1)+pv(2,j-1,i-1)+pv(2,j-1,i))
      v(j,i)=0.25*(pv(3,j,i)+pv(3,j,i-1)+pv(3,j-1,i-1)+pv(3,j-1,i))
      e(j,i)=0.25*(pv(4,j,i)+pv(4,j,i-1)+pv(4,j-1,i-1)+pv(4,j-1,i))
   enddo
enddo

open(1,file='post.tec')
write(1,*) ' variables= x, y, density, u-vel, v-vel, pressure, entropy'
write(1,*) ' zone t="domain01", i=',ni,', j=',nj,', datapacking=point'
do j=1,nj
   do i=1,ni
      p=(gamma-1.0)*(e(j,i)-0.5*r(j,i)*(u(j,i)*u(j,i)+v(j,i)*v(j,i)))
      write(1,*) x(j,i),y(j,i),r(j,i)*r0,u(j,i)*aid,v(j,i)*aid,p*r0*aid**2,(p*r0*(gamma*rspecific*t0))/((r(i,j)*r0)**gamma)
   enddo
enddo
close(1)
open(1,file='cp.txt')
do i=1,ni
   j=1
   p=(gamma-1.0)*(e(j,i)-0.5*r(j,i)*(u(j,i)*u(j,i)+v(j,i)*v(j,i)))
   cp=2*(p-1/gamma)/((u0*u0+v0*v0)/(gamma*rspecific*t0))
   write(1,*) x(j,i),cp
enddo
close(1)
end subroutine post
