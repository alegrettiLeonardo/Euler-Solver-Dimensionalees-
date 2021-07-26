subroutine bcfx

use variables
implicit none

integer :: i,j,k
real*8 :: f1,f2,f3,f4

do i=1,ni1m
   j=0
   f1=0.0
   f2=(gamma-1.0)*(pv(4,j+1,i)-0.5*pv(1,j+1,i)*(pv(2,j+1,i)*pv(2,j+1,i)+pv(3,j+1,i)*pv(3,j+1,i)))
   f3=0.0
   f4=0.0
   g(1,j,i)=f1*s(1,1,i)                            
   g(2,j,i)=-(f2*snx(1,1,i)-f3*sny(1,1,i))*s(1,1,i) 
   g(3,j,i)=-(f3*snx(1,1,i)+f2*sny(1,1,i))*s(1,1,i) 
   g(4,j,i)=f4*s(1,1,i)                            
enddo

do j=1,nj1m
   i=0
   f(1,j,i)=f(1,j,ni1m)
   f(2,j,i)=f(2,j,ni1m)
   f(3,j,i)=f(3,j,ni1m)
   f(4,j,i)=f(4,j,ni1m)
enddo

end subroutine bcfx
