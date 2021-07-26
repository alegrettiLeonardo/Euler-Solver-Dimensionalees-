subroutine grid

use variables
implicit none
integer :: i,j,k
real*8 :: s1x,s2x,s3x,s4x
real*8 :: s1y,s2y,s3y,s4y
real*8 :: v11,v12
character*50 :: Mesh

open(1,file='MESH.dat')
read(1,*) ni,nj
ni1m=ni-1
nj1m=nj-1
ni1p=ni+1
nj1p=nj+1
allocate(x(nj,ni),y(nj,ni))
allocate(r(nj,ni),u(nj,ni),v(nj,ni),e(nj,ni))
allocate(pv(4,0:nj1p,0:ni1p),cv(4,nj1m,ni1m))
allocate(s(4,nj1m,ni1m),snx(4,nj1m,ni1m),sny(4,nj1m,ni1m),vol(nj1m,ni1m))
allocate(f(4,0:nj1m,0:ni1m),g(4,0:nj1m,0:ni1m))
allocate(dt(nj1m,ni1m))
do j=1,nj
   do i=1,ni
      read(1,*) x(j,i), y(j,i)
   enddo
enddo
close(1)

Do j=1,nj1m
   Do i=1,ni1m
      s1x=y(j,i+1)-y(j,i)
      s1y=x(j,i)-x(j,i+1)
      s2x=y(j+1,i+1)-y(j,i+1)
      s2y=x(j,i+1)-x(j+1,i+1)
      s3x=y(j+1,i)-y(j+1,i+1)
      s3y=x(j+1,i+1)-x(j+1,i)
      s4x=y(j,i)-y(j+1,i)
      s4y=x(j+1,i)-x(j,i)      
      s(1,j,i)=sqrt(s1x*s1x+s1y*s1y)
      s(2,j,i)=sqrt(s2x*s2x+s2y*s2y)
      s(3,j,i)=sqrt(s3x*s3x+s3y*s3y)
      s(4,j,i)=sqrt(s4x*s4x+s4y*s4y)
      snx(1,j,i)=s1x/s(1,j,i)
      sny(1,j,i)=s1y/s(1,j,i)
      snx(2,j,i)=s2x/s(2,j,i)
      sny(2,j,i)=s2y/s(2,j,i)
      snx(3,j,i)=s3x/s(3,j,i)
      sny(3,j,i)=s3y/s(3,j,i)
      snx(4,j,i)=s4x/s(4,j,i)
      sny(4,j,i)=s4y/s(4,j,i)
      v11=(x(j,i)-x(j+1,i+1))*(y(j,i+1)-y(j+1,i))
      v12=(x(j+1,i)-x(j,i+1))*(y(j,i)-y(j+1,i+1))
      vol(j,i)=0.5*(v11+v12)
   Enddo
Enddo

end subroutine grid
