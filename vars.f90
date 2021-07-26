module variables

implicit none

real*8 :: specificCp=1.003E3, specificCv=0.716E3
real*8 :: gamma=1.401, rspecific=287.0530

integer :: n,ni,nj,nt,ni1m,nj1m,ni1p,nj1p
real*8 :: r0,u0,v0,p0,t0,e0,cfl,error
real*8 :: rtot,ptot,ttot,aid,acr,radm,aux,tin,pin,rin,ein
real*8 :: uin,vin,v2
real*8, allocatable :: x(:,:),y(:,:)
real*8, allocatable :: r(:,:),u(:,:),v(:,:),e(:,:)
real*8, allocatable :: pv(:,:,:),cv(:,:,:)
real*8, allocatable :: s(:,:,:),snx(:,:,:),sny(:,:,:),vol(:,:)
real*8, allocatable :: f(:,:,:),g(:,:,:)
real*8, allocatable :: dt(:,:)

end module variables
