subroutine solv(nrk,resr,resu,resv,rese)

use variables
implicit none
integer :: i,j,k,nrk
real*8 :: rsum,usum,vsum,esum,r1,u1,v1,e1,resr,resu,resv,rese 
real*8 :: cv1,cv2,cv3,cv4
real*8 :: sigma1,sigma2,sigma3

if(nrk.eq.1) then
   sigma1=0.1481
   do j=1,nj1m
      do i=1,ni1m
         cv1=cv(1,j,i)-sigma1*dt(j,i)*(f(1,j,i)-f(1,j,i-1)+g(1,j,i)-g(1,j-1,i))/vol(j,i)
         cv2=cv(2,j,i)-sigma1*dt(j,i)*(f(2,j,i)-f(2,j,i-1)+g(2,j,i)-g(2,j-1,i))/vol(j,i)
	      cv3=cv(3,j,i)-sigma1*dt(j,i)*(f(3,j,i)-f(3,j,i-1)+g(3,j,i)-g(3,j-1,i))/vol(j,i)
         cv4=cv(4,j,i)-sigma1*dt(j,i)*(f(4,j,i)-f(4,j,i-1)+g(4,j,i)-g(4,j-1,i))/vol(j,i)
         pv(1,j,i)=cv1
	      pv(2,j,i)=cv2/cv1
	      pv(3,j,i)=cv3/cv1
	      pv(4,j,i)=cv4
      Enddo
   enddo
ElseIf(nrk.eq.2) Then                          
   sigma2=0.40
   do j=1,nj1m
      do i=1,ni1m      
         cv1=cv(1,j,i)-sigma2*dt(j,i)*(f(1,j,i)-f(1,j,i-1)+g(1,j,i)-g(1,j-1,i))/vol(j,i)
	      cv2=cv(2,j,i)-sigma2*dt(j,i)*(f(2,j,i)-f(2,j,i-1)+g(2,j,i)-g(2,j-1,i))/vol(j,i)
	      cv3=cv(3,j,i)-sigma2*dt(j,i)*(f(3,j,i)-f(3,j,i-1)+g(3,j,i)-g(3,j-1,i))/vol(j,i)
	      cv4=cv(4,j,i)-sigma2*dt(j,i)*(f(4,j,i)-f(4,j,i-1)+g(4,j,i)-g(4,j-1,i))/vol(j,i)
         pv(1,j,i)=cv1
	      pv(2,j,i)=cv2/cv1
	      pv(3,j,i)=cv3/cv1			
	      pv(4,j,i)=cv4
      Enddo
   enddo
elseIf(nrk.eq.3) then
   sigma3=0.6
   rsum=0.0
   usum=0.0
   vsum=0.0
   esum=0.0                   
   do j=1,nj1m
      do i=1,ni1m         
         cv1=cv(1,j,i)-sigma3*dt(j,i)*(f(1,j,i)-f(1,j,i-1)+g(1,j,i)-g(1,j-1,i))/vol(j,i)
         cv2=cv(2,j,i)-sigma3*dt(j,i)*(f(2,j,i)-f(2,j,i-1)+g(2,j,i)-g(2,j-1,i))/vol(j,i)
	      cv3=cv(3,j,i)-sigma3*dt(j,i)*(f(3,j,i)-f(3,j,i-1)+g(3,j,i)-g(3,j-1,i))/vol(j,i)
	      cv4=cv(4,j,i)-sigma3*dt(j,i)*(f(4,j,i)-f(4,j,i-1)+g(4,j,i)-g(4,j-1,i))/vol(j,i)
         pv(1,j,i)=cv1			   
	      pv(2,j,i)=cv2/cv1
	      pv(3,j,i)=cv3/cv1		
	      pv(4,j,i)=cv4
         rsum=rsum+abs(cv(1,j,i)-pv(1,j,i))
         usum=usum+abs(cv(2,j,i)/cv(1,j,i)-pv(2,j,i))
	      vsum=vsum+abs(cv(3,j,i)/cv(1,j,i)-pv(3,j,i))
         esum=esum+abs(cv(4,j,i)-pv(4,j,i))
         cv(1,j,i)=cv1
	      cv(2,j,i)=cv2
	      cv(3,j,i)=cv3
	      cv(4,j,i)=cv4
      Enddo
   enddo
Endif
resr=rsum/((ni-1)*(nj-1))
resu=usum/((ni-1)*(nj-1))
resv=vsum/((ni-1)*(nj-1))
rese=esum/((ni-1)*(nj-1))
end subroutine solv