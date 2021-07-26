subroutine bcpv

use variables
implicit none
integer :: i,j,k

do i=1,ni1m
   j=0
   pv(1,j,i)=pv(1,j+1,i)
   pv(2,j,i)=pv(2,j+1,i) 
   pv(3,j,i)=pv(3,j+1,i) 
   pv(4,j,i)=pv(4,j+1,i)
   do j=nj,nj1p
      pv(1,j,i)=pv(1,nj1m,i)
      pv(2,j,i)=pv(2,nj1m,i) 
      pv(3,j,i)=pv(3,nj1m,i) 
      pv(4,j,i)=pv(4,nj1m,i)
   enddo
enddo

do j=1,nj1m
   i=0
   pv(1,j,i)=pv(1,j,ni1m)
   pv(2,j,i)=pv(2,j,ni1m)
   pv(3,j,i)=pv(3,j,ni1m)
   pv(4,j,i)=pv(4,j,ni1m)
   i=ni
   pv(1,j,i)=pv(1,j,1)
   pv(2,j,i)=pv(2,j,1)
   pv(3,j,i)=pv(3,j,1)
   pv(4,j,i)=pv(4,j,1)
   i=ni1p
   pv(1,j,i)=pv(1,j,2)
   pv(2,j,i)=pv(2,j,2)
   pv(3,j,i)=pv(3,j,2)
   pv(4,j,i)=pv(4,j,2)
enddo

end subroutine bcpv
