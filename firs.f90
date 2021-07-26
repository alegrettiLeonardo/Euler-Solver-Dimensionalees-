subroutine firs(k,i,j,rl,rr,ul,ur,vl,vr,el,er)

use variables
implicit none

integer :: i,j,k
real*8 :: rl,rr,ul,ur,vl,vr,el,er
real*8 :: u2l,u2r,v2l,v2r,u3l,u3r,v3l,v3r

if(k.eq.2) Then	
   rl=pv(1,j,i)
   rr=pv(1,j,i+1)
   u2l=pv(2,j,i)
   u2r=pv(2,j,i+1)
   v2l=pv(3,j,i)
   v2r=pv(3,j,i+1)
   el=pv(4,j,i)
   er=pv(4,j,i+1)
   ul=snx(k,j,i)*u2l+sny(k,j,i)*v2l
   vl=snx(k,j,i)*v2l-sny(k,j,i)*u2l
   ur=snx(k,j,i)*u2r+sny(k,j,i)*v2r
   vr=snx(k,j,i)*v2r-sny(k,j,i)*u2r
Elseif(k.eq.3) Then
   rl=pv(1,j,i)
   rr=pv(1,j+1,i)
   u3l=pv(2,j,i)
   u3r=pv(2,j+1,i)
   v3l=pv(3,j,i)
   v3r=pv(3,j+1,i)
   el=pv(4,j,i)
   er=pv(4,j+1,i)
   ul=snx(k,j,i)*u3l+sny(k,j,i)*v3l
   vl=snx(k,j,i)*v3l-sny(k,j,i)*u3l	 
   ur=snx(k,j,i)*u3r+sny(k,j,i)*v3r
   vr=snx(k,j,i)*v3r-sny(k,j,i)*u3r
Endif

end subroutine firs
