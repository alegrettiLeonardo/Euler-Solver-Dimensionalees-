subroutine high(k,i,j,rl,rr,ul,ur,vl,vr,el,er)

use variables
implicit none
integer :: i,j,k,l
real*8 :: eps
real*8 :: al,ar,bl,br,dell,delr
real*8 :: rl,rr,ul,ur,vl,vr,el,er
real*8 :: u2l,u2r,v2l,v2r,u3l,u3r,v3l,v3r

eps=0.0001*sqrt(vol(i,j)) 
!eps=0.00001*(vol(j,i)**(1.0/3.0)) 
!eps=sqrt(vol(i,j))
if(k.eq.2) Then	
   do l=1,4
      aL=pv(l,j,i+1)-pv(l,j,i)
      bL=pv(l,j,i)-pv(l,j,i-1)
      delL=(aL*(bL*bL+eps)+bL*(aL*aL+eps))/(aL*aL+bL*bL+2.0*eps)
      if(l.eq.1) rl= pv(l,j,i)+0.5*delL
      if(l.eq.2) u2l=pv(l,j,i)+0.5*delL
      if(l.eq.3) v2l=pv(l,j,i)+0.5*delL
      if(l.eq.4) el= pv(l,j,i)+0.5*delL
      aR=pv(l,j,i+2)-pv(l,j,i+1)
      bR=pv(l,j,i+1)-pv(l,j,i)
      delR=(aR*(bR*bR+eps)+bR*(aR*aR+eps))/(aR*aR+bR*bR+2.0*eps)
      if(l.eq.1) rr= pv(l,j,i+1)-0.5*delR  
      if(l.eq.2) u2r=pv(l,j,i+1)-0.5*delR 
      if(l.eq.3) v2r=pv(l,j,i+1)-0.5*delR
      if(l.eq.4) er= pv(l,j,i+1)-0.5*delR	
   enddo
   ul=snx(k,j,i)*u2l+sny(k,j,i)*v2l
   vl=snx(k,j,i)*v2l-sny(k,j,i)*u2l
   ur=snx(k,j,i)*u2r+sny(k,j,i)*v2r
   vr=snx(k,j,i)*v2r-sny(k,j,i)*u2r
Elseif(k.eq.3) Then
   do l=1,4
      aL=pv(l,j+1,i)-pv(l,j,i)
      bL=pv(l,j,i)-pv(l,j-1,i)
      delL=(aL*(bL*bL+eps)+bL*(aL*aL+eps))/(aL*aL+bL*bL+2.0*eps)
      if(l.eq.1) rl= pv(l,j,i)+0.5*delL
      if(l.eq.2) u3l=pv(l,j,i)+0.5*delL
      if(l.eq.3) v3l=pv(l,j,i)+0.5*delL
      if(l.eq.4) el= pv(l,j,i)+0.5*delL

if(j.eq.1) then
      if(l.eq.1) rl= pv(l,j,i)
      if(l.eq.2) u3l=pv(l,j,i)
      if(l.eq.3) v3l=pv(l,j,i)
      if(l.eq.4) el= pv(l,j,i)
endif
      aR=pv(l,j+2,i)-pv(l,j+1,i)
      bR=pv(l,j+1,i)-pv(l,j,i)
      delR=(aR*(bR*bR+eps)+bR*(aR*aR+eps))/(aR*aR+bR*bR+2.0*eps)
      if(l.eq.1) rr= pv(l,j+1,i)-0.5*delR
      if(l.eq.2) u3r=pv(l,j+1,i)-0.5*delR 
      if(l.eq.3) v3r=pv(l,j+1,i)-0.5*delR 
      if(l.eq.4) er= pv(l,j+1,i)-0.5*delR	 
   enddo
   ul=snx(k,j,i)*u3l+sny(k,j,i)*v3l
   vl=snx(k,j,i)*v3l-sny(k,j,i)*u3l	 
   ur=snx(k,j,i)*u3r+sny(k,j,i)*v3r
   vr=snx(k,j,i)*v3r-sny(k,j,i)*u3r
Endif

end subroutine high
