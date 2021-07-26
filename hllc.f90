subroutine hllc(rl,rr,ul,ur,vl,vr,pl,pr,el,er,al,ar,f1,f2,f3,f4)

implicit none

real*8 :: sl,sr,al,ar,pl,pr,ul,ur,rl,rr,vl,vr,el,er,s1l,s2l,s1r,s2r
real*8 :: sstar,ustarl1,ustarl2,ustarl3,ustarl4,ustarr1,ustarr2,ustarr3,ustarr4
real*8 :: f1l,f2l,f3l,f4l,f1r,f2r,f3r,f4r,f1,f2,f3,f4

s1l=ul-al
s2l=ur-ar
s1r=ul+al
s2r=ur+ar
sl=min(s1l,s2l)
sr=max(s1r,s2r)

sstar=(pr-pl+rl*ul*(sl-ul)-rr*ur*(sr-ur))/(rl*(sl-ul)-rr*(sr-ur))

ustarl1=rl*(sl-ul)/(sl-sstar)
ustarl2=ustarl1*sstar
ustarl3=ustarl1*vl
ustarl4=ustarl1*(el/rl+(sstar-ul)*(sstar+pl/(rl*(sl-ul))))

ustarr1=rr*(sr-ur)/(sr-sstar)
ustarr2=ustarr1*sstar
ustarr3=ustarr1*vr
ustarr4=ustarr1*(er/rr+(sstar-ur)*(sstar+pr/(rr*(sr-ur))))

f1l=rl*ul
f2l=f1l*ul+pl
f3l=f1l*vl
f4l=ul*(el+pl)

f1r=rr*ur
f2r=f1r*ur+pr
f3r=f1r*vr
f4r=ur*(er+pr)

if(sl.ge.0.0) then
   f1=f1l
   f2=f2l
   f3=f3l
   f4=f4l
elseif(sl.le.0.0.and.sstar.ge.0.0) then
   f1=f1l+sl*(ustarl1-rl)
   f2=f2l+sl*(ustarl2-rl*ul)
   f3=f3l+sl*(ustarl3-rl*vl)
   f4=f4l+sl*(ustarl4-el)
elseif(sstar.le.0.0.and.sr.ge.0.0) then
   f1=f1r+sr*(ustarr1-rr)
   f2=f2r+sr*(ustarr2-rr*ur)
   f3=f3r+sr*(ustarr3-rr*vr)
   f4=f4r+sr*(ustarr4-er)
elseif(sr.le.0.0) then
   f1=f1r
   f2=f2r
   f3=f3r
   f4=f4r
endif

end subroutine hllc
