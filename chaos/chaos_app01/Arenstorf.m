% Gander, {\em Scientific Computing An Introduction using Maple and MATLAB}
% pg 618
function yp=Arenstorf(t,y);
a=0.012277471; b=1-a;
D1=((y(1)+a)^2+y(2)^2)^(3/2);
D2=((y(1)-b)^2+y(2)^2)^(3/2);
yp(1,1)=y(3);
yp(2,1)=y(4);
yp(3,1)=y(1)+2*y(4)-b*(y(1)+a)/D1-a*(y(1)-b)/D2;
yp(4,1)=y(2)-2*y(3)-b*y(2)/D1-a*y(2)/D2;
yp=yp(:);
