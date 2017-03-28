function xdot = twobody3(t,x)
pkg load odepkg;
% these values work, caution when modifying ...
G = 2;
m1 = 2;
m2 = 2;
% declare and initialize 8x1 column storage
xdot = zeros(8,1);
x1=x(1);
u1=x(2);
x2=x(3);
u2=x(4);
y1=x(5);
v1=x(6);
y2=x(7);
v2=x(8);

r = sqrt( (x1-x2)^2 + (y1-y2)^2 );
xdot(1)=x(2);
xdot(2)=G*m2*(x2-x1)/r^3;
xdot(3)=x(4);		 
xdot(4)=G*m1*(x1-x2)/r^3;
xdot(5)=x(6);		 
xdot(6)=G*m2*(y2-y1)/r^3;
xdot(7)=x(8);		 
xdot(8)=G*m1*(y1-y2)/r^3;

end
