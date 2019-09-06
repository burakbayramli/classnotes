function m = TPBVP(p1,p2)
% 16.323 Spring 2007
% Jonathan How
%
global A B x0 b alp
A=[0 1;0 0];
B=[0 1]’;
x0=[10 0]’;
b=p1;
alp=p2;
solinit = bvpinit(linspace(0,1),@TPBVPinit);
sol = bvp4c(@TPBVPode,@TPBVPbc,solinit);
time = sol.y(5)*sol.x;
state = sol.y([1 2],:);
adjoint = sol.y([3 4],:);
control = -(1/b)*sol.y(4,:);
m(1,:) = time;
m([2 3],:) = state;
m([4 5],:) = adjoint;
m(6,:) = control;
%-------------------------------------------------------------------------­
function dydt=TPBVPode(t,y)
global A B x0 b alp
dydt=y(5)*[ A -B*[0 1]/b zeros(2,1); zeros(2,2) -A’ zeros(2,1);zeros(1,5)]*y;

%-------------------------------------------------------------------------­
function res=TPBVPbc(ya,yb)
global A B x0 b alp
res=[ya(1) - x0(1);ya(2)-x0(2);yb(1);yb(2);-0.5*yb(4)^2/b+ alp*yb(5)];
%-------------------------------------------------------------------------­
function v=TPBVPinit(t)
global A B x0 b alp
v=[x0;1;0;1];
return
