%nahin
pkg load odepkg;
alpha=1;d=2.1;fps=4*pi*pi;
options=odeset('AbsTol',1e-8,'RelTol',1e-5);
tspan=[0 1.8];
uzero=[1;0;0;2*pi];
[t,u]=ode45(@orbit,tspan,uzero,options);
disp(u(:,1));
disp(u(:,3));
