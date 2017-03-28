pkg load odepkg;
[t,x]=ode45(@twobody3,[0:0.01:10],[-1,0,1,0,0,-1,0,1]);
plot(x(:,1),x(:,5));
