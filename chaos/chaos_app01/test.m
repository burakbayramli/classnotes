pkg load odepkg;

tol1=0.005;tol2=tol1/10;
tspan=[0,17.06521656015796];
y0=[0.994;0;0;-2.00158510637908];
figure(1),clf(1)
[t1,y1,h1]=ode12(@Arenstorf,tspan,y0,tol1);
plot(y1(:,1),y1(:,2));
axis([-1.5 1.5 -1.5 1.5]); hold
plot(0,0,'o')
plot(1,0,'o')


%figure(2),clf(2)
%axis([0,length(h1)+10,0,0.3]); hold
%plot(h1)
%NumberOfSteps=length(t1)
%RepeatedSteps=length(h1)-length(t1)
%figure(3),clf(3)
%[t2,y2,h2]=ode12(@Arenstorf,tspan,y0,tol2);
%plot(y2(:,1),y2(:,2));
