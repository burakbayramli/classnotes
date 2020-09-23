function [t,x]= bvp2_shoot(f,t0,tf,x0,xf,N,tol,kmax,Kg)
dx0(1)= (xf-x0)/(tf-t0);
[t,x]= ode_RK4(f,[t0 tf],[x0 dx0(1)],N);
plot(t,x(:,1),'b'), hold on
e(1)=x(end,1)-xf;
dx0(2)= dx0(1)-0.1*sign(e(1));
for k=2: kmax-1
   disp(k)
   [t,x]= ode_RK4(f,[t0 tf],[x0 dx0(k)],N);
   if k==2, plot(t,x(:,1),'c'); hold on; end;
   e(k)=x(end,1)-xf; %difference between the resulting final value and the desired one
   ddx= dx0(k)-dx0(k-1);
   if abs(e(k))<tol||abs(ddx)<tol, break; end
   deddx= (e(k)-e(k-1))/ddx;           
   dx0(k+1)= dx0(k)-e(k)/deddx; %move by secant method
end
