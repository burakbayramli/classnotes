function [t,y,H]=ode12(f,tspan,y0,tol);

dt=(tspan(2)-tspan(1))/10; H=dt;
y(:,1)=y0;
t(1)=tspan(1);
as=0.8; amin=0.2; amax=5;
i=1;
while t(i)<tspan(2),
  k1=f(t(i),y(:,i));
  k2=f(t(i)+dt/2,y(:,i)+dt/2*k1);
  yn=y(:,i)+dt*k2;
  yt=y(:,i)+dt*k1;
  toli=max([norm(yn,inf) norm(y(:,i),inf)]);
  toli=tol*(1+toli);
  errest=norm(yn-yt);
  if errest<toli,
    i=i+1;
    y(:,i)=yn;
    t(i)=t(i-1)+dt;
  end;
  dtopt=as*(dt*sqrt(toli/errest));
  if dtopt<amin*dt,
    dt=amin*dt;
  elseif dtopt>amax*dt,
    dt=amax*dt;
  else
    dt=dtopt;
  end;
  if t(i)+dt>tspan(2),
    dt=tspan(2)-t(i);
  end;
  H=[H;dt];  
end;
t=t';
y=y';  
