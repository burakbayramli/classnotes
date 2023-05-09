%do_shoot  to solve BVP2 by the shooting method
clear
figure(1), clf
t0 = 0; tf = 1; x0 = 1/4; xf = 1/3;
N = 100; tol = 1e-8; kmax = 10;
[t,x] = bvp2_shoot('df661',t0,tf,x0,xf,N,tol,kmax);
xo = 1./(4 - t.*t); err = norm(x(:,1) - xo)/(N + 1)
plot(t,x(:,1),'g', t,xo,'--r')
print -djpg /tmp/out1.jpg
