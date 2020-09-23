function dx = minsky_I_dx(tspan,x,alpha,beta,c,d,gamma,nu)
    dx=zeros(4,1);
    dx(1)=( (1/nu)*(1-x(2)/x(3))- gamma - alpha)*x(1);
    dx(2)=(d*(x(1)/x(4))-c)*x(2);
    dx(3)=alpha*x(3);
    dx(4)=beta*x(4);
end

