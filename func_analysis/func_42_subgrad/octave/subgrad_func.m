% https://raw.githubusercontent.com/fengcls/Lasso/master/lasso_main.m
% 0.5*||Ax - b||_2 + lambda*||x||_1
% subgradient method
function subgrad_func(A,b,lambda)
  [~,n2] = size(A);
  x = zeros(n2,1);
  k=1;
  g = ones(n2,1);
  t = 0.01;

  while k<3 || abs(f(k-1)-f(k-2))/f(k-1)>1e-5
    % f(round(k/10)+1)=0.5*norm(A*x-b,2)^2+lambda*norm(x,1);
    f(k)=0.5*norm(A*x-b,2)^2+lambda*norm(x,1);
    disp(f(k));
    % the subgradient is A'*(A*x-b)
    s = x;
    s(x>0)=1;
    s(x<0)=-1;
    s(x==0) = -2*rand(length(find(x==0)),1)+1;
    g = A'*(A*x-b)+lambda*s;
    x = x - t*g;
    k = k+1;
end;
x
