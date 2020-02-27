% from boyd 11.24 exercise solution

MAXITERS = 200;
TOL = 1e-6;
m=3;n = 3
RESTOL = 1e-8;
MU = 10;
ALPHA = 0.01;
BETA = 0.5;
x = zeros(n,1);

b = ones(n,1)*10.;
q = ones(n,1)*3.;
A = [2 3 5; 3 4 5; 4 5 3];
%A = rand(n,n);
%P = rand(n,n)
P = [1 2 4; 2 4 4; 1 1 1];

s = b-A*x;
z = 1./s;
for iters = 1:MAXITERS
  gap = s'*z;
  res = P*x + q + A'*z ;
  if ((gap < TOL) && (norm(res) < RESTOL)), break; end;
  tinv = gap/(m*MU);
  sol = -[ P A'; A diag(-s./z) ] \ [ P*x+q+A'*z; -s + tinv*(1./z) ];
  dx = sol(1:n);
  dz = sol(n+[1:m]);
  ds = -A*dx;
  r = [P*x+q+A'*z; z.*s-tinv];
  step = min(1.0, 0.99/max(-dz./z));
  while (min(s+step*ds) <= 0),
    step = BETA*step;
    step
  end;
  newz = z+step*dz; newx = x+step*dx; news = s+step*ds;
  newr = [P*newx+q+A'*newz; newz.*news-tinv];
  while (norm(newr) > (1-ALPHA*step)*norm(r))
    step = BETA*step;
    newz = z+step*dz; newx = x+step*dx; news = s+step*ds;
    newr = [P*newx+q+A'*newz; newz.*news-tinv];
  end;
  x = x+step*dx; z = z +step*dz; s = b-A*x;

end;
x
