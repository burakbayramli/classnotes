% solves the diabetes problem using proximal desc

load A.mat;
load y.mat;
y = y';
b = y;

%n = 1000; % number of variables
%m = 50; % number of equality constraints
[m,n] = size(A);
%randn('state',1); % set state so problem is reproducable
%A = randn(m,n);
%b = randn(m,1);

% threshold value below which we consider an element to be zero
DELTA = 1e-8;

% initial point needs to satisfy A*x1 = b (can use least-norm solution)
x1 = pinv(A)*b;

%********************************************************************
% subgradient method computation
%********************************************************************
f = [+Inf]; fbest = [+Inf];

k = 1;
x = x1;

MAX_ITERS = 1000;

while k < MAX_ITERS 

  % subgradient calculation
  fval = norm(x, 1);
  g = (x > DELTA) - (x < -DELTA); % sign(x) with DELTA tolerance

  % step size selection
  alpha = 0.1/k;

  % keep objective values
  f(end+1) = fval;
  fbest(end+1) = min( fval, fbest(end) );

  % subgradient update
  x = x - alpha*(g - A'*(A'\g));  k = k + 1;

  if( rem(k,500) == 0 ), fprintf(1,'iter: %d\n',k), end
end

x
