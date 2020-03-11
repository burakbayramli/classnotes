load A.mat;
load y.mat;

y = y'

size(A)
size(y)

lambda = 0.1;

subgrad_func(A,y,lambda);


