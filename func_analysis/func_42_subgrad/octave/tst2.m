load A.mat;
load y.mat;

y = y';

lambda = 0.1;

subgrad_func(A,y,lambda);
