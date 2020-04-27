clear all;
m = 4
n = 5

rand('seed',0);

A = [randn(m-1,n); ones(1,n)];
x_0 = rand(n,1) + 0.1;
b = A*x_0;
c = randn(n,1);

[x_star, nu_star, lambda_hist] = lp_acent(A,b,c,x_0);

x_star

[x_star, history, gap] = lp_barrier(A,b,c,x_0);

x_star

p_star = c'*x_star;

p_star

