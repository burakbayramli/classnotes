clear;clc;
%% generate random synthetic data
n = 30; p = 30; sparse_ratio = 0.01;
A = randn(n,p);
beta = randn(p,1);

beta(randperm(p,round((1-sparse_ratio)*p)))=0;
beta0 = randn;
b = beta0 + A*beta + randn(n,1);

lambda = 0.1;

beta

subgrad_func(A,b,lambda);
