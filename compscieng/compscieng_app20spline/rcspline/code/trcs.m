%% Generate some data
close all
clear all
clc

n=300;
x=normrnd(0,2,n,1);
e=normrnd(0,0.5,n,1);
y=sin(x)+e;


%% Now apply the rcspline function to fit the spline with knots defined by me: 
knots=[-5.5938   -3.7732   -1.9526   -0.1320    1.6886    3.5092   5.3298];
[bhat knots]=rcs2(x,y,knots);
%and obtain the values of the spline for x=-1:0.2:1;
bhat
size(knots)

xy = [x y];
save('out.dat','xy')
save('coef.dat','bhat')
