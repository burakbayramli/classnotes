%% Generate some data
close all
clear all
clc

xy = load('out.dat','-ascii');
x = xy(:,1);
y = xy(:,2);


%   2.81385
%   0.39847
%  -0.10055

%% Now apply the rcspline function to fit the spline with knots defined by me: 
knots=[-5.5938   -3.7732   -1.9526   -0.1320    1.6886    3.5092   5.3298];
[bhat knots]=rcs3(x,y,knots);
%and obtain the values of the spline for x=-1:0.2:1;
bhat
size(knots)

