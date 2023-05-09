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
[bhat f sse knots]=rcspline(x,y,knots);
%and obtain the values of the spline for x=-1:0.2:1;
f([-1:0.2:1])

%% Fit the spline with 6 knots places at equally spaced percentiles and plot it:
close all
clc
[bhat f sse1 knots]=rcspline(x,y,'prc6',[],[],1);

%% Obtain the value of the spline for x=[-1 0 1]:
f([-1 0 1])


%% Fit the spline with 7 equally spaced knots and plot it:
close all
clc
[bhat f sse2 knots]=rcspline(x,y,'eq7',[],[],1);
%Observe that sse2 is smaller that sse1 which is expected since sse2
%corresponds to a spline that used one additional knot.
[sse1 sse2]

%% Fit the spline with 6 knots placed at equally spaced percentiles and plot it:
%also use 100 bootstrap sample to obtain the corresponding 95% CIs for
%x=min(x):1:max(x)
close all
clc
[bhat f sse knots CI]=rcspline(x,y,'eq7',100,min(x):1:max(x),1);
%Ask for CI: the first column of which is min(x):1:max(x)
%the second column is the value of the spline at these x, i.e. f(min(x):1:max(x))
%the third and fourth column are the corresponding CI based on the
%percentile bootstrap.

CI


%% Fit spline and obtain design matrix
close all
clear all 
clc
n=100
x=normrnd(0,2,n,1);
e=normrnd(0,0.5,n,1);
y=sin(x)+e;
hold on
gr=min(x):0.01:max(x);

[bhat f sse knots CI]=rcspline(x,y,'prc4',100,min(x):1:max(x),1);
bhat
