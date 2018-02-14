% This code was written as a part of Reseacrh Methods MSc course
% Coded by: Piotr Z. Jelonek, e-mail:  p.z.jelonek@warwick.ac.uk,
% 20th and 21st February 2016
%
% Disclaimer:
% 1. This script is intended for a non-commercial use.
% 2. You can use, amend and edit it to fit to your purposes for your own use only, 
%    but not for further distribution.
% 3. This script comes with no warranty. 
% 4. Please quote the author when using the script.
%
% Copyright: The author(s) own the right to modify or amend the script and 
% claim for the authorship of it.

clear; clc;

pkg load odepkg;

% system parameters
alpha=0.02;      % <- per annum
beta=0.01;       % <- per annum
gamma=0.01;      % <- per annum
nu=3;
r=0.05;          % <- interest rate, exogeneous

% parameters of a non-linear function, capturing the dependence 
% of investment on ratio of profit to capital
x_p=0.05;
y_p=0.05;
s_p=1.75;
m_p=0;

% parameters of a non-linear function, capturing the dependence
% of wage growth rate on employment (aka Phillips curve)
x_l=0.95; 
y_l=0;
s_l=0.5;
m_l=-0.01;

% all parameters in one vector
params=[alpha beta gamma nu r x_p y_p s_p m_p x_l y_l s_l m_l];

% initial values
Y0=300;          % <- initial output
w0=0.95;         % <- initial wages
D0=0;            % <- initial debt
a0=1;            % <- initial technology
N0=300;          % <- initial population

% trajectory parameters
x0=[Y0 w0 D0 a0 N0];
T=120;           % <- terminal time
tspan=[0 T];     % <- time span (from t=0 up to t=T)

% solver parameters
small=0.000001;   % <- definition of a small number

% approximation
options = odeset('RelTol',small,'AbsTol',small,'InitialStep',12.0, 'MaxStep',12.0);
[t,x] = ode45(@(tspan,x) minsky_II_dx(tspan,x,params), tspan, x0,options);

% mapping solutions of ode to economy
Y=x(:,1); 
w=x(:,2); 
D=x(:,3);
a=x(:,4); 
N=x(:,5);

Y(end-5:end)
w(end-5:end)
D(end-5:end)
a(end-5:end)
N(end-5:end)

K=Y/nu;
L=Y./a;
P=Y-w.*L-r*D;
I=P;

% here choose your favourite chart
chart=4;

if chart==1
    % PHillips curve
    l=linspace(0.9,1.01,1101);
    H=(y_l-m_l)*exp(s_l*(l-x_l)/(y_l-m_l))+m_l;
    plot(100*l,100*H);
    xlabel('EMPLOYMENT RATE (IN %)')
    ylabel('ANNUAL CHANGE IN REAL WAGE (IN %)');  
    axis([90 101 -10 20])
    grid on;
elseif chart==2
    % Investment function
    p=linspace(-0.05,0.11,1601);
    I=(y_p-m_p)*exp(s_p*(p-x_p)/(y_p-m_p))+m_p; 
    plot(100*nu*p,100*I); % <- x has to be multiplied by nu
    xlabel('PROFIT (IN %)')
    ylabel('INVESTMENT (AS % OF OUTPUT)')
    % axis([-5 11 0 50])
    axis tight;
    grid on;
elseif chart==3
    % Output
    plot(t,Y);
    xlabel('YEARS')
    ylabel('REAL OUTPUT')
    axis tight
    grid on;
elseif chart==4
    wage_share=(w.*L)./Y; 
    employment_rate=L./N;
    debt_ratio=D./Y;
    x1=wage_share; x2=employment_rate; x3=debt_ratio; 
    c = 1:length(t);      % <- number of colors
    h = surface([x1(:), x1(:)], [x2(:), x2(:)], [x3(:), x3(:)], ...
    [c(:), c(:)], 'EdgeColor','flat', 'FaceColor','none'); colormap(jet(numel(t)))
    xlabel('WAGE SHARE OF OUTPUT')
    ylabel('EMPLOYMENT RATE')
    zlabel('DEBT RATIO')
    grid on
end
