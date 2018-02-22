% This code was written as a part of Reseacrh Methods MSc course
% Coded by: Piotr Z. Jelonek, e-mail:  p.z.jelonek@warwick.ac.uk,
% 22nd February 2016
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

%%%%%%%%%%%%%%% INITIAL VALUES 
lambda0=0.65;
omega0=0.82;
d0=0.5; 
i0=0.1;

%%%%%%%%%%%%%%% SYSTEM PARAMETERS 
alpha=0.025;      % <- per annum
beta=0.015;       % <- per annum
delta=0.07;       % <- per annum
nu=3;
r_b=0.04;         % <- base interest rate, exogeneous
s=0.3;
tau_p=1;
tau_i=0.5;

% parameters of a non-linear function, capturing the dependence 
% of
x_i=0.03;
y_i=0.03;
s_i=2.25;
m_i=0;

% parameters of a non-linear function, capturing the dependence 
% of
x_w=0.6;
y_w=0;
s_w=1;
m_w=-0.04;

% all parameters in one vector
params=[alpha beta delta nu r_b s tau_p tau_i x_i y_i s_i m_i x_w y_w s_w m_w];

%%%%%%%%%%%%%%% TRAJECTORY PARAMETERS
x0=[lambda0 omega0 d0 i0];
T=80;             % <- terminal time
tspan=[0 T];      % <- time span (from t=0 up to t=T)

% solver parameters
small=0.000001;   % <- definition of a small number

% approximation
options = odeset('RelTol',small,'AbsTol',small, 'InitialStep',1.0);
[t,x] = ode45(@(tspan,x) minsky_III_dx(tspan,x,params), tspan, x0,options);

% mapping solutions of ode to economy
lambda=x(:,1);
omega=x(:,2);
d=x(:,3);
i=x(:,4);

f=-(1/tau_p)*(1-omega/(1-s));
r=r_b+i.*(i>0); % <- this syntax is useful
p=1-omega-r.*d;
I=(y_i-m_i)*exp(s_i*((p/nu)-x_i)/(y_i-m_i))+m_i;
g=(I/nu-delta);
% there is a prize for explaining why the line below works!
Y=100*(1+[0; cumsum(0.5*(t(2:end)-t(1:end-1)).*(g(2:end)+g(1:end-1)))]);

n=size(t,1)       % <- number of points in time (optimized by solver)

%%%%%%%%%%%%%%% CHARTS %%%%%%%%%%%%%%%%%
chart=10; % <- here choose your chart

if chart==1
    plot(t,100*lambda)
    xlabel('TIME (IN YEARS)')
    ylabel('EMPLOYMENT (IN %)')
    axis([0 T 0 80])
    grid on
elseif chart==2
    plot(t,100*omega)
    xlabel('TIME (IN YEARS)')
    ylabel('WAGES TO GDP (IN %)')
    axis([0 T 65 85])
    grid on;
elseif chart==3
    plot(t,100*d)
    xlabel('TIME (IN YEARS)')
    ylabel('PRIVATE DEBT TO GDP (IN %)')
    grid on
elseif chart==4
    plot(t,100*i)
    xlabel('TIME (IN YEARS)')
    ylabel('YEARLY INFLATION (IN %)')
    grid on
elseif chart==5
    plot(t,100*p)
    xlabel('TIME (IN YEARS)')
    ylabel('PROFIT RATE (IN %)')
    grid on
elseif chart==6
    plot(t,100*r)
    xlabel('TIME (IN YEARS)')
    ylabel('NOMINAL INTEREST (IN %)')
    axis([0 T 0 18])
    grid on
elseif chart==7
    plot(t,g)
    xlabel('TIME (IN YEARS)')
    ylabel('GDP GROWTH RATE (IN %)')
    grid on
elseif chart==8
    plot(t,Y)
    xlabel('TIME (IN YEARS)')
    ylabel('GDP (IN USD)')
    axis([0 T 0 400])
    grid on
elseif chart==9
    last=800; % <- only first 60 years
    plot(100*lambda(1:last,:),100*i(1:last,:))
    xlabel('EMPLOYMENT (IN %)')
    ylabel('INFLATION (IN % PER YEAR)')
    grid on
elseif chart==10
    last=200; % <- only first 60 years
    x1=100*(1-lambda(1:last));
    x2=100*i(1:last);
    x3=100*d(1:last);
    c = 1:length(t(1:last));      % <- number of colors
    h = surface([x1(:), x1(:)], [x2(:), x2(:)], [x3(:), x3(:)], ...
    [c(:), c(:)], 'EdgeColor','flat', 'FaceColor','none'); colormap(jet(numel(t)))
    xlabel('UNEMPLOYMENT RATE')
    ylabel('INFLATION')
    zlabel('PRIVATE DEBT TO GDP')
    grid on
elseif chart==11
    % Investment function
    pt=linspace(-0.05,0.11,1601);
    It=(y_i-m_i)*exp(s_i*(pt-x_i)/(y_i-m_i))+m_i; 
    plot(100*nu*pt,100*It); % <- x has to be multiplied by nu
    xlabel('PROFIT (IN %)')
    ylabel('INVESTMENT (IN % OF GDP)')
    axis tight
    grid on;
elseif chart==12
    % Phillips curve
    lt=linspace(0.8,1.01,2101);
    Wt=(y_w-m_w)*exp(s_w*(lt-x_w)/(y_w-m_w))+m_w;
    plot(100*lt,Wt);
    xlabel('EMPLOYMENT RATE (IN %)')
    ylabel('ANNUAL CHANGE IN REAL WAGE (IN %)');  
    axis([80 101 -10 1200])
    grid on;
end
