clear; clc;

pkg load odepkg;

% system parameters
alpha=0.02;      % <- per annum
beta=0.01;       % <- per annum
c=4.8;
d=5;
gamma=0.01;      % <- per annum
nu=3;

% initial values
L0=300;          % <- initial labour force
w0=0.95;         % <- initial wages
a0=1;            % <- initial technology
N0=300;          % <- initial population

% trajectory parameters
x0=[L0 w0 a0 N0];
T=100;           % <- terminal time
tspan=[0 T];     % <- time span (from t=0 up to t=T)

% solver parameters
small=0.00001;   % <- definition of a small number

% approximation
%options = odeset('RelTol',small,'AbsTol',small);
options = odeset('RelTol',small,'AbsTol',small, 'InitialStep',10.0,'MaxStep',10.0)

[t,x] = ode45(@(tspan,x) minsky_I_dx(tspan,x,alpha,beta,c,d,gamma,nu), tspan, x0, options);

% mapping solutions of ode to economy
L=x(:,1); 
w=x(:,2); 
a=x(:,3); 
N=x(:,4);

Y=a.*L;
K=Y/nu;
P=Y-w.*L;
I=P;

employment_rate=100*L./N; % <- in % points
wage_share=100*(w.*L)./Y; % <- in % points

size(Y)
L(end-10:end)
w(end-10:end)
a(end-10:end)
N(end-10:end)
Y(end-10:end)

% here choose your favourite chart
chart=2;

%if chart==1
%    % output (cyclical)
%    plot(t,Y)
%    xlabel('TIME (IN YEARS)')
%    ylabel('OUTPUT');
%    grid on;
%elseif chart==2
%    % employment cycles
%    plot(employment_rate,wage_share)
%    xlabel('EMPLOYMENT RATE')
%    ylabel('WAGES TO OUTPUT')
%    axis([90 105 60 120])
%    grid on;
%end
