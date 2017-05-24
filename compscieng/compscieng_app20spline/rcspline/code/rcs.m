
function [bhat ff sse X]=rcs(x,y,knots,plots)

%INTERIOR FUNCTION FOR THE rcspline function:

%Fits a restricted cubic spline via least squares.
%The obtained spline is linear beyond the first and the last knot. The
%power basis representation is used. That is, the fitted spline is of the
%form: f(x)=b0+b1*x+b2*(x-t1)^3*(x>t1)+b3*(x-t2)^3*(x>t2)+...
%where t1 t2,... are the desired knots. For more information see also
%Harrell Jr, Regression Modelling Strategies.
%
%INPUT ARGUMENTS:
%x: A vector containing the covariate values x.
%y: A vector of length(x) that contains the response values y.
%knots: A vector of points at which the knots are to be placed.
%
%OPTIONAL INPUT ARGUMENT:
%plots: If set to 1, it returns a plot of the spline and the data.
%Otherwise it is ignored. This input argument can also not be reached at
%all.
%
%OUTPUT ARGUMENTS:
%bhat: the estimated spline coefficients.
%ff:   a function handle from which you can evaluate the spline value at a
%      given x (which can be a scalar or a vector). For example ff(2) will 
%      yield the spline value for x=2. You can use a vector (grid) of x values to
%      plot the f(x) by requesting plot(x,f(x)).
%rss:  equals to sum((y-ff(x)).^2)

%Code author: Leonidas E. Bantis, University of the Aegean.
%E-mail: leobantis@gmail.com
%Date: January 14th, 2013.
%Version: 1.


%Some error checking:
% if sum(isnan(x))~=0 || sum(isnan(y))~=0;error('The x and y vectors must not contain NaNs');end
% [rx cx]=size(x);
% [ry cy]=size(y);
% if rx~=1 && cx~=1;error('x must be a vector and not a matrix');end
% if ry~=1 && cy~=1;error('x must be a vector and not a matrix');end
% if cx~=1;x=x';end
% if cy~=1;y=y';end
% if length(x)~=length(y);error('x and y must have the same length');end
% 
% [rk ck]=size(knots);
% if rk~=1 && ck~=1;error('knots must be a vector and not a matrix');end


n=length(y);
k=knots;
X1=x;
q=length(k);
myX=zeros(n,length(knots)-2);
for j=1:(q-2)
    XX=(x-k(j)).^3.*(x>k(j))-(x-k(q-1)).^3.*(x>k(q-1)).*(k(q)-k(j))./(k(q)-k(q-1));
    XX=XX+(x-k(q)).^3.*(x>k(q)).*(k(q-1)-k(j))./(k(q)-k(q-1));
    myX(:,j)=XX;
end

X=[ones(n,1) X1 myX]; %the design matrix
bhat=X\y; %obtain the coefs

%Deal with the restriction and derive the last coefs so as linearity is
%imposed beyond the first and the last knots:
bhatt(length(bhat)+1)=sum(bhat(3:end).*(k(1:end-2)-k(end))');   
bhatt(length(bhat)+1)=bhatt(length(bhat)+1)./(k(end)-k(end-1));

bhatt=[bhatt 0];
bhatt(end)=sum(bhat(3:end).*(k(1:end-2)-k(end-1))');   
bhatt(end)=bhatt(end)./(k(end-1)-k(end));
bhat=[bhat; bhatt(end-1:end)'];
%Just obtained the estimated coefs vector

f2=@(x) bhat(1)+bhat(2).*x+sum(bhat(3:end)'.*(x-k(1:end)).^3.*(x>k(1:end)));
gr=min(x):0.01:max(x);
ff=@(x)arrayfun(f2, x); %The spline function handle

%If requested provide the plot:
if plots==1;
    %subplot(2,1,1)
    plot(x,y,'.')
    hold on;
    plot(knots,min(y)+zeros(1,length(knots)),'or')
    plot(gr,ff(gr),'r');    
    legend('data','knots','spline')
end

sse=sum((y-ff(x)).^2);



end




