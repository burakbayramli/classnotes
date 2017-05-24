
function [bhat f sse knots CI]=rcspline(x,y,knots,bootsams,atwhich,plots)

%Fits the so called restricted cubic spline via least squares (see Harrell 
%(2001)). The obtained spline is linear beyond the first and the last 
%knot. The truncated power basis representation is used. That is, the 
%fitted spline is of the form: 
%f(x)=b0+b1*x+b2*(x-t1)^3*(x>t1)+b3*(x-t2)^3*(x>t2)+...
%where t1 t2,... are the desired knots. 
%95% confidence intervals are provided based on the bootstrap procedure.

%For more information see also:
%Frank E Harrell Jr, Regression Modelling Strategies (With application to 
%linear models, logistic regression and survival analysis), 2001, 
%Springer Series in Statistics, pages 20-21.
%
%INPUT ARGUMENTS:
%x:     A vector containing the covariate values x.
%y:     A vector of length(x) that contains the response values y.
%knots: A vector of points at which the knots are to be placed.
%       Alternatively, it can be set as 'prc3', 'prc4', ..., 'prc8' and 3 
%       or 4 or...8 knots placed at equally spaced percentiles will be used.
%       It can also be set to 'eq3', 'eq4', ...,'eq8' to use 3 or 4  or ... 
%       or 8 equally spaced knots. There is a difference in using one of 
%       these strings to define the knots instead of passing them directly 
%       as a vector of numbers and the difference involves only the
%       bootstrap option and not the fit itself. When the bootstrap is used
%       and the knots are passed in as numbers, then the knot sequence will
%       be considered fixed as provided by the user for each bootstrap 
%       iteration. If a string as the ones mentioned above is used, then 
%       the knot sequence is re-evaluated for each bootstrap sample
%       based on this choice.
%
%OPTIONAL INPUT ARGUMENTS: (These can be not reached at all or set as [] to
%proceed to the next optional input argument):
%
%bootsams: The number of bootstrap samples if the user wants to derive 95% CIs.
%atwhich: a vector of x values at which the CIs of the f(x) are to be evaluated.
%plots: If set to 1, it returns a plot of the spline and the data.
%       Otherwise it is ignored. This input argument can also not be reached 
%       at all. (It also plots the CIs provided that they are requested).
%
%OUTPUT ARGUMENTS:
%bhat:  the estimated spline coefficients.
%f:     a function handle from which you can evaluate the spline value at a
%       given x (which can be a scalar or a vector). For example ff(2) will 
%       yield the spline value for x=2. You can use a vector (grid) of x values to
%       plot the f(x) by requesting plot(x,f(x)).
%sse:   equals to sum((y-ff(x)).^2)
%knots: the knots used for fitting the spline.
%CI   : 95% bootstrap based confidence intervals.
%       Obtained only if the bootstrap is requested and and only fot the 
%       points at which the CIs were requested. Hence, CI is a three column
%       matrix with its first column be the spline value at the points
%       supplied by the user, and the second and third column are
%       respectively the lower and upper CI limits for that points.
%
%References: Frank E. Harrell, Jr. Regression Modeling Strategies (With 
%applications to linear models, logistic regression, and survival
%analysis). Springer 2001. 
%
%
%Code author: Leonidas E. Bantis, 
%Dept. of Statistics & Actuarial-Financial Mathematics, School of Sciences
%University of the Aegean, Samos Island.
%
%E-mail: leobantis@gmail.com
%Date: January 14th, 2013.
%Version: 1.


%Some error checking:
if sum(isnan(x))~=0 || sum(isnan(y))~=0;error('The x and y vectors must not contain NaNs');end
[rx cx]=size(x);
[ry cy]=size(y);
if rx~=1 && cx~=1;error('x must be a vector and not a matrix');end
if ry~=1 && cy~=1;error('x must be a vector and not a matrix');end
if cx~=1;x=x';end
if cy~=1;y=y';end
if length(x)~=length(y);error('x and y must have the same length');end


if isnumeric(knots)==1
[rk ck]=size(knots);
if rk~=1 && ck~=1;error('knots must be a vector and not a matrix');end
end


if nargin>=4 && nargin<5
    error('The number of bootstrap samples must be followed by the points at which the CIs are needed');
end

if nargin>=5
    if isempty(bootsams)==1 && isempty(bootsams)~=1;error('If the ''bootsams'' is empty then the ''atwhich'' must be also  empty');end 
    if isempty(bootsams)~=1 && isempty(bootsams)==1;error('If the ''atwhwich'' is empty then the ''bootsams'' must be also  empty');end
end

orknots=knots; %original knots suuplied by the user

if nargin>=5;
[rat cat]=size(y);
if rat~=1 && cat~=1;error('x must be a vector and not a matrix');end
if cat~=1;atwhich=atwhich';end
end


if strcmpi(knots, 'prc3')==1
    knots=prctile(x,linspace(0,100,3));
elseif strcmpi(knots, 'prc4')==1
    knots=prctile(x,linspace(0,100,4));
elseif strcmpi(knots, 'prc5')==1
    knots=prctile(x,linspace(0,100,5));
elseif strcmpi(knots, 'prc6')==1
    knots=prctile(x,linspace(0,100,6));
elseif strcmpi(knots, 'prc7')==1
    knots=prctile(x,linspace(0,100,7));
elseif strcmpi(knots, 'prc8')==1
    knots=prctile(x,linspace(0,100,8));
elseif strcmpi(knots, 'eq3')==1
    knots=linspace(min(x),max(x),3);
elseif strcmpi(knots, 'eq4')==1
    knots=linspace(min(x),max(x),4);
elseif strcmpi(knots, 'eq5')==1
    knots=linspace(min(x),max(x),5);
elseif strcmpi(knots, 'eq6')==1
    knots=linspace(min(x),max(x),6);
elseif strcmpi(knots, 'eq7')==1
    knots=linspace(min(x),max(x),7);
elseif strcmpi(knots, 'eq8')==1
    knots=linspace(min(x),max(x),8);
end
                     
n=length(y);
if nargin<6;plots=0;end
[bhat f sse]=rcs(x,y,knots,plots);%get the spline


if nargin>=4 && isempty(bootsams)~=1
    FF=zeros(length(atwhich),bootsams);
    for boots=1:bootsams
        at=randsample(n,n,'true');
        xb=x(at);yb=y(at);
        if isnumeric(knots)~=0;
            bknots=evknots(orknots,xb);
        else
            bknots=knots;
        end
        [~, fb]=rcs(xb,yb,bknots,0);
        FF(:,boots)=fb(atwhich);
    end
    low=zeros(1,length(atwhich));
    upp=low;

    for i=1:length(atwhich)
        low(i)=prctile(FF(i,:),2.5);
        upp(i)=prctile(FF(i,:),97.5);
    end 
    low=low';upp=upp';
    CI=[atwhich' f(atwhich)' low upp];
    if plots==1
        %subplot(2,1,2)
        figure
        gr=min(x):0.01:max(x);
        plot(gr,f(gr),'r');hold on;
        plot(atwhich,low,'.g');plot(atwhich,upp,'.g');
        legend('spline', '95% CIs')
        hold off
    end
end
    
end

function out=evknots(knots,x) 
%interior function that evaluates the knots for the bootstrap
%when they are not consider fixed.
if strcmpi(knots, 'prc3')==1
    knots=prctile(x,linspace(0,100,3));
elseif strcmpi(knots, 'prc4')==1
    knots=prctile(x,linspace(0,100,4));
elseif strcmpi(knots, 'prc5')==1
    knots=prctile(x,linspace(0,100,5));
elseif strcmpi(knots, 'prc6')==1
    knots=prctile(x,linspace(0,100,6));
elseif strcmpi(knots, 'prc7')==1
    knots=prctile(x,linspace(0,100,7));
elseif strcmpi(knots, 'prc8')==1
    knots=prctile(x,linspace(0,100,8));
elseif strcmpi(knots, 'eq3')==1
    knots=linspace(min(x),max(x),3);
elseif strcmpi(knots, 'eq4')==1
    knots=linspace(min(x),max(x),4);
elseif strcmpi(knots, 'eq5')==1
    knots=linspace(min(x),max(x),5);
elseif strcmpi(knots, 'eq6')==1
    knots=linspace(min(x),max(x),6);
elseif strcmpi(knots, 'eq7')==1
    knots=linspace(min(x),max(x),7);
elseif strcmpi(knots, 'eq8')==1
    knots=linspace(min(x),max(x),8);
end
out=knots;

end
