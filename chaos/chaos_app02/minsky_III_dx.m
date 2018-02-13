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

function dx = minsky_III_dx(tspan,x,params)

    alpha=params(1); beta=params(2); delta=params(3); nu=params(4); 
    r_b=params(5); s=params(6); tau_p=params(7); tau_i=params(8);
    x_i=params(9); y_i=params(10); s_i=params(11); m_i=params(12);
    x_w=params(13); y_w=params(14); s_w=params(15); m_w=params(16);

    r=r_b; % <- interest rate
    if x(4)>0
        r=r+x(4); 
    end
    p=1-x(2)-r*x(3);
    f=-(1/tau_p)*(1-x(2)/(1-s));
    I=(y_i-m_i)*exp(s_i*((p/nu)-x_i)/(y_i-m_i))+m_i;
    W=(y_w-m_w)*exp(s_w*(x(1)-x_w)/(y_w-m_w))+m_w;

    dx=zeros(4,1);

    dx(1)=( ((1/nu)*I-delta) -(alpha + beta) )*x(1);
    dx(2)=( W - (alpha+f) )*x(2);
    dx(3)=( I-p ) -( (1/nu)*I - delta + f )*x(3);
    dx(4)=-(1/tau_i)*(x(4)-f);

end