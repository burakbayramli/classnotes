% This code was written as a part of Reseacrh Methods MSc course
% Coded by: Piotr Z. Jelonek, e-mail:  p.z.jelonek@warwick.ac.uk,
% 20th February 2016
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

function dx = minsky_II_dx(tspan,x,params)

    % reading paramaters
    alpha=params(1); beta=params(2); gamma=params(3); nu=params(4); r=params(5);
    x_p=params(6); y_p=params(7); s_p=params(8); m_p=params(9); 
    x_l=params(10); y_l=params(11); s_l=params(12); m_l=params(13);
   
    % auxilaries
    L=x(1)/x(4);                                 % <- labour
    P=x(1)-x(2)*L - r*x(3);                      % <- profit
    p=P/(nu*x(1));                               % <- profit to capital
    I=(y_p-m_p)*exp(s_p*(p-x_p)/(y_p-m_p))+m_p;  % <- investment as a function of profit
    l=x(1)/(x(4)*x(5));                          % <- employment rate
    H=(y_l-m_l)*exp(s_l*(l-x_l)/(y_l-m_l))+m_l;  % <- growth rate of wages as a fctn. of employment rate

    % derivative
    dx=zeros(5,1);
    dx(1)=x(1)*(I/nu - gamma );
    dx(2)=H*x(2);
    dx(3)=I*x(1)-P;
    dx(4)=alpha*x(4);
    dx(5)=beta*x(5);

end

