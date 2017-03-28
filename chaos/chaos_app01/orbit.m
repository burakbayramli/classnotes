% nahin
function xyderiv=orbit(t,u)
  alpha=1;d=2.1;fps=4*pi*pi;
  r1=[u(1),u(3)];r2=[d-u(1),u(3)];
  denom1=norm(r1)^3;denom2=norm(r2)^3;
  xyderiv=[u(2);fps*((-u(1)/denom1)+(alpha*(d-u(1))/denom2));u(4);-fps*u(3)*((1/denom1)+(alpha/denom2))];
end
