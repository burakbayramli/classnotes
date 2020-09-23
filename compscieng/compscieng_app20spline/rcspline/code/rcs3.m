
function [bhat X]=rcs3(x,y,knots)

n=length(y);
k=knots;
X1=x;
q=length(k);
myX=zeros(n,length(knots)-2);

for j=1:(q-2)
  tmp1 = (x-k(j)).^3.*(x>k(j));
  tmp2 = (x-k(q-1)).^3.*(x>k(q-1)).*(k(q)-k(j));
  XX= tmp1-tmp2./(k(q)-k(q-1));  
  tmp1 = (x-k(q)).^3.*(x>k(q));
  tmp2 = (k(q-1)-k(j));
  XX=XX+tmp1.*tmp2./(k(q)-k(q-1));
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
disp(bhatt(end-1:end));
bhat=[bhat; bhatt(end-1:end)'];
end




