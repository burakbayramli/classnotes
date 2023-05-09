
function [bhat X]=rcs2(x,y,knots,plots)

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

end




