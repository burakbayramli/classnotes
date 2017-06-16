function [eigval, eigvect] = CCIPCA(lambda, U, x, options)
% [eigval, eigvect] = CCIPCA(lambda, U, x, options)
% implementaiton of candid covariance-free incermental PCA from Weng 2003
% parameters
% lambda: eigenvalues
% U: eigenvectors
% x: current sample
% options.n: sample size before observing x
% options.f: forgetting factor: a number in (0,1).
% options.q: number of components to find
% options.tol: tolerance
% output
% eigval:updated eigenvalues
% eigvect=iupdated eigenvectors

n=options.n;
q=options.q;


if isfield(options,'tol')
    tol=options.tol;
else
    tol=1e-7;
end    


k =length(lambda);
q =int32(q);



x=x';

V=zeros(size(U));
for jjj=1:q  
    u_=U(:,jjj);
    v_=n/(n+1)*lambda(jjj)*u_+1/(n+1)*(x'*u_)*x;    
    nrm=norm(v_);    
    u_=v_/nrm;
    lambda(jjj)=nrm;
    x=x-u_*(u_'*x);
    U(:,jjj)=u_;
end

[~,idx]=sort(lambda,'descend');
eigval=lambda(idx);
eigvect=U(:,idx);
