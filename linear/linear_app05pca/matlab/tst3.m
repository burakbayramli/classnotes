load irisdata.txt

D = 4;
q = 2;
X = irisdata(:,1:D);
n = size(X,2);
Xmean = mean(X,2);           % find mean
A = X - Xmean*ones(1,n);    % subtract mean from each point
%rho = norm(A,'fro')^2       % total variation of data
[U,S,V] = svd(A,'econ');    % find singular value decomposition
sigma = diag(S);
sigma
V

%[V,D,W] = svd(cov(A(1:10,:)));
%values=diag(D);
%eigvect=V;
%values=values(1:q);
%vectors=eigvect(:,1:q);

values=ones(q, 1) / q;
%vectors=normrnd(0, 0.2, D, q);
vectors=ones(D, q) / (D*q)
%values=diag(S1)
%vectors=V1

%values
%vectors

for i = 1:150
  %[values, vectors] = CCIPCA2(values, vectors, A(i,:),i-1,2);
  [V, S, D, Vn] = CCIPCA3(vectors, i, A)
  exit;
end

values
vectors

