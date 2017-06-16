function [V, S, D, Vn] = CCIPCA3(V, time, input, pp, num_comp)
%CCIPCA: M. Luciw July 31,2011
%---inputs---
%V: matrix of eigenvectors (col vectors), L2-norms are eigenvalues
%   num cols of V gives the num of vectors to compute
%time: current time step
%input: input vector --- IMPORTANT: the input must come from zero-mean distribution! 
%pp: plasticity parameters for amnesic average, t1, t2, c, r (see paper or amnesic.m)
%num_comp: number of components to compute... optional
%---outputs---
%V: updated matrix of eigenvectors
%S: updated sphering matrix (whitening matrix)
%D: eigenvalue diagonal matrix
%Vn: normed eigenvectors
%M. Luciw
%http://people.idsia.ch/~luciw/incsfa.html  

%number components
if (nargin < 5)
    num_comp = size(V,2); 
end

if (nargin < 4)
    pp = [20 200 3 2000];
end

%learning rate from amnesic average
[dummy lr] = amnesic(time,pp(1),pp(2),pp(3),pp(4));  
                
Xt = input;

for j=1:num_comp
    
    %update
    V(:,j) = (1-lr) * V(:,j) + ...
        lr * (V(:,j)' * Xt)./norm(V(:,j)) * Xt;
    
    %residual
    Xt = Xt - (Xt' * V(:,j))./norm(V(:,j)) * (V(:,j)./norm(V(:,j)));
    
    Vnorm(j) = norm(V(:,j));
end

%ensure ordering of eigenvectors (optional)
[dummy order]=sort(-Vnorm);
V=V(:,order);

%normalized
for j=1:num_comp    
    Vn(:,j) = V(:,j)./norm(V(:,j));
    D(j,j) = 1/sqrt(norm(V(:,j)));    %eigenvalue estimate is magnitude
end

%whitening matrix
S = Vn * D;
