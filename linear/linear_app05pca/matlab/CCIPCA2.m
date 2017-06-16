function [eigval, eigvect] = CCIPCA2(lambda, U, x, n, q)
  tol=1e-7;
  k = length(lambda);
  x = x';
  V = zeros(size(U));
  for jjj = 1:q  
    u_ = U(:,jjj);
    n/(n+1)*lambda(jjj)
    v_ = n/(n+1)*lambda(jjj) * u_+1 / (n+1)*(x'*u_)*x;
    exit;
    nrm = norm(v_);    
    u_ = v_/nrm;
    lambda(jjj) = nrm;
    x = x-u_*(u_'*x);
    U(:,jjj) = u_;
  end
  [~,idx] = sort(lambda,'descend');
  eigval = lambda(idx);
  eigvect = U(:,idx);
