% femcode_P1.m
 
% [p,t,b] from distmesh tool
 
fd=@(p) sqrt(sum(p.^2,2))-1;
[p,t]=distmesh2d(fd,@huniform,0.20,[-1,-1;1,1],[]);
be=boundedges(p,t);
b=unique(be);
  
f=vectorize(inline('4','x','y'));
k=vectorize(inline('1','x','y')); 
u=vectorize(inline('1-x^2-y^2','x','y'));
ux=vectorize(inline('-2*x','x','y'));
uy=vectorize(inline('-2*y','x','y')); 
 
% [K,F] = assemble(p,t) % K and F for any mesh of triangles: linear phi's
N=size(p,1);T=size(t,1); % number of nodes, number of triangles
% p lists x,y coordinates of N nodes, t lists triangles by 3 node numbers
K=sparse(N,N); % zero matrix in sparse format: zeros(N) would be "dense"
F=zeros(N,1); % load vector F to hold integrals of phi's times load f(x,y)
 
for e=1:T  % integration over one triangular element at a time
  nodes=t(e,:); % row of t = node numbers of the 3 corners of triangle e
  Pe=[ones(3,1),p(nodes,:)]; % 3 by 3 matrix with rows=[1 xcorner ycorner]
  Area=abs(det(Pe))/2; % area of triangle e = half of parallelogram area
  C=inv(Pe); % columns of C are coeffs in a+bx+cy to give phi=1,0,0 at nodes
  % now compute 3 by 3 Ke and 3 by 1 Fe for element e
  qpt = (1/3)*sum(p(nodes,:));
  I = feval(k,qpt(1),qpt(2));
  grad=C(2:3,:);Ke=Area*I*grad'*grad; % element matrix from slopes b,c in grad
  g = feval(f,qpt(1),qpt(2));
  Fe=Area/3*g; % integral of phi over triangle is volume of pyramid: f(x,y)=1
  % multiply Fe by f at centroid for load f(x,y): one-point quadrature!
  % centroid would be mean(p(nodes,:)) = average of 3 node coordinates
  K(nodes,nodes)=K(nodes,nodes)+Ke; % add Ke to 9 entries of global K
  F(nodes)=F(nodes)+Fe; % add Fe to 3 components of load vector F
end   % all T element matrices and vectors now assembled into K and F
 
% [Kb,Fb] = dirichlet(K,F,b) % assembled K was singular! K*ones(N,1)=0
% Implement Dirichlet boundary conditions U(b)=0 at nodes in list b
K(b,:)=0; K(:,b)=0; F(b)=0; % put zeros in boundary rows/columns of K and F
K(b,b)=speye(length(b),length(b)); % put I into boundary submatrix of K
Kb=K; Fb=F; % Stiffness matrix Kb (sparse format) and load vector Fb
 
% Solving for the vector U will produce U(b)=0 at boundary nodes
U=Kb\Fb;  % The FEM approximation is U_1 phi_1 + ... + U_N phi_N
 
% Plot the FEM approximation U(x,y) with values U_1 to U_N at the nodes
trisurf(t,p(:,1),p(:,2),0*p(:,1),U,'edgecolor','k','facecolor','interp');
view(2),axis([-1 1 -1 1]),axis equal,colorbar
 
% Calculate Energy Norms
enorm = 0;
for e=1:T  % integration over one triangular element at a time
  nodes=t(e,:); % row of t = node numbers of the 3 corners of triangle e
  Pe=[ones(3,1),p(nodes,:)]; % 3 by 3 matrix with rows=[1 xcorner ycorner]
  Area=abs(det(Pe))/2; % area of triangle e = half of parallelogram area
%  C=inv(Pe); % columns of C are coeffs in a+bx+cy to give phi=1,0,0 at nodes
  % now compute 3 by 3 Ke and 3 by 1 Fe for element e
  % perform 3 point quadrature over triangle
  lambda = zeros(3,3);
  lambda(1,1) = 2/3;
  lambda(1,2) = 1/6;
  lambda(1,3) = 1/6;
  lambda(2,1) = 1/6;
  lambda(2,2) = 2/3;
  lambda(2,3) = 1/6;
  lambda(3,1) = 1/6;
  lambda(3,2) = 1/6;
  lambda(3,3) = 2/3;
  qpts = lambda*p(nodes(1,[1:3]),:);      % three gaussian quadrature points inside triangle element
  w = ones(3,1)/3;
 
  kvals = feval(k,qpts(:,1),qpts(:,2));
 
  uxvals = feval(ux,qpts(:,1),qpts(:,2));
  uyvals = feval(uy,qpts(:,1),qpts(:,2));
 
  enorm = enorm + Area*((w.*kvals)'*(uxvals.^2 + uyvals.^2));
 
end   % all T element matrices and vectors now assembled into K and F
 
nrm = sqrt(enorm);
 
enorm = 0;
for e=1:T  % integration over one triangular element at a time
  nodes=t(e,:); % row of t = node numbers of the 3 corners of triangle e
  Pe=[ones(3,1),p(nodes,:)]; % 3 by 3 matrix with rows=[1 xcorner ycorner]
  Area=abs(det(Pe))/2; % area of triangle e = half of parallelogram area
 
  av = Pe\U(nodes);
 
  % perform 3 point quadrature over triangle
  lambda = zeros(3,3);
  lambda(1,1) = 2/3;
  lambda(1,2) = 1/6;
  lambda(1,3) = 1/6;
  lambda(2,1) = 1/6;
  lambda(2,2) = 2/3;
  lambda(2,3) = 1/6;
  lambda(3,1) = 1/6;
  lambda(3,2) = 1/6;
  lambda(3,3) = 2/3;
  qpts = lambda*p(nodes(1,[1:3]),:);      % three gaussian quadrature points inside triangle element
  w = ones(3,1)/3;
 
  kvals = feval(k,qpts(:,1),qpts(:,2));
 
  uxvals = feval(ux,qpts(:,1),qpts(:,2));
  uyvals = feval(uy,qpts(:,1),qpts(:,2));
 
  enorm = enorm + Area*((w.*kvals)'*((uxvals - av(2)).^2 + (uyvals - av(3)).^2));
 
end   % all T element matrices and vectors now assembled into K and F
 
err = sqrt(enorm);
 
disp(['Energy Norm: ',num2str(err)]);
disp(['Energy Norm of exact solution: ',num2str(nrm)]);
disp(['Relative energy norm error in computed solution: ',num2str(err/nrm)])
 
u2 = feval(u,p(:,1),p(:,2));
u3 = U - u2;
L2 = norm(u3,2)

print -djpg /tmp/out2.jpg
