
u = [1 2 3 4;
     5 6 7 8;
     1 2 3 4;
     5 6 7 8];

M=4;N=4;

D = spdiags([-ones(M,1) ones(M,1)],[0 1],M,M);
D(M,:) = 0;

Dyu = D*u;
Dxu = u*D';

Dxu
Dyu
