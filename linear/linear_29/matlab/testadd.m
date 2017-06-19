X = randn(3,4);
X
[U,S,V] = svds(X);
U,S,V
A = [1; 1; 1]
X2 = [X A]
[Up,Sp,Vp] = addblock_svd_update( U, S, V, A, true)
[U2,S2,V2] = svds(X2);
U2,S2,V2

