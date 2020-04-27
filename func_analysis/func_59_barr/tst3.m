
A = [[1.  1. 1. 0.]
     [1.  3. 0. 1.]
     [9.  1. -3. 1.]
    ]

b = [5.,7.,-1]'

c = [-1., -5., 0., 0. ]'

[x_star,p_star,gap,status,nsteps] = lp_solve(A,b,c);

x_star
