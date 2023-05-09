import numpy as np
from scipy.optimize import linprog
from numpy.linalg import matrix_rank

def solve(c, A, b, epsilon=0.0001):
    if matrix_rank(A) < min(A.shape[0], A.shape[1]):
        print('A is not full rank, dropping redundant rows')
        _, pivots = sympy.Matrix(A).T.rref()
        A = A[list(pivots)]
        print('Shape of A after dropping redundant rows is {}'.format(A.shape))

    m = A.shape[0]
    n = A.shape[1]

    x = np.ones(shape=(n, ))
    l = np.ones(shape=(m, ))
    s = np.ones(shape=(n, ))

    k = 0

    while abs(np.dot(x, s)) > epsilon:        
        k += 1
        primal_obj = np.dot(c, x)
        dual_obj = np.dot(b, l)
        print('iteration #{}; primal_obj = {:.5f}, dual_obj = {:.5f}; duality_gap = {:.5f}'.format
              (k, primal_obj, dual_obj, primal_obj - dual_obj)) 
        sigma_k = 0.4
        mu_k = np.dot(x, s) / n

        A_ = np.zeros(shape=(m + n + n, n + m + n))
        A_[0:m, 0:n] = np.copy(A)
        A_[m:m + n, n:n + m] = np.copy(A.T)
        A_[m:m + n, n + m:n + m + n] = np.eye(n)
        A_[m + n:m + n + n, 0:n] = np.copy(np.diag(s))
        A_[m + n:m + n + n, n + m:n + m + n] = np.copy(np.diag(x))

        b_ = np.zeros(shape=(n + m + n, ))
        b_[0:m] = np.copy(b - np.dot(A, x))
        b_[m:m + n] = np.copy(c - np.dot(A.T, l) - s)
        tmp = np.dot(np.dot(np.diag(x), np.diag(s)), np.ones(shape=(n, )))
        b_[m + n:m + n + n] = np.copy( sigma_k * mu_k * np.ones(shape=(n, )) - tmp )

        delta = np.linalg.solve(A_, b_)
        delta_x = delta[0:n]
        delta_l = delta[n:n + m]
        delta_s = delta[n + m:n + m + n]

        alpha_max = 1.0
        for i in range(n):
            if delta_x[i] < 0:
                alpha_max = min(alpha_max, -x[i]/delta_x[i])
            if delta_s[i] < 0:
                alpha_max = min(alpha_max, -s[i]/delta_s[i])
        eta_k = 0.99
        alpha_k = min(1.0, eta_k * alpha_max)

        x = x + alpha_k * delta_x
        l = l + alpha_k * delta_l
        s = s + alpha_k * delta_s

    diff = np.dot(A, x) - b
    print('Ax - b = {}; ideally it should have been zero vector'.format(diff))
    print('norm of Ax - b is = {}; ideally it should have been zero'.format
          (np.linalg.norm(diff)))

    return x

A = np.array([[1,  1, 1, 0],
              [1,  3, 0, 1]])

b = np.array([5,7])

c = np.array([-1, -5, 0, 0 ])
            
res = solve(c,A,b)
print (res)

res = linprog(c, A_eq=A, b_eq=b, options={"disp": True})

print (res)

