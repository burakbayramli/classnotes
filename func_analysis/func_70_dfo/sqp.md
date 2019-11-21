
Method :ref:`trust-constr <optimize.minimize-trustconstr>` is a
trust-region algorithm for constrained optimization. It swiches
between two implementations depending on the problem definition.  It
is the most versatile constrained minimization algorithm implemented
in SciPy and the most appropriate for large-scale problems.  For
equality constrained problems it is an implementation of Byrd-Omojokun
Trust-Region SQP method described in [17]_ and in [5]_, p. 549. When
inequality constraints are imposed as well, it swiches to the
trust-region interior point method described in [16]_. This interior
point algorithm, in turn, solves inequality constraints by introducing
slack variables and solving a sequence of equality-constrained barrier
problems for progressively smaller values of the barrier parameter.
The previously described equality constrained SQP method is used to
solve the subproblems with increasing levels of accuracy as the
iterate gets closer to a solution.

    .. [5] Nocedal, J, and S J Wright. 2006. Numerical Optimization.
       Springer New York.
       
    .. [17] Lalee, Marucha, Jorge Nocedal, and Todd Plantega. 1998. On the
        implementation of an algorithm for large-scale equality constrained
        optimization. SIAM Journal on Optimization 8.3: 682-706.

    .. [16] Byrd, Richard H., Mary E. Hribar, and Jorge Nocedal. 1999.
        An interior point algorithm for large-scale nonlinear  programming.
        SIAM Journal on Optimization 9.4: 877-900.

