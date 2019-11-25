from scipy.interpolate import Rbf
from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
import numpy as np
import itertools
import numpy.linalg as lin
import scipy.linalg as slin

from scipy.optimize import (SR1,
                            Bounds,
                            NonlinearConstraint,
                            LinearConstraint)

from scipy.optimize._trustregion_constr.minimize_trustregion_constr import _minimize_trustregion_constr


def rosenbrock(x):
    return (1 - x[0])**2 + 100*(x[1] - x[0]**2)**2

x0 = [-1.0,0]

opts = {'maxiter': 1000, 'verbose': 2}

def minimize(fun, x0, args=(), method=None, jac=None, hess=None,
             hessp=None, bounds=None, constraints=(), tol=None,
             callback=None, options=None):
    meth = 'trust-constr'
    x0 = np.asarray(x0)
    if x0.dtype.kind in np.typecodes["AllInteger"]:
        x0 = np.asarray(x0, dtype=float)
    if not isinstance(args, tuple):
        args = (args,)


    # check gradient vector
    if type(jac) is bool:
        if jac:
            fun = MemoizeJac(fun)
            jac = fun.derivative
        else:
            jac = '2-point'
    elif jac is None:
        jac = '2-point'
    elif not callable(jac) and jac not in ('2-point', '3-point', 'cs'):
        raise ValueError("Unsupported jac definition.")

    # set default tolerances
    if tol is not None:
        options = dict(options)
        options.setdefault('xtol', tol)
        options.setdefault('gtol', tol)
        options.setdefault('barrier_tol', tol)

    if bounds is not None:
        bounds = standardize_bounds(bounds, x0, meth)

    if constraints is not None:
        constraints = standardize_constraints(constraints, x0, meth)

    if meth == '_custom':
        return method(fun, x0, args=args, jac=jac, hess=hess, hessp=hessp,
                      bounds=bounds, constraints=constraints,
                      callback=callback, **options)
    elif meth == 'trust-constr':
        print ('inside _minimize trust-constr')
        return _minimize_trustregion_constr(fun, x0, args, jac, hess, hessp,
                                            bounds, constraints,
                                            callback=callback, **options)


def minimize_scalar(fun, bracket=None, bounds=None, args=(),
                    method='brent', tol=None, options=None):
    """Minimization of scalar function of one variable.

    Parameters
    ----------
    fun : callable
        Objective function.
        Scalar function, must return a scalar.
    bracket : sequence, optional
        For methods 'brent' and 'golden', `bracket` defines the bracketing
        interval and can either have three items ``(a, b, c)`` so that
        ``a < b < c`` and ``fun(b) < fun(a), fun(c)`` or two items ``a`` and
        ``c`` which are assumed to be a starting interval for a downhill
        bracket search (see `bracket`); it doesn't always mean that the
        obtained solution will satisfy ``a <= x <= c``.
    bounds : sequence, optional
        For method 'bounded', `bounds` is mandatory and must have two items
        corresponding to the optimization bounds.
    args : tuple, optional
        Extra arguments passed to the objective function.
    method : str or callable, optional
        Type of solver.  Should be one of:

            - 'Brent'     :ref:`(see here) <optimize.minimize_scalar-brent>`
            - 'Bounded'   :ref:`(see here) <optimize.minimize_scalar-bounded>`
            - 'Golden'    :ref:`(see here) <optimize.minimize_scalar-golden>`
            - custom - a callable object (added in version 0.14.0), see below

    tol : float, optional
        Tolerance for termination. For detailed control, use solver-specific
        options.
    options : dict, optional
        A dictionary of solver options.

            maxiter : int
                Maximum number of iterations to perform.
            disp : bool
                Set to True to print convergence messages.

        See :func:`show_options()` for solver-specific options.

    Returns
    -------
    res : OptimizeResult
        The optimization result represented as a ``OptimizeResult`` object.
        Important attributes are: ``x`` the solution array, ``success`` a
        Boolean flag indicating if the optimizer exited successfully and
        ``message`` which describes the cause of the termination. See
        `OptimizeResult` for a description of other attributes.

    See also
    --------
    minimize : Interface to minimization algorithms for scalar multivariate
        functions
    show_options : Additional options accepted by the solvers

    Notes
    -----
    This section describes the available solvers that can be selected by the
    'method' parameter. The default method is *Brent*.

    Method :ref:`Brent <optimize.minimize_scalar-brent>` uses Brent's
    algorithm to find a local minimum.  The algorithm uses inverse
    parabolic interpolation when possible to speed up convergence of
    the golden section method.

    Method :ref:`Golden <optimize.minimize_scalar-golden>` uses the
    golden section search technique. It uses analog of the bisection
    method to decrease the bracketed interval. It is usually
    preferable to use the *Brent* method.

    Method :ref:`Bounded <optimize.minimize_scalar-bounded>` can
    perform bounded minimization. It uses the Brent method to find a
    local minimum in the interval x1 < xopt < x2.

    **Custom minimizers**

    It may be useful to pass a custom minimization method, for example
    when using some library frontend to minimize_scalar.  You can simply
    pass a callable as the ``method`` parameter.

    The callable is called as ``method(fun, args, **kwargs, **options)``
    where ``kwargs`` corresponds to any other parameters passed to `minimize`
    (such as `bracket`, `tol`, etc.), except the `options` dict, which has
    its contents also passed as `method` parameters pair by pair.  The method
    shall return an `OptimizeResult` object.

    The provided `method` callable must be able to accept (and possibly ignore)
    arbitrary parameters; the set of parameters accepted by `minimize` may
    expand in future versions and then these parameters will be passed to
    the method.  You can find an example in the scipy.optimize tutorial.

    .. versionadded:: 0.11.0

    Examples
    --------
    Consider the problem of minimizing the following function.

    >>> def f(x):
    ...     return (x - 2) * x * (x + 2)**2

    Using the *Brent* method, we find the local minimum as:

    >>> from scipy.optimize import minimize_scalar
    >>> res = minimize_scalar(f)
    >>> res.x
    1.28077640403

    Using the *Bounded* method, we find a local minimum with specified
    bounds as:

    >>> res = minimize_scalar(f, bounds=(-3, -1), method='bounded')
    >>> res.x
    -2.0000002026

    """
    if not isinstance(args, tuple):
        args = (args,)

    if callable(method):
        meth = "_custom"
    else:
        meth = method.lower()
    if options is None:
        options = {}

    if tol is not None:
        options = dict(options)
        if meth == 'bounded' and 'xatol' not in options:
            warn("Method 'bounded' does not support relative tolerance in x; "
                 "defaulting to absolute tolerance.", RuntimeWarning)
            options['xatol'] = tol
        elif meth == '_custom':
            options.setdefault('tol', tol)
        else:
            options.setdefault('xtol', tol)

    if meth == '_custom':
        return method(fun, args=args, bracket=bracket, bounds=bounds, **options)
    elif meth == 'brent':
        return _minimize_scalar_brent(fun, bracket, args, **options)
    elif meth == 'bounded':
        if bounds is None:
            raise ValueError('The `bounds` parameter is mandatory for '
                             'method `bounded`.')
        # replace boolean "disp" option, if specified, by an integer value, as
        # expected by _minimize_scalar_bounded()
        disp = options.get('disp')
        if isinstance(disp, bool):
            options['disp'] = 2 * int(disp)
        return _minimize_scalar_bounded(fun, bounds, args, **options)
    elif meth == 'golden':
        return _minimize_scalar_golden(fun, bracket, args, **options)
    else:
        raise ValueError('Unknown solver %s' % method)


def standardize_bounds(bounds, x0, meth):
    """Converts bounds to the form required by the solver."""
    if meth == 'trust-constr':
        if not isinstance(bounds, Bounds):
            lb, ub = old_bound_to_new(bounds)
            bounds = Bounds(lb, ub)
    elif meth in ('l-bfgs-b', 'tnc', 'slsqp'):
        if isinstance(bounds, Bounds):
            bounds = new_bounds_to_old(bounds.lb, bounds.ub, x0.shape[0])
    return bounds


def standardize_constraints(constraints, x0, meth):
    """Converts constraints to the form required by the solver."""
    all_constraint_types = (NonlinearConstraint, LinearConstraint, dict)
    new_constraint_types = all_constraint_types[:-1]
    if isinstance(constraints, all_constraint_types):
        constraints = [constraints]
    constraints = list(constraints)  # ensure it's a mutable sequence

    if meth == 'trust-constr':
        for i, con in enumerate(constraints):
            if not isinstance(con, new_constraint_types):
                constraints[i] = old_constraint_to_new(i, con)
    else:
        # iterate over copy, changing original
        for i, con in enumerate(list(constraints)):
            if isinstance(con, new_constraint_types):
                old_constraints = new_constraint_to_old(con, x0)
                constraints[i] = old_constraints[0]
                constraints.extend(old_constraints[1:])  # appends 1 if present

    return constraints



res = minimize (fun=rosenbrock,
                x0=x0,
                method = 'trust-constr',
                jac = "2-point",
                hess = SR1 (),
                bounds=Bounds([0.3, 0.5], [0.0, 2.0]),
                options=opts)

print (res)
