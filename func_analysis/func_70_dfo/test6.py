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

    return _minimize_trustregion_constr(fun, x0, args, jac, hess, hessp,
                                        bounds, constraints,
                                        callback=callback, **options)

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
