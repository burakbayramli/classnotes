from __future__ import absolute_import

import scipy as sp
import itertools
import warnings
from scipy.optimize import rosen, rosen_der
import scipy
import numpy as np
import scipy.linalg
import scipy.optimize

class Rosenbrock(object):

    def __init__(self):
        self.pars = np.array([-1.0,0])

    def f(self, x):
        return rosen(x)

    def fprime(self, x):
        return rosen_der(x)

    def f_Hp(self, pars, p):
        eps = 1E-6
        return (self.fprime(pars + p * eps) - self.fprime(pars)) / eps

    def solved(self, tolerance=0.1):
        return abs(self.pars - [1, 1]).mean() < tolerance

class Minimizer(object):

    def __init__(self, wrt, args=None):
        self.wrt = wrt
        if args is None:
            self.args = itertools.repeat(([], {}))
        else:
            self.args = args

        self.n_iter = 0

    def set_from_info(self, info):

        for f in self.state_fields:
            self.__dict__[f] = info[f]

    def extended_info(self, **kw):
        """Return a dictionary populated with the values of the state fields.
        Further values can be given as keyword arguments.

        Parameters
        ----------

        **kw : dict
            Arbitrary data to place into dictionary.

        Returns
        -------

        dct : dict
            Contains all attributes of the class given by the ``state_fields``
            attribute. Additionally updated with elements from ``kw``.
        """
        dct = dict((f, getattr(self, f)) for f in self.state_fields)
        dct.update(kw)
        return dct

    def minimize_until(self, criterions):
        if not criterions:
            raise ValueError('need to supply at least one criterion')

        # if criterions is a single criterion, wrap it in iterable list
        if not isinstance(criterions, collections.Iterable):
            criterions = [criterions]

        info = {}
        for info in self:
            for criterion in criterions:
                if criterion(info):
                    return info
        return info

    def __iter__(self):
        for info in self._iterate():
            yield self.extended_info(**info)


def is_nonzerofinite(arr):
    """Return True if the array is neither zero, NaN or infinite."""
    return (arr != 0).any() and np.isfinite(arr).all()

def wolfe_line_search(x, t, d, f, g, gtd,
        c1, c2, LS, maxLS, tolX, funObj):

        #Evaluate the Objective and Gradient at the Initial Step
        f_new, g_new = funObj(x+t*d)
        funEvals = 1

        gtd_new = np.dot(g_new, d)

        # Bracket an intervail containing a point
        # satisfying the wolfe criteria
        LSiter = 0
        t_prev = 0
        f_prev = f
        g_prev = g
        gtd_prev = gtd
        done = False

        while LSiter < maxLS:

            # Bracketing phase
            if not isLegal(f_new) or not isLegal(g_new):
                t = (t + t_prev)/2.
                # missing: if 0 in minFunc!!
                #
                # Extrapolated into illegal region, switching
                # to Armijo line search
                # no Hessian is computed!!
                t, x_new, f_new, g_new, _fevals = armijobacktrack(
                    x, t, d, f, f, g, gtd, c1, max(0, min(LS-2, 2)), tolX,
                    funObj)
                funEvals += _fevals
                return t, f_new, g_new, funEvals
            #
            if (f_new > f + c1*t*gtd) or (LSiter > 1 and f_new >= f_prev):
                bracket = [t_prev, t]
                bracketFval = [f_prev, f_new]
                # check here: two gradients next to each other, in columns
                bracketGval = np.array([g_prev, g_new])
                break
            elif abs(gtd_new) <= -c2*gtd:
                bracket = np.array([t])
                bracketFval = np.array([f_new])
                bracketGval = np.array([g_new])
                done = True
                break
            elif gtd_new >= 0:
                bracket = [t_prev, t]
                bracketFval = [f_prev, f_new]
                # check here (again), see above
                bracketGval = np.array([g_prev, g_new])
                break
            temp = t_prev
            t_prev = t
            minStep = t + 0.01*(t-temp)
            maxStep = t*10
            f_prev = f_new
            g_prev = g_new
            gtd_prev = gtd_new
            #
            # no saveHessian stuff!!!
            f_new, g_new = funObj(x + t*d)
            funEvals += 1
            gtd_new = np.inner(g_new, d)
            LSiter += 1
        # while ....
        #
        if LSiter == maxLS:
            bracket = [0, t]
            bracketFval = [f, f_new]
            # check here, same again!
            bracketGval = np.array([g, g_new])

        # Zoom Phase:
        # We now either have point satisfying the criteria
        # or a bracket surrounding a point satisfying the criteria.
        # Refine the bracket until we find a point satifying the criteria.
        #
        insufProgress = False
        # Next line needs a check!!!!!
        Tpos = 1
        LOposRemoved = False
        while not done and LSiter < maxLS:
            # Find high and low points in the bracket
            # check here, axees needed??
            f_LO = np.min(bracketFval)
            LOpos = np.argmin(bracketFval)
            HIpos = 1 - LOpos
            #
            # Compute new trial value
            if LS == 3 or not isLegal(bracketFval) or not isLegal(bracketGval):
                # Bisecting
                t = np.mean(bracket)
            elif LS == 4:
                # Grad cubic interpolation
                t, _ = polyinterp(
                    np.array(
                        [[bracket[0], bracketFval[0], np.dot(bracketGval[0], d)],
                         [bracket[1], bracketFval[1], np.dot(bracketGval[1], d)]]))
            else:
                # Mixed case
                # Is this correct ???????
                nonTpos = 1 - Tpos
                if not LOposRemoved:
                    oldLOval = bracket[nonTpos]
                    oldLOFval = bracketFval[nonTpos]
                    oldLOGval = bracketGval[nonTpos]
                t = mixedInterp(
                        bracket, bracketFval, bracketGval, d, Tpos, oldLOval,
                        oldLOFval, oldLOGval)

            #
            # Test that we are making sufficient progress
            bracket_min = min(bracket)
            bracket_max = max(bracket)

            if min(bracket_max - t, t - bracket_min) / (bracket_max - bracket_min) < 0.1:
                # Interpolation close to boundary
                if insufProgress or (t >= np.max(bracket)) or (t <= np.min(bracket)):
                    # Evaluating at 0.1 away from boundary
                    if np.abs(t - np.max(bracket)) < np.abs(t - np.min(bracket)):
                        t = np.max(bracket) - 0.1 * (np.max(bracket) - np.min(bracket))
                    else:
                        t = np.min(bracket) + 0.1 * (np.max(bracket) - np.min(bracket))
                    insufProgress = False
                #
                else:
                    insufProgress = True
            #
            else:
                insufProgress = False

            # Evaluate new point
            # no Hessian!
            t = scipy.real(t)
            f_new, g_new = funObj(x + t * d)
            funEvals += 1
            gtd_new = np.dot(g_new, d)
            LSiter += 1

            if f_new > f + c1 * t * gtd or f_new >= f_LO:
                # Armijo condition not satisfied or
                # not lower than lowest point
                bracket[HIpos] = t
                bracketFval[HIpos] = f_new
                bracketGval[HIpos] = g_new
                Tpos = HIpos
            else:
                if np.abs(gtd_new) <= -c2 * gtd:
                    # Wolfe conditions satisfied
                    done = True
                elif gtd_new * (bracket[HIpos] - bracket[LOpos]) >= 0:
                    # old HI becomes new LO
                    bracket[HIpos] = bracket[LOpos]
                    bracketFval[HIpos] = bracketFval[LOpos]
                    bracketGval[HIpos] = bracketGval[LOpos]
                    if LS == 5:
                        # LO Pos is being removed
                        LOposRemoved = True
                        oldLOval = bracket[LOpos]
                        oldLOFval = bracketFval[LOpos]
                        oldLOGval = bracketGval[LOpos]
                    #

                # New point becomes new LO
                bracket[LOpos] = t
                bracketFval[LOpos] = f_new
                bracketGval[LOpos] = g_new
                Tpos = LOpos

            if not done and np.abs(bracket[0] - bracket[1]) * gtd_new < tolX:
                # Line search can not make further progress
                break
        # while ...

        # TODO a comment here maybe nice
        if LSiter == maxLS:
            # could give info:
            # Line Search exceeded maximum line search iterations
            # TODO: what to do here?
            pass
        #
        # check if axes necessary?
        f_LO = np.min(bracketFval)
        LOpos = np.argmin(bracketFval)
        t = bracket[LOpos]
        f_new = bracketFval[LOpos]
        g_new = bracketGval[LOpos]

        # missing Hessain evaluation
        return t, f_new, g_new, funEvals

def isLegal(v):
    """
    Do exactly that.
    """
    return not (np.any(np.iscomplex(v)) or
                np.any(np.isnan(v)) or
                np.any(np.isinf(v)))


def polyinterp(points, xminBound=None, xmaxBound=None):
    """
    Minimum of interpolating polynomial based
    on function and derivative values.

    Note: doPlot from minFunc missing!!!
    """
    nPoints = points.shape[0]
    order = np.sum(np.isreal(points[:, 1:3])) - 1

    # Code for most common case:
    # - cubic interpolation of 2 points
    #   w/ function and derivative values for both
    # - no xminBound/xmaxBound

    if nPoints == 2 and order == 3 and xminBound is None and xmaxBound is None:
        # Solution in this case (where x2 is the farthest point):
        #    d1 = g1 + g2 - 3*(f1-f2)/(x1-x2);
        #    d2 = sqrt(d1^2 - g1*g2);
        #    minPos = x2 - (x2 - x1)*((g2 + d2 - d1)/(g2 - g1 + 2*d2));
        #    t_new = min(max(minPos,x1),x2);
        minVal = np.min(points[:, 0])
        minPos = np.argmin(points[:, 0])
        notMinPos = 1 - minPos

        x1 = points[minPos, 0]
        x2 = points[notMinPos, 0]
        g1 = points[minPos, 2]
        g2 = points[notMinPos, 2]
        f1 = points[minPos, 1]
        f2 = points[notMinPos, 1]

        d1 = g1 + g2 - 3 * (f1 - f2) / (x1 - x2)
        d2 = sp.sqrt(d1 ** 2 - g1 * g2)
        if np.isreal(d2):
            t = points[notMinPos, 0] -\
                    (points[notMinPos, 0] - points[minPos, 0]) * \
                    (
                      (points[notMinPos, 2] + d2 - d1) /
                      (points[notMinPos, 2] - points[minPos, 2] + 2 * d2)
                    )
            minPos = np.minimum(
                np.maximum(t, points[minPos, 0]), points[notMinPos, 0])

        else:
            minPos = np.mean(points[:, 0])
        # fmin is not returned here
        return minPos, None
    #
    #
    xmin = np.min(points[:, 0])
    xmax = np.max(points[:, 0])
    # Compute Bouns of Interpolation Area
    if xminBound is None:
        xminBound = xmin
    if xmaxBound is None:
        xmaxBound = xmax
    #
    #
    # Collect constraints for parameter estimation
    A = np.zeros((2 * nPoints, order + 1))
    b = np.zeros((2 * nPoints, 1))
    # Constraints based on available function values
    for i in range(points.shape[0]):
        if np.isreal(points[i, 1]):
            A[i] = [points[i, 0] ** (order - j) for j in range(order + 1)]
            b[i] = points[i, 1]
            points[i, 0], points[i, 1]
    # Constraints based on available derivatives
    for i, p in enumerate(points[:, 2]):
        if np.isreal(p):
            A[nPoints + i] = [(order - j + 1) * points[i, 0] ** (order - j)
                              for j in range(1, order + 1)] + [0]
            b[nPoints + i] = points[i, 2]
    #
    # Find interpolating polynomial
    params = np.linalg.lstsq(A, b)[0].flatten()

    # Compute critical points
    dParams = [(order - j) * params[j] for j in range(order)]

    cp = [xminBound, xmaxBound] + list(points[:, 0])
    if not np.any(np.isinf(dParams)):
        cp += list(np.roots(dParams))

    # Test critical points
    fmin = np.inf
    # Default to bisection if no critical points are valid
    minPos = (xminBound + xmaxBound) / 2.
    for x in cp:
        if np.isreal(x) and x >= xminBound and x <= xmaxBound:
            fx = np.polyval(params, x)
            if np.isreal(fx) and fx <= fmin:
                minPos = x
                fmin = fx
    return minPos, fmin

class LineSearch(object):

    def __init__(self, wrt):
        self.wrt = wrt

    def search(self, direction, initialization, args=None, kwargs=None):
        raise NotImplemented()


class WolfeLineSearch(LineSearch):
    """Port of Mark Schmidt's line search."""

    def __init__(self, wrt, f, fprime, c1=1E-4, c2=0.9, maxiter=25,
                 min_step_length=1E-9, typ=4):
        super(WolfeLineSearch, self).__init__(wrt)
        self.f = f
        self.fprime = fprime
        self.c1 = c1
        self.c2 = c2
        self.maxiter = maxiter
        self.min_step_length = min_step_length
        self.typ = typ

        # TODO: find better API for this
        self.first_try = True

    def search(self, direction, initialization=None, args=None, kwargs=None,
               loss0=None):
        args = args if args is not None else ()
        kwargs = kwargs if kwargs is not None else {}
        loss0 = self.f(self.wrt, *args, **kwargs) if loss0 is None else loss0
        grad0 = self.fprime(self.wrt, *args, **kwargs)
        direct_deriv0 = scipy.inner(grad0, direction)
        f = lambda x: (self.f(x, *args, **kwargs),
                       self.fprime(x, *args, **kwargs))

        if self.first_try:
            self.first_try = False
            t = min(1, 1 / sum(abs(grad0)))
        else:
            t = initialization if initialization is not None else 1

        step, fstep, fprimestep, n_evals = wolfe_line_search(
            self.wrt, t, direction, loss0, grad0, direct_deriv0,
            self.c1, self.c2, self.typ, self.maxiter, self.min_step_length,
            f)

        self.val = fstep
        self.grad = fprimestep

        return step


class Bfgs(Minimizer):
    def __init__(self, wrt, f, fprime, initial_inv_hessian=None,
                 line_search=None, args=None):

        super(Bfgs, self).__init__(wrt, args=args)
        self.f = f
        self.fprime = fprime
        self.inv_hessian = initial_inv_hessian

        if line_search is not None:
            self.line_search = line_search
        else:
            self.line_search = WolfeLineSearch(wrt, self.f, self.fprime)

    def set_from_info(self, info):
        raise NotImplemented('nobody has found the time to implement this yet')

    def extended_info(self, **kwargs):
        raise NotImplemented('nobody has found the time to implement this yet')

    def find_direction(self, grad_m1, grad, step, inv_hessian):
        H = self.inv_hessian
        grad_diff = grad - grad_m1
        ys = np.inner(grad_diff, step)
        Hy = np.dot(H, grad_diff)
        yHy = np.inner(grad_diff, Hy)
        H += (ys + yHy) * np.outer(step, step) / ys ** 2
        H -= (np.outer(Hy, step) + np.outer(step, Hy)) / ys
        direction = -np.dot(H, grad)
        return direction, {'gradient_diff': grad_diff}

    def __iter__(self):
        args, kwargs = next(self.args)
        grad = self.fprime(self.wrt, *args, **kwargs)
        grad_m1 = scipy.zeros(grad.shape)

        if self.inv_hessian is None:
            self.inv_hessian = scipy.eye(grad.shape[0])

        for i, (next_args, next_kwargs) in enumerate(self.args):
            if i == 0:
                direction, info = -grad, {}
            else:
                direction, info = self.find_direction(
                    grad_m1, grad, step, self.inv_hessian)

            if not is_nonzerofinite(direction):
                # TODO: inform the user here.
                break

            step_length = self.line_search.search(
                direction, None, args, kwargs)

            if step_length != 0:
                step = step_length * direction
                self.wrt += step
                print ('we are at', self.wrt)
            else:
                self.logfunc(
                    {'message': 'step length is 0--need to bail out.'})
                break

            # Prepare everything for the next loop.
            args, kwargs = next_args, next_kwargs
            # TODO: not all line searches have .grad!
            grad_m1[:], grad[:] = grad, self.line_search.grad

            info.update({
                'step_length': step_length,
                'n_iter': i,
                'args': args,
                'kwargs': kwargs,
            })
            yield info



obj = Rosenbrock()
print (obj.pars)
opt = Bfgs(obj.pars, obj.f, obj.fprime)            
for i, info in enumerate(opt):
    print (info)
print (obj.solved())
