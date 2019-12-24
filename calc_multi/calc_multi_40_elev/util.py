import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

epsilon = np.sqrt(np.finfo(float).eps)

def _approx_fprime_helper(xk, f):
    f0 = f(xk)
    grad = np.zeros((len(xk),), float)
    ei = np.zeros((len(xk),), float)
    for k in range(len(xk)):
        ei[k] = 1.0
        d = epsilon * ei
        df = (f(xk + d) - f0) / d[k]
        if not np.isscalar(df):
            try:
                df = df.item()
            except (ValueError, AttributeError):
                raise ValueError("The user-provided "
                                 "objective function must "
                                 "return a scalar value.")
        grad[k] = df
        ei[k] = 0.0
    return grad
