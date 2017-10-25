from keras import backend as K
import numpy as np

def _keras_unstack_hack(ab):
    ndim = len(K.int_shape(ab))
    if ndim == 0:
        print('can not unstack with ndim=0')
    else:
        a = ab[..., 0]
        b = ab[..., 1]
    return a, b

def output_lambda(x, init_alpha=1.0, max_beta_value=5.0, max_alpha_value=None):
    a, b = _keras_unstack_hack(x)
    if max_alpha_value is None:
        a = init_alpha * K.exp(a)
    else:
        a = init_alpha * K.clip(x=a, min_value=K.epsilon(),max_value=max_alpha_value)
        
    m = max_beta_value
    
    if m > 1.05:
        _shift = np.log(m - 1.0)
        b = K.sigmoid(b - _shift)
    else:
        b = K.sigmoid(b)

    b = m * K.clip(x=b, min_value=K.epsilon(), max_value=1. - K.epsilon())
    x = K.stack([a, b], axis=-1)
    return x


def weibull_loss_discrete(y_true, y_pred, name=None):
    y,u = _keras_unstack_hack(y_true)
    a,b = _keras_unstack_hack(y_pred)
    hazard0 = K.pow((y + 1e-35) / a, b)
    hazard1 = K.pow((y + 1.0) / a, b)    
    loglikelihoods = u * K.log(K.exp(hazard1 - hazard0) - 1.0) - hazard1
    loss = -1 * K.mean(loglikelihoods)
    return loss

