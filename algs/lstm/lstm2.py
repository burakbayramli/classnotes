"""Implements the long-short term memory character model.
This version vectorizes over multiple examples, but each string
has a fixed length."""

from __future__ import absolute_import
from builtins import range
from os.path import dirname, join
import autograd.numpy as np
import autograd.numpy.random as npr
from autograd import grad
from autograd.scipy.misc import logsumexp

from autograd.optimizers import adam

def sigmoid(x):
    return 0.5*(np.tanh(x) + 1.0)   # Output ranges from 0 to 1.

def concat_and_multiply(weights, *args):
    cat_state = np.hstack(args + (np.ones((args[0].shape[0], 1)),))
    return np.dot(cat_state, weights)

def init_lstm_params(input_size, state_size, output_size,
                     param_scale=0.01, rs=npr.RandomState(0)):
    def rp(*shape):
        return rs.randn(*shape) * param_scale

    return {'init cells':   rp(1, state_size),
            'init hiddens': rp(1, state_size),
            'change':       rp(input_size + state_size + 1, state_size),
            'forget':       rp(input_size + state_size + 1, state_size),
            'ingate':       rp(input_size + state_size + 1, state_size),
            'outgate':      rp(input_size + state_size + 1, state_size),
            'predict':      rp(state_size + 1, output_size)}

def lstm_predict(params, inputs):
    def update_lstm(input, hiddens, cells):
        change  = np.tanh(concat_and_multiply(params['change'], input, hiddens))
        forget  = sigmoid(concat_and_multiply(params['forget'], input, hiddens))
        ingate  = sigmoid(concat_and_multiply(params['ingate'], input, hiddens))
        outgate = sigmoid(concat_and_multiply(params['outgate'], input, hiddens))
        cells   = cells * forget + ingate * change
        hiddens = outgate * np.tanh(cells)
        return hiddens, cells

    def hiddens_to_output_probs(hiddens):
        output1 = concat_and_multiply(params['predict'], hiddens).T
        output2 = logsumexp(output1, axis=1, keepdims=True).T 
        return output1 - output2

    num_sequences = inputs.shape[1]
    hiddens = np.repeat(params['init hiddens'], num_sequences, axis=0)
    cells   = np.repeat(params['init cells'],   num_sequences, axis=0)

    output = [hiddens_to_output_probs(hiddens)]
    for input in inputs:  # Iterate over time steps.
        hiddens, cells = update_lstm(input, hiddens, cells)
        output.append(hiddens_to_output_probs(hiddens))
    return output

def lstm_log_likelihood(params, inputs, targets):
    logprobs = lstm_predict(params, inputs)
    loglik = 0.0
    num_time_steps, num_examples, _ = inputs.shape
    for t in range(num_time_steps):
        loglik += np.sum(logprobs[t] * targets[t])
    return loglik / (num_time_steps * num_examples)

np.random.seed(1)

def f(t):
    return t * np.sin(t) / 3 + 2 * np.sin(t*5)

def next_batch(batch_size, n_steps):
    t_min, t_max = 0, 30
    resolution = 0.1
    t0 = np.random.rand(batch_size, 1) * (t_max - t_min - n_steps * resolution) # 
    Ts = t0 + np.arange(0., n_steps + 1) * resolution
    ys = f(Ts)
    Xa = ys[:, :-1].reshape(-1, n_steps, 1)
    ya = ys[:, 1:].reshape(-1, n_steps, 1)
    return np.array(Xa),np.array(ya)

if __name__ == '__main__':

    X,y = next_batch(20,30)
    print X.shape, y.shape
    
    init_params = init_lstm_params(input_size=1, output_size=1,
                                   state_size=40, param_scale=0.001)

    def training_loss(params, iter): return -lstm_log_likelihood(params, X, y)

    def callback(weights, iter, gradient):
        if iter % 10 == 0:
        print("Iteration", iter, "Train loss:", training_loss(weights, 0))    
    
    training_loss_grad = grad(training_loss)
    
    trained_params = adam(training_loss_grad, init_params,
                          step_size=0.01, num_iters=54,
                          callback=callback)


    
