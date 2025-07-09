import autograd.numpy as np
import autograd.numpy.random as npr
from autograd import grad
from autograd.scipy.misc import logsumexp
from os.path import dirname, join
from builtins import range

def sigmoid(x): return 0.5*(np.tanh(x) + 1.0)   

def concat_and_multiply(weights, *args):
    cat_state = np.hstack(args + (np.ones((args[0].shape[0], 1)),))
    return np.dot(cat_state, weights)

def create_rnn_params(input_size, state_size, output_size,
                      param_scale=0.01, rs=npr.RandomState(0)):
    return {'init hiddens': rs.randn(1, state_size) * param_scale,
            'change':       rs.randn(input_size + state_size + 1,
                                     state_size) * param_scale,
            'predict':      rs.randn(state_size + 1, output_size) * param_scale}

def rnn_predict(params, inputs):
    def update_rnn(input, hiddens):
        return np.tanh(concat_and_multiply(params['change'], input, hiddens))

    def hiddens_to_output_probs(hiddens):
        output = concat_and_multiply(params['predict'], hiddens)
        return output - logsumexp(output, axis=1, keepdims=True) 

    num_sequences = inputs.shape[1]
    hiddens = np.repeat(params['init hiddens'], num_sequences, axis=0)
    output = [hiddens_to_output_probs(hiddens)]

    for input in inputs:  # Iterate over time steps.
        hiddens = update_rnn(input, hiddens)
        output.append(hiddens_to_output_probs(hiddens))
    return output

def string_to_one_hot(string, maxchar):
    ascii = np.array([ord(c) for c in string]).T
    return np.array(ascii[:,None] == np.arange(maxchar)[None, :], dtype=int)

def one_hot_to_string(one_hot_matrix):
    return "".join([chr(np.argmax(c)) for c in one_hot_matrix])

def rnn_log_likelihood(params, inputs, targets):
    logprobs = rnn_predict(params, inputs)
    loglik = 0.0
    num_time_steps, num_examples, _ = inputs.shape
    for t in range(num_time_steps):
        loglik += np.sum(logprobs[t] * targets[t])
    return loglik / (num_time_steps * num_examples)

def training_loss(params, iter):
    return -rnn.rnn_log_likelihood(params, train_inputs, train_inputs)
