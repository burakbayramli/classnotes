import numpy as np
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers import Activation
from keras.layers import Masking
from keras.optimizers import RMSprop
from keras import backend as k
from sklearn.preprocessing import normalize

# Configurable observation look-back period for each engine/day
max_time = 100

"""
Discrete log-likelihood for Weibull hazard function on censored survival data

y_true is a (samples, 2) tensor containing time-to-event (y), and
an event indicator (u)

ab_pred is a (samples, 2) tensor containing predicted Weibull
alpha (a) and beta (b) parameters

For math, see thesis (Page 35)

"""
def weibull_loglik_discrete(y_true, ab_pred, name=None):
    y_ = y_true[:, 0]
    u_ = y_true[:, 1]
    a_ = ab_pred[:, 0]
    b_ = ab_pred[:, 1]

    hazard0 = k.pow((y_ + 1e-35) / a_, b_)
    hazard1 = k.pow((y_ + 1) / a_, b_)

    return -1 * k.mean(u_ * k.log(k.exp(hazard1 - hazard0) - 1.0) - hazard1)

"""
    Not used for this model, but included in case somebody needs it
    For math, see thesis (Page 35)
"""
def weibull_loglik_continuous(y_true, ab_pred, name=None):
    y_ = y_true[:, 0]
    u_ = y_true[:, 1]
    a_ = ab_pred[:, 0]
    b_ = ab_pred[:, 1]

    ya = (y_ + 1e-35) / a_
    return -1 * k.mean(u_ * (k.log(b_) + b_ * k.log(ya)) - k.pow(ya, b_))


"""Custom Keras activation function, outputs alpha neuron using
    exponentiation and beta using softplus
"""
def activate(ab):
    a = k.exp(ab[:, 0])
    b = k.softplus(ab[:, 1])

    a = k.reshape(a, (k.shape(a)[0], 1))
    b = k.reshape(b, (k.shape(b)[0], 1))

    return k.concatenate((a, b), axis=1)

np.set_printoptions(suppress=True, threshold=10000)

def get_model():
    # Start building our model
    model = Sequential()

    # Mask parts of the lookback period that are all zeros (i.e.,
    # unobserved) so they don't skew the model
    model.add(Masking(mask_value=0., input_shape=(max_time, 24)))

    # LSTM is just a common type of RNN. You could also try anything else
    # (e.g., GRU).
    model.add(LSTM(20, input_dim=24))

    # We need 2 neurons to output Alpha and Beta parameters for our
    # Weibull distribution
    model.add(Dense(2))

    # Apply the custom activation function mentioned above
    model.add(Activation(activate))

    return model


if __name__ == "__main__": 
 
    train_x = np.load("./data/train_x.npy")
    train_y = np.load("./data/train_y.npy")
    test_x = np.load("./data/test_x.npy")
    test_y = np.load("./data/test_y.npy")
    
    model = get_model()

    # Use the discrete log-likelihood for Weibull survival data as our
    # loss function
    model.compile(loss=weibull_loglik_discrete, optimizer=RMSprop(lr=.001))

    # Fit!
    model.fit(train_x,
              train_y,
              nb_epoch=100,
              batch_size=2000,
              verbose=2,
              validation_data=(test_x, test_y))

    model.save('wtte.h5')

    # Make some predictions and put them alongside the real TTE and event
    # indicator values
    test_predict = model.predict(test_x)
    test_predict = np.resize(test_predict, (100, 2))
    test_result = np.concatenate((test_y, test_predict), axis=1)

    # TTE, Event Indicator, Alpha, Beta
    print(test_result)
