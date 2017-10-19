# sudo pip install keras==2.0.8
# sudo pip install theano
import numpy as np
import matplotlib.pyplot as plt
import tensorflow as tf
from keras import backend as K
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM,GRU
from keras.layers import Lambda
from keras.layers.wrappers import TimeDistributed
from keras.optimizers import RMSprop,adam
from keras.callbacks import History
import data

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

n_timesteps    = 200
n_sequences = every_nth = 80
n_features = 1
n_repeats = 1000
noise_level = 0.005
use_censored = True

res  = data.get_data(n_timesteps, every_nth, n_repeats, noise_level, n_features, n_sequences, use_censored)
y_train, x_train, y_test, x_test, events = res

tte_mean_train = np.nanmean(y_train[:,:,0])
init_alpha = -1.0/np.log(1.0-1.0/(tte_mean_train+1.0) )
init_alpha = init_alpha/np.nanmean(y_train[:,:,1]) # use if lots of censoring
print 'init_alpha: ',init_alpha

np.random.seed(1)

history = History()

model = Sequential()
model.add( GRU(1, input_shape=(n_timesteps, n_features), activation='tanh', return_sequences=True) )
model.add( Dense(2) )
model.add( Lambda(output_lambda, arguments={"init_alpha":init_alpha,  "max_beta_value":4.0}) )

model.compile(loss=weibull_loss_discrete, optimizer=adam(lr=.01))

model.summary()

np.random.seed(1)
model.fit(x_train, y_train, epochs=2,  batch_size=x_train.shape[0]/10, 
          verbose=2,  validation_data=(x_test, y_test), callbacks=[history])

model.save_weights("wtte-simple-weights.h5")
