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
import data, util

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

args = {"init_alpha":init_alpha,  "max_beta_value":4.0}

history = History()
model = Sequential()
model.add( GRU(1, input_shape=(n_timesteps, n_features), activation='tanh', return_sequences=True) )
model.add( Dense(2) )
model.add( Lambda(util.output_lambda, arguments=args ))
model.compile(loss=util.weibull_loss_discrete, optimizer=adam(lr=.01))

model.summary()

print 'batch size', x_train.shape[0]/10
print x_train.shape

np.random.seed(1)
model.fit(x_train, y_train, epochs=80,  batch_size=x_train.shape[0]/10, 
          verbose=2,  validation_data=(x_test, y_test), callbacks=[history])

model.save_weights("wtte-simple-weights.h5")
