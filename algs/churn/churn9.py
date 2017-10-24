import numpy as np
import pickle, util
from keras.optimizers import RMSprop,adam
from keras.layers import LSTM,GRU
from keras.layers import Dense
from keras.layers import Lambda
from keras.models import Sequential
from keras.callbacks import History
import tensorflow as tf

file = open("recs.pkl",'r')
recs = pickle.load(file)
print len(recs)
file.close()

res = []
N = 200
for i in range(len(recs)):
    if len(recs[i]) > N:res.append(recs[i][-N:])
res = np.array(res).reshape(len(res),N,5)
print res.shape

x = res[:,:,[2]]
y = res[:,:,[0,1]]
print x.shape, y.shape

B = 2000
x_train = x[0:B]; y_train = y[0:B]
x_test = x[B:];   y_test = y[B:]

print x_train.shape

np.random.seed(1)

args = {"init_alpha":100., "max_beta_value":2.0}

history = History()
model = Sequential()
model.add( LSTM(1, input_shape=(N, 1), activation='relu', return_sequences=True) )
model.add( Dense(2) )
model.add( Lambda(util.output_lambda, arguments=args ))
model.compile(loss=util.weibull_loss_discrete, optimizer=adam(lr=.01))

model.summary()

np.random.seed(1)
model.fit(x_train, y_train,
          epochs=3,
          batch_size=200, 
          verbose=2,
          validation_data=(x_test, y_test),
          callbacks=[history])


