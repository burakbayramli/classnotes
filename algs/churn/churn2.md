
```python
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

model = Sequential()
model.add( GRU(1, input_shape=(n_timesteps, n_features), activation='tanh', return_sequences=True) )
model.add( Dense(2) )
model.add( Lambda(util.output_lambda, arguments={"init_alpha":init_alpha,  "max_beta_value":4.0}) )
model.compile(loss=util.weibull_loss_discrete, optimizer=adam(lr=.01))
model.load_weights("/home/burak/Downloads/wtte-simple-weights.h5")
```

```text
n_sequences 80
init_alpha:  43.4425042957
```

```python
print('test shape',x_test.shape,y_test.shape)
plt.imshow(x_test[:,:,:].sum(axis=2)>0,interpolation="none",cmap='Accent',aspect='auto')
plt.title('x_test (lagged/deterministic event indicator)')
plt.savefig('churn2_01.png')
plt.imshow(y_test[:,:,0],interpolation="none",cmap='jet',aspect='auto')
plt.title('y_test[:,:,0] actual tte')
plt.savefig('churn2_02.png')
```

```text
('test shape', (80, 200, 1), (80, 200, 2))
```

```python
print('train shape',x_train.shape,y_train.shape)
plt.imshow(x_train[:every_nth,:,:].mean(axis=2),interpolation="none",cmap='Accent',aspect='auto')
plt.title('x_train[:every_nth,:,0] (lagged/noisy event indicator)')
plt.savefig('churn2_03.png')
plt.imshow(y_train[:every_nth,:,0],interpolation="none",cmap='jet',aspect='auto')
plt.title('y_train[:every_nth,:,0] censored tte')
plt.savefig('churn2_04.png')
plt.imshow(y_train[:every_nth,:,1],interpolation="none",cmap='Accent',aspect='auto')
plt.title('y_train[:every_nth,:,1] u (non-censoring indicator)')
plt.savefig('churn2_05.png')
```

```text
('train shape', (80000, 200, 1), (80000, 200, 2))
```

```python
drawstyle = 'steps-post'
predicted = model.predict(x_train[:every_nth,:,:])
batch_indx = 2+every_nth//4
this_seq_len = n_timesteps
a = predicted[batch_indx,:this_seq_len,0]
b = predicted[batch_indx,:this_seq_len,1]
t = np.array(xrange(len(a)))
x_this = x_train[batch_indx,:this_seq_len,:]

tte_censored = y_train[batch_indx,:this_seq_len,0]
tte_actual   = y_test[batch_indx,:this_seq_len,0]

u = y_train[batch_indx,:this_seq_len,1]>0

plt.plot(tte_censored,label='censored tte',color='red',linestyle='dashed',linewidth=2,drawstyle=drawstyle)
plt.plot(t,tte_actual,label='uncensored tte',color='black',linestyle='solid',linewidth=2,drawstyle=drawstyle)
plt.savefig('churn2_06.png')
```

























































