
```python
import pandas as pd, zipfile
#df = pd.read_csv('online_retail.csv')
with zipfile.ZipFile('/home/burak/Documents/Dropbox/Public/data/retail.zip', 'r') as z:
    df =  pd.read_csv(z.open('online_retail.csv'))    
```

```python
print df[df.CustomerID == 15065][['Description','InvoiceDate']].tail(10)
```

```text
                            Description   InvoiceDate
433515  PAPER CHAIN KIT 50'S CHRISTMAS   11/2/11 9:45
433516         ROLL WRAP 50'S CHRISTMAS  11/2/11 9:45
433517        IVORY WICKER HEART MEDIUM  11/2/11 9:45
433518  VINTAGE DOILY TRAVEL SEWING KIT  11/2/11 9:45
433519         JUMBO BAG VINTAGE DOILY   11/2/11 9:45
433520        HAND WARMER RED RETROSPOT  11/2/11 9:45
433521          HAND WARMER BIRD DESIGN  11/2/11 9:45
433522    PRETTY HANGING QUILTED HEARTS  11/2/11 9:45
433523          BUBBLEGUM RING ASSORTED  11/2/11 9:45
433524           6 RIBBONS RUSTIC CHARM  11/2/11 9:45
```

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

model = Sequential()
model.add( GRU(1, input_shape=(n_timesteps, n_features), activation='tanh', return_sequences=True) )
model.add( Dense(2) )
model.add( Lambda(output_lambda) )
model.compile(loss=weibull_loss_discrete, optimizer=adam(lr=.01))
model.load_weights("wtte-simple-weights.h5")
```





























































