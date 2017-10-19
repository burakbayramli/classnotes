
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
import data

n_timesteps    = 30
n_sequences = every_nth = 5
n_features = 3
n_repeats = 1000
#noise_level = 0.005
noise_level = 0.00
use_censored = True

y_train, x_train, y_test, x_test, events = data.get_data(n_timesteps, every_nth, n_repeats, noise_level, n_features, n_sequences, use_censored)

print type(y_train), len(y_train)
print type(x_train), len(x_train)
print 'y'
print y_train[1][:10]
print 'y'
print y_train[1][-10:]
print 'x'
print x_train[0][:10]
```

```text
n_sequences 5
<type 'numpy.ndarray'> 5000
<type 'numpy.ndarray'> 5000
y
[[ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 4.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 4.  1.]
 [ 3.  1.]]
y
[[ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 4.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 2.  0.]
 [ 1.  0.]]
x
[[ 0.  0.  0.]
 [ 0.  0.  0.]
 [ 0.  0.  0.]
 [ 0.  0.  0.]
 [ 1.  1.  1.]
 [ 0.  0.  0.]
 [ 0.  0.  0.]
 [ 0.  0.  0.]
 [ 0.  0.  0.]
 [ 1.  1.  1.]]
```


model = keras.models.load_model(filepath)


































