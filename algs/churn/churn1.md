
```python    
import data

n_timesteps    = 30
n_sequences = every_nth = 4
n_features = 3
n_repeats = 1000
#noise_level = 0.005
noise_level = 0.00
use_censored = True

y_train, x_train, y_test, x_test, events = data.get_data(n_timesteps, every_nth, n_repeats, noise_level, n_features, n_sequences, use_censored)

print type(y_train), len(y_train), y_train.shape
print type(x_train), len(x_train)
print y_train[0].shape
print 'y'
print y_train[1][:10]
print 'y'
print y_train[1][-20:]
print 'y'
print y_train[2][-20:]
print 'x'
print x_train[0][:10]
```

```text
n_sequences 4
<type 'numpy.ndarray'> 4000 (4000, 30, 2)
<type 'numpy.ndarray'> 4000
(30, 2)
y
[[ 1.  1.]
 [ 0.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]]
y
[[ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]]
y
[[ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 1.  0.]]
x
[[ 0.  0.  0.]
 [ 0.  0.  0.]
 [ 0.  0.  0.]
 [ 1.  1.  1.]
 [ 0.  0.  0.]
 [ 0.  0.  0.]
 [ 0.  0.  0.]
 [ 1.  1.  1.]
 [ 0.  0.  0.]
 [ 0.  0.  0.]]
```



































