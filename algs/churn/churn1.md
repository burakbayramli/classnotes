
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
print y_train[1][-20:]
print 'y'
print y_train[2][-20:]
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
 [ 4.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 4.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 2.  0.]
 [ 1.  0.]]
y
[[ 1.  1.]
 [ 0.  1.]
 [ 4.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 4.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 4.  1.]
 [ 3.  1.]
 [ 2.  1.]
 [ 1.  1.]
 [ 0.  1.]
 [ 3.  0.]
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



































