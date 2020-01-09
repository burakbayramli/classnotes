
n=5000;
EPSILON = 0.001;
MU = 50;

```python
f = 'xcor.mat'
import scipy.io as sio
test = sio.loadmat(f)
xcor = test['xcor']
print (xcor.shape)
```

```text
(5000, 1)
```










