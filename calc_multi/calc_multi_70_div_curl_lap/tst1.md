
```python
f = 'wind.mat'
import scipy.io as sio
mat = sio.loadmat(f)
print (mat['x'].shape)
print (mat['y'].shape)
print (mat['u'].shape)
```

```text
(35, 41, 15)
(35, 41, 15)
(35, 41, 15)
```

```python
print (type(mat['x']))
```

```text
<class 'numpy.ndarray'>
```







