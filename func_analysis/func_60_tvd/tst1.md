
save ("xcor.mat", "xcor", "-v7")

https://github.com/lucasrodes/kPCA-denoising-python


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

```python
f = '/tmp/xcor.mat'
import scipy.io as sio
test = sio.loadmat(f)
xcor = test['xcor']
print (xcor.shape)

f = '/tmp/x.mat'
import scipy.io as sio
test = sio.loadmat(f)
x = test['x']
print (x.shape)
```

```text
(5000, 1)
(5000, 1)
```

```python
plt.plot(range(len(x)), xcor)
plt.plot(range(len(x)), x)
plt.savefig('/tmp/out.png')
```









