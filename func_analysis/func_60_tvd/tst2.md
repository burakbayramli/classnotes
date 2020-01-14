

```python
from skimage import io
img1 = io.imread('lena.jpg', as_gray=True)
img2 = io.imread('lena-noise.jpg', as_gray=True)
```

```python
def nabla(I):
    h, w = I.shape
    G = np.zeros((h, w, 2), I.dtype)
    G[:, :-1, 0] -= I[:, :-1]
    G[:, :-1, 0] += I[:, 1:]
    G[:-1, :, 1] -= I[:-1]
    G[:-1, :, 1] += I[1:]
    return G

def anorm(x):
    '''Calculate L2 norm over the last array dimention'''
    return np.sqrt((x*x).sum(-1))
    
def calc_energy_TVL1(X, observation, clambda):
    Ereg = anorm(nabla(X)).sum()
    Edata = clambda * np.abs(X - observation).sum()
    return Ereg + Edata

lambda_TVL1 = 1.0
print (calc_energy_TVL1(img1, img1, lambda_TVL1 ))
print (calc_energy_TVL1(img2, img2, lambda_TVL1 ))
```

```text
3006.4742105110554
6086.196577065672
```









