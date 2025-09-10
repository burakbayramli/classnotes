


```python
from scipy import stats
from scipy.stats import norm

def make_data_binormal(data_count=1000, seed=100):
    np.random.seed(seed)
    n1 = data_count // 2
    n2 = data_count - n1
    x = np.concatenate([
        np.random.normal(-1, 2, n1),
        np.random.normal(5, 1, n2)  
    ])
    def true_pdf(z):
        return 0.5 * norm(-1, 2).pdf(z) + 0.5 * norm(5, 1).pdf(z)
    np.random.shuffle(x)
    return x, true_pdf
    
data, d = make_data_binormal(data_count=1*10000)
x_vals = np.linspace(np.min(data), np.max(data), 200)
num_points = len(data)    
total_density = None
h = 0.5
alpha = 0.001
x_values = np.linspace(min(data) - 1, max(data) + 1, num_points)
current = data[0]
total_density = norm.pdf(x_values, loc=current, scale=h)
for i in range(len(data[1:])):
    current = data[i]
    density = norm.pdf(x_values, loc=current, scale=h)
    total_density =  (1-alpha)*total_density +  (alpha)*density

plt.plot(x_vals, d(x_vals), color='green', lw=2, linestyle='-', label='Gercek Dagilim')
plt.plot(x_values, total_density, color='red', lw=2, linestyle='--', label='EWMA KDE')
plt.legend()
plt.savefig('/tmp/out1.jpg')
```

```text
(50000,)
```

