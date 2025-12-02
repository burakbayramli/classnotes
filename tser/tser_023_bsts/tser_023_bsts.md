# Bayes Yapısal Zaman Serisi Modelleri

```python
import pandas as pd

np.random.seed(42)
n = 120
time = np.arange(n)
trend = 0.1 * time
seasonal = 10 * np.sin(2 * np.pi * time / 12)
noise = np.random.normal(0, 3, n)
y = 50 + trend + seasonal + noise
event_start = 80
y[event_start:] -= 20  
data = pd.DataFrame({'y': y})
data['y'].plot()
plt.savefig('tser_023_bsts_01.jpg')
```


![](tser_023_bsts_01.jpg)

![](tser_023_bsts_02.jpg)

![](tser_023_bsts_03.jpg)

![](tser_023_bsts_04.jpg)













[devam edecek]

Kaynaklar

[1] Bayramli, Istatistik, *Bayes Usulü İstatistiki Analiz*

