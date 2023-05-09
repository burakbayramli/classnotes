# Paralel Python - Dask

TensorFlow hesap ağı / çiziti üzerinden paralelize etmeyi popüler hale
getirdi. Pür Python seviyesinde çalışan benzer kavramları kullanan bir
paket Dask.

Dask kutuphanesi Pandas, Numpy paketlerindeki DataFrame, matris
işlemlerine benzer arayüzler sağliyor, ve bu veri yapıları üzerindeki
işlemleri basit şekilde paralel işletilmesini sağlıyor. Fakat daha
basit bağlamda pür Python işlemlerini de paralelleştirmek kolay. İlk
önce tek makinadaki birden fazla çekirdeğe dağılım yapabilecek kodu
görelim, bu en temel kullanım. Ama yine de faydalı, artık Amazon AWS,
ya da Google bulut servislerinde ucuz şekilde 20-30 çekirdekli tek
makina başlatmak mümkün. Bu çok zaman alabilecek doğal parallelliği
olan bir işlemi 30 kat hızlı işletebiliriz demektir!

Kurmak icin pip install dask.

Şöyle bir kod düşünelim,

from time import sleep

```python
def inc(x):
    sleep(1)
    return x + 1

def add(x, y):
    sleep(1)
    return x + y
```

```
%time x = inc(1)
%time y = inc(2)
%time z = add(x, y)

CPU times: user 0 ns, sys: 0 ns, total: 0 ns
Wall time: 1 s
CPU times: user 4 ms, sys: 0 ns, total: 4 ms
Wall time: 1 s
CPU times: user 4 ms, sys: 0 ns, total: 4 ms
Wall time: 1 s
```

`%time` komutu jupyter not defteri içinde çalışıyor, kendi başına
işleyen kodda bunu çıkartabiliriz.

Üstteki toplam 3 saniye alan bir işlem, çünkü tüm çağrılar seri
şekilde yapılmak zorunda. Bir sonraki bir önceki çağrıyı
beklemeli. Fakat delayed komutu ile bir hesap çiziti oluşturabiliriz,
bu çizitteki düğümler paralel şekilde işletilebilirler,

from dask import delayed

```python
%time x = delayed(inc)(1)
%time y = delayed(inc)(2)
%time z = delayed(add)(x, y)
%time print z.compute()
```

```
CPU times: user 0 ns, sys: 0 ns, total: 0 ns
Wall time: 382 us
CPU times: user 0 ns, sys: 0 ns, total: 0 ns
Wall time: 411 us
CPU times: user 0 ns, sys: 0 ns, total: 0 ns
Wall time: 342 us
5
CPU times: user 12 ms, sys: 0 ns, total: 12 ms
Wall time: 2.01 s
```

Bu işlem 2 saniye sürdü. Çünkü çağrılar paralel işledi. x,inc
zincirinin y,inc zincirini beklemesine gerek yok.

Şimdi paralel liste işlemeyi görelim. Bir listedeki tüm öğelerin
karekökunu alıp ikinci bir liste üretme islemi yapalim,

```python
import time
L = range(30)
def f(x):
    time.sleep(0.1)
    return np.sqrt(x)
results = []
%time for x in L: results.append(f(x))
```

```
CPU times: user 4 ms, sys: 0 ns, total: 4 ms
Wall time: 3.02 s
```

Bu kod seri işledi ve 3 saniye aldı. Parallellik için listedeki her
öge için yapılan karekök (sqrt) çağrısını delayed hale getirmek lazım,

```python
import time
L = range(30)
def f(x):
    time.sleep(0.1)
    return np.sqrt(x)
results = []
for x in L:
    y = dask.delayed(f)(x)
    results.append(y)

%time results = dask.compute(*results)  # call compute after you have collected many delayed calls
```

```
CPU times: user 32 ms, sys: 16 ms, total: 48 ms

Wall time: 1.54 s
```

Bizim dizüstü bilgisayarda 2 çekirdek var, hesap iki kat daha hızlı
işledi. 30 tane olsa 30 kat daha hızlı işlerdi!

Sonuç listesini üretirken olanlara dikkat: karekök hesabını değil o
hesabı yapan çağrıyı listeye ekledik, ve bu çağrı delayed ile
paralelize edilmiş çağrıydı. Gerçek hesap dask.compute çağrısı o liste
üzerinde işletilince tetikleniyor, ve o sırada Dask arka planda elde
olan çekirdeklere gerekli dağıtımı yapıyor.

Numpy işlemlerinin Dask karşılığından bahsettik, mesela normal Numpy ile

```python
import numpy as np
# normal mean
%time print np.random.randn(10000,10000).mean()
```

Dask ile

```python
import dask.array as da

# dask mean
x = da.random.normal(0, 1, size=(10000,10000), chunks=(1000, 1000))
%time print x.mean().compute()
```

Bu çağrı da iki kat daha hızlı işledi. Burada yapılan bölme işlemi
chunks anahtar kelimesi üzerinden; Dask'e işlenecek her parçanın ne
kadar büyük olacağını söyledik, ona göre parçalar yaratılıp hesap buna
göre planlandı. Döngü örneğinde parçaları elle biz yaratmıştık, burada
sadece büyüklük tanımlayıp parçaları Dask'e bıraktık.

http://dask.pydata.org/en/latest/delayed-best-practices.html

https://github.com/dask/dask-tutorial

https://gist.github.com/mrocklin/d009e5d4a1f49ecdb433107f3d72c7f3#file-pygotham-dataframes-ipynb

