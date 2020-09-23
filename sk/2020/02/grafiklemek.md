# Grafiklemek, Matplotlib, Pandas

Basit X-Y grafikleri, ikili, üçlü, iki eksenli grafikleri bu yazıda işleyeceğiz.

En basit grafik,

```python
import matplotlib.pyplot as plt

x = np.linspace(-10,10,100)
y  = np.sin(x)
plt.plot(x,y)
plt.savefig('graf_01.png')
```

![](graf_01.png)
![](https://4.bp.blogspot.com/-5IQcs4lswR8/XlywA6moy7I/AAAAAAAAB7M/OjmLdhv1jsoOObkQRUYo1-feZ82C6SsJwCLcBGAsYHQ/s1600/graf_01.png)

Önceki metot tüm noktaları birleştiriyor. Eğer birleştirilmesini
istemesek nokta nokta olarak her veri öğesinin basılmasını istesek
`plot` içinde `.` kullanırdık.  


```python
plt.plot(x,y,'r.')
plt.savefig('graf_02.png')
```

![](graf_02.png)
![](https://2.bp.blogspot.com/-XMnE4JdEx0M/XlywBM9ctHI/AAAAAAAAB7Q/sR96rsWw_UQO_K_0lGJGgdYsHToObTyjwCLcBGAsYHQ/s1600/graf_02.png)

Renkleri de aynı komutta belirtilebiliyoruz. Üstteki `r` kırmızı (red) için. 

Çoğu zaman aynı imajda birden fazla grafik olmasını isteyebiliyoruz,
karşılaştırma amacıyla, vs. Alt alta iki grafik için çabuk bir şekilde

```python
x2 = x+100
y1 = np.cos(x2) + 3*np.sin(x2)
y2 = np.exp(x)

fig, axs = plt.subplots(2)
axs[0].plot(x2,y1)
axs[1].plot(x,y2)
plt.savefig('graf_03.png')
```

![](graf_03.png)
![](https://4.bp.blogspot.com/-uOJjHO3oy9k/XlywBfPxdqI/AAAAAAAAB7U/rHdlQ54DXj4M0vDrSIedhQpdJu_jwP1xACLcBGAsYHQ/s1600/graf_03.png)

Üstteki örnekte x ekseni farklı, çünkü değerlerin skalası farklı o
şekilde grafiklemek istedik. Fakat bazen de aynı x skalası üzerinden
farklı grafikler basmak isteyebiliriz, 

```python
y1 = np.cos(x) + 3*np.sin(x)
y2 = np.exp(x)

fig, axs = plt.subplots(2,sharex=True)
axs[0].plot(x,y1)
axs[1].plot(x,y2)
plt.savefig('graf_04.png')
```

![](graf_04.png)
![](https://3.bp.blogspot.com/-_4l65FYhMYc/XlywBimyCqI/AAAAAAAAB7Y/8NDr6ou3YfATocQe8gCZoLaTICcdCDPPQCLcBGAsYHQ/s1600/graf_04.png)

Eğer Pandas üzerinden grafikleme yapılıyorsa onun `plot(ax=..)`
komutuna dışarıdan `axs[0]`, `axs[1]` vs verilebiliyor.

Pandas ile bu arada çoğu grafikleme iyice basit hale geliyor, mesela
bir `DataFrame` içinde iki kolon olsun, basmak için direk `DataFrame`
üzerinde `plot` çağırmak yeterli,

```python
import pandas as pd
x = np.linspace(0,10,100)
df = pd.DataFrame(index=x)
df['y1'] = np.cos(x)
df['y2'] = np.sin(x)
df.plot()
plt.savefig('graf_05.png')
```

![](graf_05.png)
![](https://2.bp.blogspot.com/-RI-e3gazHfE/XlywCON7gRI/AAAAAAAAB7c/efk1QbIRZ5YtInRo6qn3RDm0GSbiRcaRgCLcBGAsYHQ/s1600/graf_05.png)

Grafikler direk çizildi, her kolon için ayrı grafik olmak üzere,
renklendirildi, ve kolon isimleri bile grafiğin içine yazıldı!

Eğer üstteki gibi bir grafiği ama y ekseninde farklı skala olacak
şekilde basmak istesek ne yapardık? Mesela bir grafik sıcaklık, diğeri
nem miktarı, bunlar farklı skalalarda olabilir ama trendlerini aynı
grafikte aynı x skalası üzerinden (mesela tarih) görmek faydalı olabilir. 

```python
import pandas as pd
x = np.linspace(0,10,100)
plt.figure(figsize=(5,3))
df['y1'] = np.cos(x)
df['y2'] = np.sin(x) + 100.0
ax1 = df.y1.plot(color='blue', grid=True, label='y1')
ax2 = df.y2.plot(color='red', grid=True, label='y2',secondary_y=True)
h1, l1 = ax1.get_legend_handles_labels()
h2, l2 = ax2.get_legend_handles_labels()
plt.legend(h1+h2, l1+l2, loc=2)
plt.savefig('graf_06.png')
```

![](graf_06.png)
![](https://2.bp.blogspot.com/-348G1HJq4_Y/XlywCXh_04I/AAAAAAAAB7g/-X-2yBQ5uRYpRVw8A3wqZtbz7QE8rdyMACLcBGAsYHQ/s1600/graf_06.png)

Belli bölgeleri belli x değerleri için tüm y eksen bazında daha farklı
renkte göstermek isteyebiliriz, mesela finansta ekonomik krizler zaman
serisi üzerinde bu şekilde gösterilir. Örnek olarak ABD / Britanya
Pound döviz kuru üzerinde görelim, ve iki krizi zamanını işaretleyelim,


```python
import pandas as pd

df = pd.read_csv('DEXUSUK.csv',index_col=0,parse_dates=True)
df = df[df.index > '1995-01-01']
df.plot()
c = 'gainsboro' # acik gri rengi
plt.axvspan('01-03-2001', '27-10-2001', color=c, alpha=0.5, lw=0)
plt.axvspan('22-12-2007', '09-05-2009', color=c, alpha=0.5, lw=0)
plt.savefig('graf_07.png')
```

![](graf_07.png)
![](https://1.bp.blogspot.com/-1SOHrKbkg0g/XlywCshJEqI/AAAAAAAAB7k/Su7GXUkcWiEEkps8aO9z8VXe-zG91exNwCLcBGAsYHQ/s1600/graf_07.png)












