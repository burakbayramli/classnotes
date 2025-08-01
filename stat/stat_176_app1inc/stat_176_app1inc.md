# Azar Azar İstatistik (Incremental Statistics)

Artımsal Ortalama (Mean)

Eldeki bir verinin yaş, boy, kilo gibi kolonlarını özetlemenin yollarından biri
ortalama. Ortalama hesabının pek çok kütüphanede çağrısı var, mesela
`numpy.mean`, ya da Pandas `Series.mean`.


```python
import pandas as pd
from io import StringIO

data = """
Name,Height
Ali,160
Veli,170
Musa,150
Mustafa,200
Cem,180
"""
df = pd.read_csv(StringIO(data),sep=',')
print (df)
```

```
      Name  Height
0      Ali     160
1     Veli     170
2     Musa     150
3  Mustafa     200
4      Cem     180
```

Ortalamayı alırsak

```python
df['Height'].mean()
```

```
Out[1]: 172.0
```

Matematiksel olarak eldeki $x_i$ verisi için klasik ortalama hesabı $\bar{x}$
basit, tüm değerleri topla, ve değer sayısına böl,

$$
\bar{x} = \frac{1}{n} \sum_{i=1}^{n} x_i
\qquad (2)
$$

Kod ile

```python
print (np.array(df.Height))
mean = df.Height.sum() / len(df)
print ('ortalama',mean)
```

```
[160 170 150 200 180]
ortalama 172.0
```

Şimdi artımsal hesabı düşünelim. Üstteki klasik hesabı yapmak için tüm verileri
toplamak gerekti, eğer ilk akla gelen yöntemle artımsal ortalama hesaplasak, her
adımda o ana kadar olan toplamı takip edip o ana kadar olan veri sayısına bölmek
gerekirdi, ama elde çok fazla veri varsa bu toplamın çok büyük rakamlara
erişmesi mümkündür, bu da taşma, veri hataları ortaya çıkartabilir.

Başka bir şekilde azar azar ortalama hesabı mümkün müdür acaba? Sadece
ortalamayı güncellesek, toplamlarla uğraşmasak? O ana kadar bakılan verinin
ortalaması fazla büyümez, ayrıca paralel işletim açısından azar azar işletim
daha ölçeklenebilir bir yaklaşımdır. Bu tür bir hesap için matematikte biraz
değişim yapmak lazım [3]. Üstteki toplam formülünde ilk $n-1$ toplamını
ayıralım,

$$
\bar{x} = \frac{1}{n} \left( \sum_{i=1}^{n-1} x_i + x_i \right)
\qquad (1)
$$

(2)'de görülen $\bar{x}$ formülü ilk $n$ verisinin ortalaması demiştik, o zaman
ilk $n-1$ verisinin ortalaması doğal olarak

$$
\bar{x}_{n-1}  = \frac{\sum_{i=1}^{n} x_i}{n-1} 
$$

Tekrar düzenlersek,

$$
\sum_{i=1}^{n} x_i  = (n-1) \bar{x}_{n-1}  
$$

Bu formülü (1)'e sokalım,

$$
\bar{x} = \bar{x}_n = \frac{1}{n} \left( (n-1) \bar{x}_{n-1} + x_n \right)
$$

Sağ tarafı açalım,

$$
\bar{x}_n = \frac{n \bar{x}_{n-1} - \bar{x}_{n-1} + x_n}{n}
$$

$$
\bar{x}_n = \frac{n \bar{x}_{n-1}}{n} + \frac{x_n - \bar{x}_{n-1}}{n}
$$

İlk terimdeki $n$'ler iptal olur,

$$
\bar{x}_n = \bar{x}_{n-1} + \frac{x_n - \bar{x}_{n-1}}{n}
$$

Yani bir sonraki ortalama hesabı için eldeki yeni veri $x_n$'den o ana kadar
elde olan ortalamayı çıkartıp $n$'ye bölüp bu sonucu önceki ortalamaya
ekliyoruz. Böylece sürekli daha ufak sayılarla uğraşıyoruz, patlama olmuyor
ayrıca elde sürekli bir ortalama hesabı oluyor.

```python
barx = 160 # ilk degeri ilk ortalama olarak kullan
for n,xn in enumerate(np.array(df.Height)):
   barx = barx + (xn - barx) / (n+1)
   print (xn, barx)   
```

```
160 160.0
170 165.0
150 160.0
200 170.0
180 172.0
```

Üstte görülen 172 değerine ulaştık.

Artımsal Standart Sapma (Incremental Standard Devation)

Varyans (standard sapmanın karesi) formülü ile başlayalım.

$$
s^2 = \frac{ \sum_{i=1}^{n} (x_i - \bar{x}_n )^2 }{n-1}
$$

Her iki tarafı $n-1$ ile çarpalım,

$$
(n-1)s^2 =  \sum_{i=1}^{n} (x_i - \bar{x}_n )^2 
$$

Sağdaki değeri $d_n^2$ diye tanımlayalım, bu cebirde ilerlerken bize
faydalı olacak.

$$
d_n^2 =  \sum_{i=1}^{n} (x_i - \bar{x}_n )^2 
$$

Şimdi sağ tarafı açalım, ve üç ayrı toplam haline getirelim,

$$
d_n^2 = \sum_{i=1}^{n} ( x_i^2 - 2x_i\bar{x}_n + \bar{x}_n^2)
$$

$$
d_n^2 =
\sum_{i=1}^{n} x_i^2 -
\sum_{i=1}^{n} 2x_i\bar{x}_n +
\sum_{i=1}^{n} \bar{x}_n^2
$$

Sabitleri disari cikartabiliriz,

$$
d_n^2 =
\sum_{i=1}^{n} x_i^2 -
2 \bar{x}_n \sum_{i=1}^{n} x_i +
\bar{x}_n^2 \sum_{i=1}^{n} 1
$$

Şimdi toplam ve ortalama $\bar{x}_n$ arasındaki ilişki $\sum_{i=1}^{n} x_i = n \bar{x}_n$ 
formülünden hareketle, üstteki formülü

$$
d_n^2 =
\sum_{i=1}^{n} x_i^2 -
2 \bar{x}_n \cdot n \bar{x}_n +
\bar{x}_n^2 \sum_{i=1}^{n} 1
$$

Ayrıca $n = \sum_{i=1}^{n} 1$ olduğu için

$$
d_n^2 = \sum_{i=1}^{n} x_i^2 - 2 n \bar{x}_n^2 + n \bar{x}_n^2
$$

$$
d_n^2 = \sum_{i=1}^{n} x_i^2 - n \bar{x}_n^2 
$$

Bu sonucu elde ettikten sonra onu ilk $n-1$ için kullanalım,

$$
d_{n-1}^2 = \sum_{i=1}^{n-1} x_i^2 - (n-1) \bar{x}_{n-1}^2 
$$

Son iki formülü birbirinden çıkartırsak,

$$
d_n^2 - d_{n-1}^2 =
\sum_{i=1}^{n} x_i^2 - n \bar{x}_n^2  -
\left( \sum_{i=1}^{n-1} x_i^2 - (n-1) \bar{x}_{n-1}^2   \right)
$$

$$
d_n^2 - d_{n-1}^2 =
\sum_{i=1}^{n} x_i^2 - n \bar{x}_n^2  - \sum_{i=1}^{n-1} x_i^2 + (n-1) \bar{x}_{n-1}^2 
$$

Alttaki eşitlikten hareketle,

$$
x_n^2 = \sum_{i=1}^{n} x_i^2 - \sum_{i=1}^{n-1} x_i^2
$$

İki üstteki ifade biraz daha basitleştirilebilir,

$$
d_n^2 - d_{n-1}^2 = x_n^2 - n \bar{x}_n^2 + (n-1) \bar{x}_{n-1}^2
$$

Son terimi çarpınca

$$
d_n^2 - d_{n-1}^2 = x_n^2 - n \bar{x}_n^2 + n \bar{x}_n^2 - \bar{x}_{n-1}^2
$$

Sıralamayı değiştirirsek,

$$
d_n^2 - d_{n-1}^2 = x_n^2  - \bar{x}_{n-1}^2 + n \bar{x}_{n-1}^2 - n \bar{x}_n^2
$$

$$
d_n^2 - d_{n-1}^2 = x_n^2  - \bar{x}_{n-1}^2 + n (\bar{x}_{n-1}^2 - \bar{x}_n^2)
$$

Temel cebirden biliyoruz ki $a^2-b^2 = (a-b)(a+b)$, bunu üstteki formüle
uygularsak,

$$
d_n^2 - d_{n-1}^2 =
x_n^2  -
\bar{x}_{n-1}^2 +
n (\bar{x}_{n-1} - \bar{x}_n) (\bar{x}_{n-1} + \bar{x}_n) 
$$

[3] yazısından biliyoruz ki şöyle bir eşitlik mevcut $n (\bar{x}_{n-1} - \bar{x}_n) = \bar{x}_{n-1} - x_n$,
onu üstte kullanırsak,

$$
d_n^2 - d_{n-1}^2 = x_n^2  - \bar{x}_{n-1}^2 +  (\bar{x}_{n-1} - x_n) (\bar{x}_{n-1} + \bar{x}_n) 
$$

Tüm çarpımı yaparsak,

$$
d_n^2 - d_{n-1}^2 =
x_n^2 -
\bar{x}_{n-1}^2  +
\bar{x}_{n-1}^2 +
\bar{x}_n \bar{x}_{n-1} -
\bar{x}_{n-1} x_n -
\bar{x}_n x_n
$$

2'inci ve 3'uncu terim iptal olur,

$$
d_n^2 - d_{n-1}^2 =
x_n^2 -
\bar{x}_{n-1} x_n -
\bar{x}_n x_n -
\bar{x}_n \bar{x}_{n-1}
$$

Yine temel cebirden biliyoruz ki

$$
(x-a)(x-b) = x^2 - bx - ax + ab
$$

Bunu kullanarak iki ustteki formulu basitlestirebiliriz,

$$
d_n^2 - d_{n-1}^2 = (x_n - \bar{x}_n)(x_n - \bar{x}_{n-1} )
$$

Nihai sonuç,

$$
d_n^2 = d_{n-1}^2 + (x_n - \bar{x}_n)(x_n - \bar{x}_{n-1} )
$$

Biraz zaman aldı ama güzel bir artımsal formül elde ettik. Yeni $d_n^2$
büyüklüğünü bir önceki büyüklüğü kullanarak hesaplamak artık mümkün. 
Varyansı elde etmek için $n-1$ ile bölmek yeterli,

$$
s_n^2 = \frac{d_n^2}{n-1}
$$

Onun karekökünü alınca da standart sapma elde edilir,

$$
s_n = \sqrt{\frac{d_n^2}{n-1}}
$$

```python
dns = 0; barx = 160
for n,xn in enumerate(np.array(df.Height)):
   barxprev = barx
   barx = barx + (xn - barx) / (n+1)
   dns = dns + (xn - barx)*(xn-barxprev)
   print (xn, dns/n)
```

```
160 nan
170 50.0
150 100.0
200 466.6666666666667
180 370.0
```

Eğer paket çağrısı ile hesaplarsak, 

```python
print (df.Height.var())   
```

```
370.0
```

Aynı sonucu elde ettik. 

Artımsal Ortalama ve Varyans Hesabı (Youngs ve Cramer Yöntemi)

[1]'de gördüğümüz varyans formülünü $x$ kullanarak tekrar yazarsak,

$$ 
S = \sum_{i=1}^{n} x_i^2 - \frac{1}{n} \bigg( \sum_{i=1}^{n} x_i \bigg)^2  
$$

Bu formülü her yeni veri geldikçe eldeki mevcut varyansı "güncelleme''
amaçlı olarak tekrar düzenleyebilirdik, böylece veri üzerinden bir kez
geçmekle kalmayıp en son bakılan veriye göre en son varyansı
hesaplayabilmiş olurduk. Ortalama için mesela her yeni veri bir toplama
eklenebilir, ayrıca kaç veri noktası görüldüğü hatırlanır, ve o andaki en
son ortalama en son toplam bölü bu en son sayıdır. 

Fakat varyans için (5)'in bir problemi var, $\sum x_i^2$ ve $(\sum x_i)^2$
sayıları uygulamalarda aşırı büyüyorlar, ve yuvarlama hataları (rounding
errors) hataları ortaya çıkmaya başlıyor. Eğer varyans küçük ise bu aşırı
büyük sayılardaki tüm basamaklar birbirini iptal eder, geriye hiçbir şey
kalmaz. Bu hatalardan uzak durmak için varyansı farklı bir artımsal
yöntemle hesaplamak istiyoruz.

Youngs ve Cramer'in yöntemine göre [2, sf. 69] bu hesap şöyle
yapılabilir. $T_{ij}$, $M_{ij}$ ve $S_{ij}$, veri noktaları $x_i$ $x_j$
arasındaki verileri kapsayacak şekilde sırasıyla toplam, ortalama ve verinin
karesinin toplamı olsun,

$$ 
T_{ij} = \sum_{k=i}^{j} x_k , \quad  
M_{ij} = \frac{1}{(j-1+1)}, \quad
S_{ij} = \sum_{k=i}^{j} (x_k - M_{ij})^2
$$

Güncelleme formülleri şunlardır, 

$$ T_{1,j} = T_{i,j-1} + x_j$$

$$ S_{1,j} = S_{i,j-1} + \frac{1}{j(j-1)} (jx_j - T_{1,j})^2  $$

ki $T_{1,1} = x_1$ ve $S_{1,1}=0$ olacak şekilde.

İspat

$$ 
\sum_{k=1}^{j} \bigg( x_k - \frac{1}{j} T_{1j} \bigg) = 
\sum_{k=1}^{j} \bigg( x_k - \frac{1}{j} (T_{1,j-1}+x_j)  \bigg)^2
$$

$$ = \sum_{k=1}^{j} \bigg(
\bigg(x_k - \frac{1}{j-1}T_{1,j-1} \bigg) + 
\bigg( \frac{1}{j(j-1)} T_{1,j-1} - \frac{1}{j} x_j\bigg) 
\bigg)^2
$$

çünkü $\frac{1}{j} = \frac{1}{j-1}-\frac{1}{j(j-1)}$


$$
= \sum_{k=1}^{j-1} \bigg( x_k - \frac{1}{j-1} T_{1,j-1} \bigg)^2  
 \bigg( x_j - \frac{1}{j-1} T_{1,j-1} \bigg)^2 +
$$
$$
2 \sum_{k=1}^{j}  \bigg( x_k - \frac{1}{j-1} T_{1,j-1} \bigg)
\bigg( \frac{1}{j(j-1)} T_{1,j-1} - \frac{1}{j} x_j \bigg) +
$$
$$
j \bigg( \frac{1}{j(j-1)} T_{1,j-1} - \frac{1}{j} x_j \bigg) 
$$

$$ 
= \sum_{k=1}^{j-1} \bigg( x_k - \frac{1}{j-1} T_{1,j-1} \bigg)^2 + 
\bigg( x_j - \frac{1}{j-1} T_{1,j-1} \bigg)^2 \bigg( 1-\frac{2}{j} \bigg) + 
j \bigg( \frac{1}{j(j-1)} T_{1,j-1} - \frac{1}{j}x_j \bigg)^2
$$

çünkü $\sum_{k=1}^{j-1} (x_k-\frac{1}{j-1} T_{1,j-1} )=0$

$$ 
= S_{1,j-1}  + \bigg( x_j - \frac{1}{j-1} (T_{1j}-x_j) \bigg) ^2
\bigg( 1-\frac{2}{j}+\frac{1}{j}\bigg)
$$

$$ = S_{1,j-1} + \frac{1}{(j-1)^2} (jx_j - T_{1j})^2 \frac{j-1}{j} $$

Bu algoritma (5) algoritmasından daha stabil. Kod üzerinde görelim,

```python
def incremental_mean_and_var(x, last_sum, last_var, j):
    new_sum = last_sum + x
    new_var = last_var + (1./(j*(j-1))) * (j*x - new_sum)**2 
    return new_sum, new_var

N = 10
arr = np.array(range(N)) # basit veri, 0..N-1 arasi sayilar
print (arr)
last_sum = arr[0]; last_var = 0.
for j in range(2,N+1):
    last_sum,last_var = incremental_mean_and_var(arr[j-1], last_sum, last_var, j)

print ('YC =', last_var / N, 'Standart = ', arr.var())
print (last_sum, arr.sum())
```

```
[0 1 2 3 4 5 6 7 8 9]
YC = 8.25 Standart =  8.25
45 45
```


Kaynaklar

[1] Bayramlı, *Istatistik, Beklenti, Varyans, Kovaryans ve Korelasyon*

[2] Weihs, *Foundations of Statistical Algorithms With References to R Packages*

[3] Nested Software, *Calculating a Moving Average on Streaming Data*,
    [https://nestedsoftware.com/2018/03/20/calculating-a-moving-average-on-streaming-data-5a7k.22879.html](https://nestedsoftware.com/2018/03/20/calculating-a-moving-average-on-streaming-data-5a7k.22879.html)

[4] Nested Software, *Calculating Standard Deviation on Streaming Data*,
    [https://nestedsoftware.com/2018/03/27/calculating-standard-deviation-on-streaming-data-253l.23919.html](https://nestedsoftware.com/2018/03/27/calculating-standard-deviation-on-streaming-data-253l.23919.html)


