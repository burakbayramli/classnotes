# Monte Carlo, Entegraller

Monte Carlo entegrasyonu bir entegral mesela $f(x)$'i sayısal olarak kestirmek
(estimation), ona yakın bir sonuca sayısal olarak erişmenin
yöntemidir. Arkasında yatan teori oldukca basit, diyelim ki $f(x)$'i bir $D$
tanım bölgesi (domain) üzerinden entegre etmek istiyoruz [1].

$$
I = \int_{x \in D} f(x) \mathrm{d} x
$$

Tek değişkenli fonksiyonlar için etki tek boyutlu ve entegrasyon sınırları
basit olarak $a$ ile $b$ arasında.

Biraz cebirsel numara yaparsak, mesela üstteki formülü $p(x)$ ile çarpalım
bölelim (hiçbir değişiklik yaratmamış oluyoruz aslında)

$$
I = \int_{a}^{b} \frac{f(x)}{p(x)} p(x) \mathrm{d} x
$$

$f(x)/p(x)$ bölümüne bir isim verelim, mesela $g(x)$,

$$
I = \int_{a}^{b} g(x) p(x) \mathrm{d} x
$$

Üstteki formül bir beklenti (expectation) hesabına benzemiyor mu? Evet,
$g(x)$'in $p(x)$ yoğunluğu üzerinden beklentisi bu formüldür, 

$$
E[g(x)] = I = \int_{a}^{b} g(x) p(x) \mathrm{d} x
$$

Beklenti hesabını örneklem ortalaması ile yaklaşık hesaplayabileceğimizi
biliyoruz, etki alanından $N$ tane $x_i$ örneklemi alalım mesela, o zaman

$$
E[g(x)] \approx
\frac{1}{N} \sum_{i=1}^{N} g(x_i) =
\frac{1}{N} \sum_{i=1}^{N} \frac{f(x_i)}{g(x_i)}
$$

Diyelim ki $a,b$ arasında örneklem aldığımız sayılar birörnek (uniform)
dağılımdan geliyor, yani $p(x)$ birörnek dağılımın yoğunluğu, $p(x) = 1/(b-a)$,
bunu üstteki son formüle sokarsak,

$$
= (b-a) \frac{1}{N} \sum_{i=1}^{N} f(x_i) 
$$

Bu son formül $f(x)$'in $a,b$ arasındaki ortalamasını hesaplıyor ve onu aralığın
uzunluğu ile çarpıyor, bir anlamda bir dikdörtgen alanını hesaplıyoruz,
ki bu dikdörtgenin eni $a,b$ aralığının uzunluğu, yüksekliği ise $f(x)$'in
beklenti değeri.

Mesela $f(x) = x^2$'nin entegralini bulalım, aralık $-2,+2$ arası,

```python
def func1(x):
    return x**2

def func1_int(a, b):
    return (1/3)*(b**3-a**3)
  
def mc_integrate(func, a, b, n = 1000):
    vals = np.random.uniform(a, b, n)
    y = [func(val) for val in vals]    
    y_mean = np.sum(y)/n
    integ = (b-a) * y_mean    
    return integ

print(f"Monte Carlo çözümü: {mc_integrate(func1, -2, 2, 500000): .4f}")
print(f"Analitik çözüm: {func1_int(-2, 2): .4f}")
```

```
Monte Carlo çözümü:  5.3254
Analitik çözüm:  5.3333
```

Eğer boyutları arttırsak çözümün genel yapısı değişmiyor mesela üç boyuta çıktık
diyelim [3, sf. 752], entegral hesabı alttaki gibi gözükecekti,

$$
\int_{x_0}^{x_1} \int_{y_0}^{y_1} \int_{z_0}^{z_1}  f(x,y,z) \mathrm{d} x \mathrm{d} y \mathrm{d} z
$$

O zaman Monte Carlo hesabı için $X_i = (x_i,y_i,z_i)$ örneklemi almak gerekir,
çok boyutlu yine birörnek dağılımdan diyelim, ve $p(X)$ hesaplanır, ve kestirme
hesap

$$
\frac{(x_1-x_0)(y_1-y_0)(z_1-z_0)}{N} \sum_i f(X_i)
$$

Bu hesap için bir örnek, iki boyutlu bir fonksiyonun entegralini hesaplayalım,
$f(x) = 10 - x_1^2 - x_2^2$, sınırlar $-2,+2$ olsun.

```python
def func1(x):
    return 10 + np.sum(-1*np.power(x, 2), axis=1)
  
def mc_integrate(func, a, b, dim, n = 1000):
    x_list = np.random.uniform(a, b, (n, dim))
    y = func(x_list)
    y_mean =  y.sum()/len(y)
    domain = np.power(b-a, dim)
    integ = domain * y_mean
    return integ

print(f"Monte Carlo çözümü : {mc_integrate(func1, -2, 2, 2, 1000000): .3f}")
print(f"Analitik çözüm: 117.333")
```

```
Monte Carlo çözümü :  117.305
Analitik çözüm: 117.333
```

Doğru Sonuca Yakınsama

Fakat niye Monte Carlo hesaplamanın normal sayısal entegral yöntemlerinden daha
iyi olacağını motive etmedik / açıklamadık. Sonuçta $a,b$ arası birörnek
dağılımdan örneklem almak niye bu aralığı eşit parçalara bölerek dikdörtgen
alanlarını klasik şekilde toplamaktan daha iyi olsun ki?

Bu sorunun cevabı çok boyutlulukta gizli; MC tek boyutta diğer klasik yöntemlere
kıyasla aşağı yukarı aynı cevabı aynı hızda verebilir, fakat yüksek boyutlara
çıktıkça MC yöntemleri parlamaya başlıyor çünkü hataları örneklem büyüklüğü $N$
sayısına bağlı, boyut sayısına değil. Klasik sayısal yöntemlerde boyut arttıkça
hesapsal yükler katlanarak artar, MC bu tür problemlerden korunaklıdır.

İspatlamak için MC tahmin edici / kestirme hesaplayıcı (estimator) varyansını
hesaplamak bilgilendirici olur. Bu varyans bize ortalama hata hakkında ipucu
verecektir, ve hatanın azalmasında hangi faktörlerin rol oynadığını
gösterir. Biraz önce hesaplanan büyüklüğü hatırlarsak, ona $\bar{g}$
diyelim [2, sf. 455],

$$
\bar{g} = \frac{1}{N} \sum_{i=1}^{N} g(X_i)
$$

$$
Var(\bar{g}) = Var \left[ \frac{1}{N} \sum_{i=1}^{N} g(X_i)  \right]
$$

Varyans operasyonu toplamın içine nüfuz edebilir, ayrıca sabitler karesi
alınarak dışarı çıkartılabilir, o zaman

$$
= \frac{1}{N^2} \sum_{i=1}^{N} Var[ g(X_i)]  
$$

Eğer herhangi bir $X_1,X_2,..$ değişkenine $X$ dersek ve tüm $X_i$ rasgele
değişkenlerinin varyansı aynı olacağı için üstteki toplam aynı varyansı $N$
kere toplar, o zaman $N$ dışarı çıkartılıp $1/N^2$ deki bir $N$'yi iptal etmek
için kullanılabilir, yani 

$$
Var(\bar{g}) = \frac{1}{N} Var[ g(X)]  
$$

Her iki tarafın karekökünü alırsak,

$$
\sqrt{Var(\bar{g})} = \frac{1}{\sqrt{N}} \sqrt{Var[ g(X)]}
$$

Gördüğümüz gibi örneklem ortalamasının standart sapması $1/\sqrt{N}$ oranında
küçülüyor, eğer $n$'yi dört katına çıkartırsak, yani dört kat daha fazla
örneklem kullanırsak, standart sapma yarıya düşüyor. Bu düşüş yüksek boyutlarda
da geçerli oluyor, bu sebeple Monte Carlo yöntemleri yüksek boyutta klasik
sayısal entegral yöntemlerinden daha iyi performans gösteriyor.

Kıyasla eksenleri eşit parçalara bölerek entegre hesaplayan yöntemler
(quadratüre) boyutlar yükseldikça problem yaşarlar. Diyelim ki tek boyutta
sayısal entegral hesap için başlangıç ve bitiş sınırları arasını 10 parçaya
bölüyoruz. Boyutlar ikiye çıkarsa ve aynı yöntemle devam edersek 10 çarpı 10 =
100 noktalı bir izgara (grid) elde ederiz. Bir sonraki boyut için benzer büyüme,
10'ar 10'ar bir artış var elimizde. Altı boyut bizi 1 milyon noktaya getirir ve
dikkat, öyle müthiş bir çözünürlülük te kullanmadık, sınırlar arasında 10 tane
nokta var sadece. Eğer çözünürlüğü arttırsak, 100 desek bunun katlanarak artması
bizi çok büyük rakamlara getirecektir, ve bu kadar fazla hesap noktası hesapsal
yükü arttırıp hesap algoritmasinin yavaşlatacaktır. Monte Carlo yaklaşımları
"boyut laneti (the curse of dimensionality)'' denen kavramdan korunaklıdır, $N$
arttıkça performansı artar, ve bu $N$ sayısı boyut $D$ ile bağlantılı değildir.

Kaynaklar

[1] Zhao, *Monte Carlo integration in Python over univariate and multivariate functions*,
    [https://boyangzhao.github.io/posts/monte-carlo-integration](https://boyangzhao.github.io/posts/monte-carlo-integration)

[2] Gezerlis, *Numerical Methods in Physics with Python*
    
[3] Pharr, *Physically Based Rendering 3rd Ed*




