# Kur Krizi Alarm Yazılımı

Sabitlenmiş bir kur devaluasyon yasamaya yakın mıdır? Bir ülkenin kur
krizi yaşama ihtimali nedir? The Global Macro Database [1] adı verilen
veriyi kullanarak kriz tahmini yapma amaçlı bir gösterge (indicator)
takip eden / saptayıcı / tahminci yazılımı paylaşacağız.

GMD verisi IMF, Dünya Bankası, OECD verilerini birleştirir ve finans,
kur krizlerini etiketleyerek araştırmacıların rahatça erisebileceği
bir kaynak sağlar. Veriyi direk [2]'den zip / csv olarak
indirebiliriz. Bir diğer yöntem,

```
pip install global_macro_data
```

sonrası

```python
from global_macro_data import gmd
df = gmd(fast="yes")
df.to_csv('/opt/Downloads/gmd/gmd.csv')
```

Bu yöntem eğer güncel yeni veri varsa en son versiyonu indirir, yoksa
eski versiyonu tutar, veri indirmesi olmaz.

### Tasarım

Yaklaşmakta olan krizlerin farklı önden gelen göstergeleri (leading
indicator) olabilir. Altta tahmin amaçlı bizim kullandıklarımızı altta
görebiliriz, onları saptarken [3,4] yazılarını temel aldık.

Tahmin amacıyla lojistik regresyon istatistik yöntemini temel
aldık. Bir senedeki (birazdan listeleyeceğimiz) değişkenler bir
*sonraki* senedeki kriz varlığı (1/0) ile regresyona verilecek. Kriz
kolonu GMD'de `CurrencyCrisis`. Ardından tanımladığımız değişkenlerin
katsayıları bir sonraki adımda tahmin amaçlı kullanılabilecek, tahmini
istenen T senesi ise T-1 senesindeki değişkenlerin değerleri
katsayıları ile birlikte lojistik formülüne sorulacak. Bu formül tüm
katsayıları ile birlikte şu şekilde,

$$
\left(
\text{Crisis}_{t+1}=1\right)=
\sigma\left(
    \beta_0+
    \beta_{1}\,z^{\text{ca}}+
    \beta_{2}\,z^{\text{rer_dev}}+
    \beta_{3}\,z^{\text{exports}}+
    \beta_{4}\,z^{\text{output}}+
    \beta_{5}\,z^{\text{infl}}+
    \beta_{6}\,z^{\text{credit}}
\right)
$$

$$\sigma(u)=\frac{1}{1+e^{-u}}$$

Değişkenler

`ca`, Cari İşlemler Hesabı (Current Account)

GMD'deki `CA_GDP` kolonunu baz alır, bu kolon cari işlemler hesabının
gayrisafi yurtiçi hasılasına (GDP) göre oranıdır. Risk artışını
hesaplamaya uğraştığımız için kod içinde kolonun negatifi alındı
`(-g["CA_GDP"])`. Cari işlemler hesabı bir ülkenin ticaret dengesini
gösterir, ve net olarak dışarıdan ne kadar getiri elde edildiğini
kayıtlar. Eğer cari işlemler hesabında kayıp var ise bu bir ülkenin
dışarıdan kazandığından daha fazlasını dışarı ödediği anlamına gelir,
ve farkı kapatmak için dış sermaye akışına ihtiyaç duymaktadır / onu
kullanmaktadır. Negatif alarak cari açığı (negatif değer) pozitif bir
risk seviyesine çeviriyoruz, çünkü dış sermayenin birden bire durması
mümkündür bu da ülkenin kuru üzerinde bir risk yaratır.

`rer_dev`, gerçek döviz kuru trendinden sapma (Real Exchange Rate
Deviation)

Bu hesabı yapmadan önce onun baz aldığı `rer` nasıl hesaplanır onu
görelim. Kriz tahmini için yayınlanan döviz kuru rakamı tabii ki
yeterli değil, zaten yeterli olsaydı bu belgede anlatılan tüm
taklalara gerek kalmazdı. Saptanmaya uğraşılan bir ülke kurunun
özellikle sabitlenmiş rejimlerde suni olarak belli seviyelerde
olmaları fakat makro ekonomik olaylar sebebiyle savunulacak hale
gelmesi ve devalüasyon durumu... Bu durumda, listelenen kur tabii ki
tahmin için yeterli değil çünkü kur doğal makroekonomik anlamda doğru
değil.

O zaman "gerçek" kuru nasıl hesaplarız? Bir ülkenin fiyat seviyesini
mesela ABD fiyat seviyesi ile karşılaştırabiliriz. Eğer bir ülkenin
fiyat seviyesi `CPI` ise onu ABD ile `US_CPI` / `CPI` formülüyle
karşılaştırabiliriz. `CPI` ekonomi çalışanların bilebileceği üzere
hayali bir alışveriş sepetini doldurmanın kaça mal olacağını
hesaplar. Gündelik ihtiyaçlar, yiyecek, içecek, ev kirası, giyim,
vs. gibi pek çok alım kalemi onun içinde mevcuttur.

Dikkat, CPI enflasyon hesabı degildir, enflasyon bu sepetin yıldan
yıla değişimi üzerinden hesaplanır.







[devam edecek]

Kaynaklar

[1] <a href="https://github.com/KMueller-Lab/Global-Macro-Database">GMD Github</a>

[2] <a href="https://www.globalmacrodata.com/">GMD</a>

[3] <a href="https://www.globalmacrodata.com/research-paper.html">GMD Makalesi</a>

[4] Reinhart, <a href="https://www.imf.org/external/pubs/ft/staffp/1998/03-98/pdf/kaminsky.pdf">Leading Indicators of Currency Crises</a>

