# Tahmin Edici Hesaplar (Estimators)

### Maksimum Olurluk (Maximum Likelihood)

Maksimum olurluk bir kestirme hesabı yapmanın yolu. Bu teknik ile
verinin her noktası teker teker olasılık fonksiyonuna geçilir, ve elde
edilen olasılık sonuçları birbiri ile çarpılır. Çoğunlukla formül
içinde bilinmeyen bir(kaç) parametre vardır, ve bu çarpım sonrası,
içinde bu parametre(ler) olan yeni bir formül ortaya çıkar. Püf nokta
şudur ki eğer bir model "doğru" ya da doğruya yakın modelse o veri
üzerinde işletilince yüksek olasılık değerler rapor edilecektir. Öyle
ya, mesela Gaussian $N(10,2)$ dağılımı var ise, 60,90 gibi değerlerin
"olurluğu'' düşüktür.

Demek ki elimizde bir maksimizasyon, optimizasyon problemi var. O
zaman çarpımlardan elde edilen nihai formülün kısmı türevi alınıp
sıfıra eşitlenince cebirsel bazı teknikler ile bilinmeyen parametre
bulunabilir. Bu sonuç eldeki veri bağlamında en mümkün (olur)
parametre değeridir. Gaussin üzerinde örnek,

$$
f(x;\mu,\sigma) = \frac{1}{\sigma\sqrt{2\pi}} 
\exp \bigg\{ - \frac{1}{2\sigma^2}(x-\mu)^2  \bigg\}
, \ x \in \mathbb{R}
$$
 
Çarpım sonrası

$$ f(x_1,..,x_n;\mu,\sigma) = 
\prod \frac{1}{\sigma\sqrt{2\pi}} 
\exp \bigg\{ - \frac{1}{2\sigma^2}(x_i-\mu)^2  \bigg\}
$$

$$ =
\frac{(2\pi)^{-n/2}}{\sigma^n}
\exp \bigg\{ - \frac{\sum (x_i-\mu)^2}{2\sigma^2}  \bigg\}
$$

Üstel kısım $-n/2$ nereden geldi? Çünkü bölen olan karekökü üste çıkardık,
böylece $-1/2$ oldu, $n$ çünkü $n$ tane veri noktası yüzünden formül $n$
kere çarpılıyor. Veri noktaları $x_i$ içinde. Eğer log, yani $\ln$ alırsak
$\exp$'den kurtuluruz, ve biliyoruz ki log olurluğu maksimize etmek normal
olurluğu maksimize etmek ile aynı şeydir, çünkü $\ln$ transformasyonu
monoton bir transformasyondur. Ayrıca olurluk içbukeydir (concave) yani
kesin tek bir maksimumu vardır. 

$$ \ln f = -\frac{1}{2} n \ln (2\pi) 
- n \ln \sigma - 
\frac{\sum (x_i-\mu)^2}{2\sigma^2}  
$$

Türevi alıp sıfıra eşitleyelim

$$ \frac{\partial (\ln f)}{\partial \mu} =
\frac{\sum (x_i-\mu)^2}{2\sigma^2}   = 0 
$$

$$ \hat{\mu} = \frac{\sum x_i }{n} $$

Bu sonuç (1)'deki formül, yani örneklem ortalaması ile aynı! Fakat buradan
hemen bir bağlantıya zıplamadan önce şunu hatırlayalım - örneklem
ortalaması formülünü *biz* tanımladık. "Tanım'' diyerek bir ifade
yazdık, ve budur dedik. Şimdi sonradan, verinin dağılımının Gaussian
olduğunu farzederek, bu verinin mümkün kılabileceği en optimal parametre
değeri nedir diye hesap ederek aynı formüle eriştik, fakat bu bir anlamda
bir güzel raslantı oldu.. Daha doğrusu bu aynılık Gaussian / Normal
dağılımlarının "normalliği'' ile alakalı muhakkak, fakat örnekleme
ortalaması hiçbir dağılım faraziyesi yapmıyor, herhangi bir dağılımdan
geldiği bilinen ya da bilinmeyen bir veri üzerinde kullanılabiliyor. Bunu
unutmayalım. İstatistikte matematiğin lakaytlaşması (sloppy) kolaydır, o
sebeple neyin tanım, neyin hangi faraziyeye göre optimal, neyin nüfus
(population) neyin örneklem (sample) olduğunu hep hatırlamamız lazım.

Devam edelim, maksimum olurluk ile $\hat{\sigma}$ hesaplayalım,

$$ \frac{\partial (\ln f)}{\partial \sigma} =
-\frac{n}{\sigma} + \frac{\sum (x_i-\mu)^2}{2\sigma^3}   = 0 
$$

Cebirsel birkaç düzenleme sonrası ve $\mu$ yerine yeni hesapladığımız
$\hat{\mu}$ kullanarak,

$$ \hat{\sigma}^2 = \frac{\sum (x_i-\hat{\mu})^2}{n} $$

Bu da örneklem varyansı ile aynı! 

Yansızlık (Unbiasedness)

Tahmin edicilerin kendileri de birer rasgele değişken olduğu için her
örneklem için değişik değerler verirler. Diyelim ki $\theta$ için bir
tahmin edici $\hat{\theta}$ hesaplıyoruz, bu $\hat{\theta}$ gerçek $\theta$
için bazı örneklemler için çok küçük, bazı örneklemler için çok büyük
sonuçlar (tahminler) verebilecektir. Kabaca ideal durumun, az çıkan
tahminlerin çok çıkan tahminleri bir şekilde dengelemesi olduğunu tahmin
edebiliriz, yani tahmin edicinin üreteceği pek çok değerin $\theta$'yı bir
şekilde "ortalaması'' iyi olacaktır.

![](unbias.png)

Bu durumu şöyle açıklayalım, madem tahmin ediciler birer rasgele değişken,
o zaman bir dağılım fonksiyonları var. Ve üstteki resimde örnek olarak
$\hat{\theta_1},\hat{\theta_2}$ olarak iki tahmin edici gösteriliyor mesela
ve onlara tekabül eden yoğunluklar $f_{\hat{\theta_1}},
f_{\hat{\theta_1}}$. İdeal durum soldaki resimdir, yoğunluğun fazla olduğu
yer gerçek $\theta$'ya yakın olması. Bu durumu matematiksel olarak nasıl
belirtiriz? Beklenti ile!

Tanım

$Y_1,..,Y_n$ üzerindeki $\theta$ tahmin edicisi $\hat{\theta}$'den alınmış
rasgele örneklem. Eğer tüm $\theta$'lar için $E(\hat{\theta}) = \theta$
işe, bu durumda tahmin edicinin yansız olduğu söylenir.

Örnek olarak maksimum olurluk ile önceden hesapladığımız $\hat{\sigma}$
tahmin edicisine bakalım. Bu ifade

$$ \hat{\sigma}^2 = \frac{1}{n}\sum (Y_i-\hat{\mu})^2 $$

ya da 

$$ \hat{\sigma}^2 = \frac{1}{n}\sum_i (Y_i-\bar{Y})^2 $$

ile belirtildi. Tahmin edici $\hat{\sigma}^2$, $\sigma^2$ için yansız midir?
Tanımımıza göre eğer tahmin edici yansız ise $E(\hat{\sigma}^2) = \sigma^2$
olmalıdır.

Not: Faydalı olacak bazı eşitlikler, daha önceden gördüğümüz

$$ Var(X) = E(X^2) - (E(X)^2)$$

ve sayısal ortalama $\bar{Y}$'nin beklentisi $E({\bar{Y}}) = E(Y_i)$, ve
$Var(\bar{Y}) = 1/n Var(Y_i)$.

Başlayalım,

$$ E(\hat{\sigma}^2) = E\bigg(\frac{1}{n}\sum_i (Y_i-\bar{Y})^2 \bigg)$$

Parantez içindeki $1/n$ sonrasındaki ifadeyi açarsak,


$$ \sum_i (Y_i-\bar{Y})^2  =  \sum_i (Y_i^2-2Y_i\bar{Y}+ \bar{Y}^2)$$

$$ = \sum_iY_i^2 -2\sum_i Y_i\bar{Y} + n\bar{Y}^2  $$

$\sum_i Y_i$'nin hemen yanında $\bar{Y}$ görüyoruz. Fakat $\bar{Y}$'nin kendisi
zaten $1/n \sum_i Y_i$ demek değil midir? Ya da, toplam içinde her $i$ için
değişmeyecek $\bar{Y}$'yi toplam dışına çekersek, $\bar{Y}\sum_iY_i$ olur, bu da
$\bar{Y} \cdot n \bar{Y}$ demektir ya da $n\bar{Y}^2$,

$$ = \sum_iY_i^2 -2 n\bar{Y}^2 + n\bar{Y}^2  $$

$$ = \sum_iY_i^2 -n\bar{Y}^2  $$

Dikkat, artık $-n\bar{Y}^2$ toplama işleminin *dışında*. Şimdi beklentiye
geri dönelim,

$$ = E \bigg( \frac{1}{n} \bigg( \sum_iY_i^2 -n\bar{Y}^2 \bigg) \bigg) $$

$1/n$ dışarı çekilir, beklenti toplamdan içeri nüfuz eder,

$$ = \frac{1}{n} \bigg(  \sum_i  E(Y_i^2) -n E(\bar{Y}^2) \bigg) $$

Daha önce demiştik ki (genel bağlamda)

$$ Var(X) = E(X^2) - (E(X)^2)$$

Bu örnek için harfleri değiştirirsek,

$$ Var(Y_i) = E(Y_i^2) - E(Y_i)^2$$

Yani

$$ E(Y_i^2) = Var(Y_i) + E(Y_i)^2 $$

$E(Y_i) = \mu$ oldugunu biliyoruz,

$$ E(Y_i^2) = Var(Y_i) + \mu^2 $$

Aynısını $E(\bar{Y}^2)$ için kullanırsak,

$$  E(\bar{Y}^2) = Var(\bar{Y}) + E(\bar{Y})^2 $$

$E(\bar{Y}) = \mu$, 

$$  E(\bar{Y}^2) = Var(\bar{Y}) + \mu^2 $$

$$
= \frac{1}{n} \bigg(  \sum_i Var(Y_i) + \mu^2   
-n (Var(\bar{Y}) + \mu^2 ) \bigg) 
$$

$Var(Y_i) = \sigma$, ve başta verdiğimiz eşitlikler ile beraber

$$
= \frac{1}{n} \bigg(  \sum_i (\sigma^2 + \mu^2)
-n (\frac{\sigma^2}{n} + \mu^2 ) \bigg) 
$$

Tekrar hatırlatalım, $\sum_i$ sadece ilk iki terim için geçerli, o zaman,
ve sabit değerleri $n$ kadar topladığımıza göre bu aslında bir çarpım
işlemi olur,

$$ 
= \frac{1}{n} \bigg(  n\sigma^2 + n\mu^2   
-n (\frac{\sigma^2}{n} + \mu^2 ) \bigg) 
$$

$$ 
=  \sigma^2 + \mu^2 -\frac{\sigma^2}{n} - \mu^2 
$$

$$ 
=  \sigma^2 -\frac{\sigma^2}{n} 
$$

$$ 
=  \frac{n\sigma^2}{n} -\frac{\sigma^2}{n} 
$$


$$ 
=  \frac{n\sigma^2 - \sigma^2}{n} 
$$

$$ 
=  \frac{\sigma^2(n-1)}{n} 
$$

$$ = \sigma^2 \frac{n-1}{n} $$

Görüldüğü gibi eriştiğimiz sonuç $\sigma^2$ değil, demek ki bu tahmin edici
yansız değil. Kontrol tamamlandı.

Fakat eriştiğimiz son denklem bize başka bir şey gösteriyor, eğer üstteki
sonucu $\frac{n}{n-1}$ ile çarpsaydık, $\sigma^2$ elde etmez miydik? O
zaman yanlı tahmin ediciyi yansız hale çevirmek için, onu $\frac{n}{n-1}$
ile çarparız ve

$$ \frac{n}{n-1} \frac{1}{n}\sum_i (Y_i-\bar{Y})^2 $$

$$ =  \frac{1}{n-1}\sum_i (Y_i-\bar{Y})^2 $$

Üstteki ifade $\sigma^2$'nin yansız tahmin edicisidir. 

Hesap için kullandığınız kütüphanelerin yanlı mı yansız mı hesap yaptığını
bilmek iyi olur, mesela Numpy versiyon 1.7.1 itibariyle yanlı standart
sapma hesabı yapıyor, fakat Pandas yansız olanı kullanıyor (Pandas
versiyonu daha iyi)

```python
import pandas as pd
arr = np.array([1,2,3])
print ('numpy', np.std(arr))
print ('pandas', pd.DataFrame(arr).std().iloc[0])
```

```
numpy 0.816496580928
pandas 1.0
```

Binom ve $p$ İçin Maksimum Olurluk Tahmini [1]

$$ L(p;x) = \prod_{i=1}^n f(x_i;p) = \prod_{i=1}^n {n \choose x} p^x(1-p)^{1-x} $$

Log alalım

$$ \log L(p;x) = 
\sum_{i=1}^n \log {n \choose x} + x \log p + (1-x) \log (1-p) $$

$p$'ye göre türevi alalım, bu sırada kombinasyon ifadesi ${n \choose x}$
içinde $p$ olmadığı için o yokolacaktır,

$$ \frac{\partial \log L(p)}{\partial p} =
\frac{x}{p} - \frac{n-x}{1-p}
$$

Maksimum değeri bulmak için sıfıra eşitleyelim ve $p$ için çözelim,

$$ 0 = \frac{x}{p} - \frac{n-x}{1-p} $$

$$  \frac{x}{p} = \frac{n-x}{1-p}  $$

$$ p(n-x)  = x(1-p) $$

$$ pn - px = x-px $$

$$ pn = x $$

$$ p = \frac{x}{n} $$

Yani $p$ için maksimum olurluk tahmini $x/n$. 

Bernoulli dağılımı Binom dağılımına çok benzer, sadece onun baş kısmında
kombinasyon ifadesi yoktur. Fakat o ifade $p$'ye göre türevde nasıl olsa
yokolacağına göre Bernoulli dağılımı için de tahmin edici aynıdır.

### Maksimum Sonsal Hesabı (Maximum a Posteriori / MAP)

Maksimum Olurluk hesabı için bir bilinmeyen parametre, diyelim ki
$\theta$'yi kestirmeye uğraştık ve bunu için verinin formülünü
oluşturduk. Bu formülde parametre sabit idi fakat bilinmiyordu. Eğer
perspektifimizi biraz değiştirirsek ve bilinmeyen parametre
$\theta$'yi da bir rasgele değişken olarak addedersek, bu bize
tahminsel hesaplama açısından biraz daha esneklik sağlayacaktır.

Bakış açısı Bayes olacak, bulmak istediğimiz $\theta_{MAP}$ tahmini,
bunun için olurluk $P(\theta | D)$'yi maksimize eden bir $\theta$
bulmaya uğraşacağız. $P(D|\theta)$ içeren formül Bayes yaklaşımıyla
şöyledir,

$$
P(\theta | D) = \frac{P(D|\theta) P(\theta) }{P(D)}
$$

$P(\theta|D)$: Sonsal dağılım, bize verilmiş olan ham veriyi dikkate
alınca tahmin $\theta$'nin olasılığı nedir? "Verilmiş olması" terimine
dikkat, Bayes yaklaşımı ile veriyi bile bir rasgele değişken kabul
ediyoruz, ve onun verilmiş olması, bu değişkenin koşulsal bir bağ ile
birleşik dağılıma dahil edilmesini ima ediyor.

$P(D|\theta)$: Olurluk, elimizdeki tahmin parametresini hesaba
katarsak verinin olurluğu nedir?

$P(\theta)$: Önsel dağılım. Hiçbir şeyi bilmeden önce tahminimizin
olasılığı neydi?

$P(D)$: Kısmi (margınal) olasılık. Mümkün tüm tahmin parametreleri
üzerinden bakınca yeni elde edilen gözlemin olasılığı nedir?



[devam edecek]



Kaynaklar

[1] Unpingco, Python for Probability, Statistics and Machine Learning

