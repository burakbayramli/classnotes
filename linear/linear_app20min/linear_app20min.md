# Lineer Cebir ile Minimizasyon

Diyelim ki $Ax$ değerinin mümkün olduğu kadar 0'a yakın olmasını istiyoruz,
yani öyle $x$ değerleri arıyoruz ki $Ax$ olabildiğince sıfıra yakın olsun, bir
şartla, $||x||=1$ olmalı. Bu bir minimizasyon problemidir [8].

$$ \min_{x} ||Ax||^2 \quad \textrm{ öyle ki } \quad ||x||^2 = 1  $$

Her iki ifadeyi açarsak,

$$ ||Ax||^2  = (Ax)^T(Ax) = x^TA^TAx $$

$$ ||x||^2 = x^Tx = 1$$

Optimizasyon için alttaki bedel fonksiyonunu yazabiliriz,

$$ L(x) = x^TA^TAx - \lambda (x^Tx-1) $$

Bu bedele Lagrangian bedeli denir ve $\lambda$ Lagrange çarpanıdır. Lagrangian
terimi kısıtlama şartını bedelin içine gömülmesini sağlar, böylece iki ayrı
ifade yerine tek ifade yeterli oluyor. Artık minimizasyonu şöyle yazabiliriz,

$$ \min_{x} \left\{ L(x) = x^T A^T A x - \lambda (x^T x-1) \right\} $$

$x$'e göre türev alırsak,

$$ A^TAx - \lambda x = 0 $$

$$ A^TAx = \lambda x $$

Bu ifade bir özvektör problemidir, $A^TA$'nin özvektörleri vardır, şimdi
$\lambda$'yi özdeğer gibi görebiliriz, ve her farklı özdeğere tekabül eden
özvektör üstteki problemi çözer. Bu farklı $x$'lere $x_\lambda$ diyelim. Ama
hangi $x_\lambda$'yi istiyoruz? Bedeli şu şekilde tekrar yazalım,

$$ L(x_\lambda) = x_\lambda^T A^T A x_\lambda - \lambda (x_\lambda^Tx_\lambda-1) $$

Özvektör tanımından $A^TA  x_\lambda = \lambda x_\lambda$ olduğuna göre üstte
yerine koyarsak ve sağdaki terim iptali yaparsak,

$$  = \lambda x_\lambda^T  x_\lambda - \cancel{\lambda (x_\lambda^Tx_\lambda-1)} $$

$x_\lambda^T  x_\lambda = 1$ olduğu için,

$$ L(x_\lambda) = \lambda x_\lambda^T  x_\lambda  = \lambda$$

Yani bedel fonksiyonu her $x_\lambda$ için o özvektörün bağlantılı olduğu
$\lambda$ değerini verir. Böylece minimizasyon için hangi $x_\lambda$'yi
seçmeliyiz sorusunun cevabını vermiş oluyoruz: en küçük $\lambda$'nin
$x_\lambda$'sı!

Örnek

Sıfır uzayı kavramını gördük, eğer $A$ tam kertede değil ise sıfır uzayı boş
değildir. Bir örnek uyduruyorum, mesela

$$
\left[\begin{array}{rrr} 1 & 2 \\ 3 & 6 \end{array}\right] x = 0
$$

Bu örnek için $x$'in ne olduğunu biliyorum, kolonlar bağımsız değil, 2. kolon
1.'nin iki katı, yani $\left[\begin{array}{cc} -2 & 1 \end{array}\right]$ bu
problemi çözer, ya da bir öğesi diğerinin negatif iki katı olan herhangi bir
diğer $x$. Peki ya problem şöyle olsaydı?

$$
\left[\begin{array}{cc} 1 & 2 \\ 3 & 5.5 \end{array}\right] x = 0
$$

Şimdi direk $\left[\begin{array}{cc} -2 & 1 \end{array}\right]$ diyemeyiz, ama
biliyoruz ki sıfıra olabildiğince yaklaşabilmek mümkün, bu problemi minimizasyon
olarak çözmek lazım.

```python
import numpy.linalg as lin
A = np.array([[1,2],[3,5.5]])
eval,evec = lin.eig(np.dot(A.T,A))
print (eval)
print (evec)
```

```
[[-0.8798189  -0.47530906]
 [ 0.47530906 -0.8798189 ]]
[  5.65043904e-03   4.42443496e+01]
```

En küçük özdeğer birincisi,

```python
print (evec[:,0])
```

```
[-0.8798189   0.47530906]
```

Yani $Ax$'i sıfıra en yaklaştıran çozum $x = \left[\begin{array}{cc} -0.88 &
 0.47 \end{array}\right]$. Görüldüğü gibi 1. öğe ikincisinin negatif iki
"katımsı''. 

Temel Bileşen Analizi (bkz [2]) tekniğinde görülecek, bu tekniğin
bahsettiğimiz minimizasyon ile yakın alakaları var. Eğer $A$ matrisi
kolonlarını belli ölçümler, yaş, ağırlık, vs gibi düşünürsek, bu ölçümler
üzerinden kovaryansın ne olduğunu biliyoruz: $A^TA$. Değil mi? Peki şimdi
şu sorunun cevabını nasıl veririz?  Öyle yönler bul ki $A^TA$ o yönlerde
kovaryans yansıması minimal ya da maksimal olsun.

Yön demek bir birim vektördür, $x$ diyelim, yani gene $A^TAx$'yi minimize /
maksimize etmeye geldik (ya da $x^TA^TAx$, aynı şey)! Şart $||x||^2 = 1$ aynen
olduğu gibi geçerli çünkü sadece bir yön arıyoruz. Kovaryansın minimal, maksimal
olduğu yerler öyle yönler olacak ki o yönlerde değişkenlerin beraber değişimi en
az, ya da en fazla olacak, altta örnek PCA örnek grafiğinde görülüyor,
noktaların sağ üste doğru "beraber'' uzandığı yer en fazla bağlantı, ona dik
olan diğer yönde en az bağlantı var. Bunlar temel bileşenler.

Rayleigh Bölümü (Quotient)

Başlangıçtaki minimizasyon formatına dönersek, daha genel bir ifade ile, eğer
$M$ simetrik ise (daha önceki örnekte $A^TA$ kullandık, ama bu ifade de her
zaman simetriktir, çünkü matrisin devriği çarpı kendisi her zaman simetrik bir
matris doğurur),

$$ 
R(M,x) = \frac{x^TMx}{x^Tx} 
\qquad (1) 
$$

ifadesi de doğru olmalı. $R$'ye Rayleigh bölümü adı veriliyor, ve eşitliğin sağı
biraz önce gördüğümüz gibi minimal noktasına en küçük özdeğer/vektör ikilisiyle
gelir. Üstte bir oran görülüyor, fakat bu karışıklık yaratmasın, daha önce $x^Tx
= 1$ şartını ayrı bir şekilde yazmıştık, ve $x^T M x$ minizasyonu
yapmıştık. Diyelim ki $x$ değil $v$ kullandık ve $v$ herhangi bir vektör
olabilir, fakat herhangi bir vektörü birim vektör haline getirmeyi biliyoruz, $x
= v/||v||$, ve $x^T M x$ içinde yerine koyarsak (1)'i elde ederiz [1].

Böylece ileride göreceğimiz Rayleigh-Ritz Teorisi'nin ispatının bir kısmına da
farklı bir çözüm getirmiş olduk.

Optimizasyonu bölüm olarak belirtmenin bazı faydalı var, sınır şartının illa 1'e
eşit olma zorunluğu bazı uygulamalar için çok kısıtlayıcı olabilir.

Örnek

Karesel denklemler de matris formunda gösterilebilir, mesela

$$ q(x,y) = 3x^2 + 2xy + 3y^2 $$

ile

$$
\left[\begin{array}{cc} x & y \end{array}\right]
\left[\begin{array}{rrr}3 & 1 \\ 1 & 3 \end{array}\right]
\left[\begin{array}{c} x \\ y \end{array}\right]
$$

aynı şey. Problem $q$'yu $x^2+y^2 = 1$ olacak şekilde optimize etmek. Fakat
artık şu şekilde de tanımlayabiliriz,

$$
r(x,y) = \frac{ 3x^2 + 2xy + 3y^2}{x^2+y^2 }
$$

Çözüm $\lambda_1=4$ ve $\lambda_2 = 2$.

$$
q(-1/\sqrt{2},1/\sqrt{2}) =
2 \le q(x,y) \le 4 =
q(1/\sqrt{2},1/\sqrt{2})
$$


Rayleigh-Ritz Teoremi

Sentetik görüntü algoritmasını gösterdiğimizde, Rayleigh-Ritz kuramına atıf
yapmıştık. Bu yazıda bütün kuramın ispatını veriyoruz. İspatta kullanılan
küme sanal sayılar kümesidir. Bizim örneğimiz için gerçek sayılar kümesi
kullanılıyor, fakat aynı ispat hala geçerli olacak.

Problem

Bir kare matrisin özdeğerlerini büyüklük sırasına dizersek, bu değerlerin
kısıtlı bir minimizasyon / maksimizasyon probleminin çözümü olduğun
görüyoruz. Kısıtlı derken, $x*x$ (x vektör devriği çarpı $x$, yani x'in
uzunluğu) çarpımını 1'e kısıtlı tutmaktan bahsediyorum. Böylece
maksimizasyon problemimizin sonsuzluğa gitmesini engellemiş
oluyoruz. $\lambda$ sembolu genelde özdeğerler için kullanılır. Yıldız
işareti * ise sanal sayılar uzayında, devrik yapmak demektir. Gerçek
sayılar uzayında olsaydık, o zaman T işaretini kullanabilirdik. (T
transpose kelimesinden gelir).

$$ \textrm{forall } x \in \ C^n  $$

$$ \lambda_1x^\ast x \le x^\ast A x \le \lambda_nx^\ast x  $$

$$ 
\lambda_{ust} = \lambda_n = 
\max\limits_{x^\ast x=1} (\frac{x^\ast A x}{x^\ast x}) =
\max\limits_{x^\ast x=1}(x^\ast Ax)
$$

$$ 
\lambda_{alt} = \lambda_n = 
\max\limits_{x^\ast x=1} (\frac{x^\ast Ax}{x^\ast x}) =
\max\limits_{x^\ast x=1}(x^\ast Ax)
$$

Problemi üstte tanımladıktan sonra, ispatına gelelim. 

A matrisi, Hermit matrisi olduğu için, elimizde bu A matrisine tekabül eden
birincil (unitary) bir matris var demektir. Bu birincil matrisi U ile
temsil edersek, şu sonuca da varırız.

$$ A = U \Lambda U^\ast $$

$$ \Lambda = diag(\lambda_1\lambda_2...,\lambda_n) $$

Bu demektir ki, 

$$ \forall x \in  C^n $$

$$ x^\ast A x = x^\ast U \Lambda U^\ast x = (U^\ast x)^\ast \Lambda(U^\ast x) $$

$$
\sum_{i=1}^n \lambda_i |(U^\ast x)_i|^2
$$

Ufak iki not olarak düşmek gerekiyor. Yukarıdaki 3. eşitliğe gelmemizin
sebebi aşağıdakinin doğru olmasıdır.

$$
x^\ast U = (U^\ast x)^\ast
$$

Doğrusal cebirde bilinen çevirimlerden biridir bu. En son not olarak, toplamlı
eşitliğe gelebilmemizin sebebi (4. terim) şundandır. $U^\ast x$ yerine $W$
koyarsak, $W^\ast W$ çarpımının her zaman $W$'nin uzunluğunu verir. Yani bir
vektörün uzunluğunu bulmak için vektörün devriğini kendisi ile çarpmak gerekir,
bu çarpım uzunluğun karesidir.

Devam ediyoruz. Her $|(U^\ast x)_i|^2$ ifadesi artı değerli olmaya mecbur
olduğu için,

$$
\lambda_{alt} \sum_{i=1}^n | (U^\ast x)_i |^2 \le x^\ast Ax 
$$

$$
\sum_{i=1} \lambda_i | (U^\ast x) |^2 \le
\lambda_{ust} \sum_{i=1}^n | (U^\ast x)_i|^2 \le
x^\ast Ax 
$$

Üstteki eşitsizliğin doğru olmasının bir sebebi var. Elimizde 3 tane
değişik 1..n arası yapılan toplam var. Dikkatle bakarsanız, ortadaki
toplam içinde i ile kontrol edilen, bütün özdeğerlerin toplandığını
göreceksiniz. Buna kıyasla mesela en soldaki, toplam içinde sürekli aynı
'alt özdeğer' toplandığını farketmemiz lazım. Buna bakarak anlıyoruz ki,
tabii ki bütün özdeğerlerin toplamı, tekrar eden aynı özdeğer değerinin
toplamından fazla olacaktır! Çünkü iki tarafta da özdeğerler haricindeki
bütün terimler birbirine eşit. Daha da basitleştirmek için U'yu yokedelim.

U birincil bir matris, o zaman

$$
\sum_{i=1}^{n} |(U^{\ast} x)_{i}|^{2}
\sum_{i=1} | x_{i} |^2 = X^{\ast} x
$$

çünkü

$$
|U^\ast x| = |x|
$$

İspat

$$
|U^\ast x| = (U^\ast x)^\ast(U^\ast x) = x^\ast UU^\ast x = x^\ast x = | x |
$$

Böylece göstermiş oluyoruz ki, 

$$
\lambda_1 x^\ast x \le \lambda_{alt} x^\ast x \le x^\ast Ax \le \lambda_{ust}
x^\ast x
$$ 

Kaynaklar

[1] Olver, *Applied Linear Algebra*

[2] Bayramlı, Istatistik, *Asal Bileşen Analizi (Principal Component Analysis -PCA-)*



