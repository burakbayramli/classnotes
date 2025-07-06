# Çok Değişkenli Gaussian Dağılımlar

Çok değişkenli normal dağılımlarla iş yaparken, mesela Gaussian karışımları
kullanırken, bazı numaraları bilmek faydalı olabiliyor. Bunlardan birincisi
$(x-\mu)^T\Sigma^{-1}(x-\mu)$ hesabını yapmaktır, diğer log-toplam-exp
numarası (logsumexp trick) diye bilinen hesaptır.

Birinciden başlayalım, daha kısalaştırmak için $y=x-\mu$ diyelim, yani
$y^T\Sigma^{-1}y$ olsun. Şimdi bu formülde bir ters alma (inversion)
işleminin olduğunu görüyoruz. Fakat bu işlem oldukça pahalı bir işlem
olarak bilinir, hele hele boyutların yükseldiği durumlardan (binler,
onbinler), kovaryansı temsil eden $\Sigma$, $n \times n$ olacaktır. Acaba
tersini almayı başka bir şekilde gerçekleştiremez miyiz?

$\Sigma$ matrisi bir kovaryans matrisi olduğu için simetrik, pozitif yarı
kesin bir matristir. Bu tür matrislerin Cholesky ayrıştırmasının olduğunu
biliyoruz ve bu işlem çok hızlı yapılabiliyor. O zaman 

$$ \Sigma = LL^T $$

ki $L$ matrisi alt-üçgensel (lower triangular) bir matristir,

$$ \Sigma^{-1} = (LL^T)^{-1} $$

$$ = L^{-T}L^{-1} $$

Bunu temel alarak iki taraftan $y$'leri geri koyalım,

$$ y^T\Sigma^{-1}y= y^TL^{-T}L^{-1}y $$

Bilindiği gibi lineer cebirde istediğimiz yere parantez koyabiliriz,

$$ = (y^TL^{-T})L^{-1}y $$

Parantezden bir şeyin devriği gibi temsil edersek, parantez içindekilerin
sırası değişir ve tek tek devriği alınır,

$$ = (L^{-1}y)^TL^{-1}y $$

$$  = |L^{-1}y|^2 $$

Üstteki ifadede $|\cdot|$ içindeki kısım $Ax=b$ durumundaki $x$'in en az
kareler çözümü olan $A^{-1}b$'ye benzemiyor mu? Evet. Gerçi $n \times n$
boyutunda bir matris olduğu için elimizde "bilinmeyenden fazla denklem''
yok, yani bu sistem artık belirtilmiş (overdetermined) değil, yani en az
kareler değil direk lineer sistem çözümü yapıyoruz. Bu durumda her standart
lineer cebir kütüphanesinde mevcut bir çağrı kullanacağız, mesela
`solve_triangular` (ve lower -alt- doğru seçeneğini kullanacağız), ki
bu çağrı özellikle alt üçgensel matris üzerinden çözüm yapmaktadır, çünkü
$L$ alt-üçgensel olduğu için çözüm geriye değer koymak (back substitution)
ile anında bulunabilir. Geriye değer koymayı hatırlarsak, mesela

$$ 
\left[\begin{array}{cc}
2 & 0 \\
3 & 4
\end{array}\right]
\left[\begin{array}{c}
x_1\\
x_2
\end{array}\right]
= 
\left[\begin{array}{c}
6\\
8
\end{array}\right]
$$

En üst satırda her zaman tek bir bilinmeyen olacak, çünkü matris alt üçgensel,
en üst satır her zaman en boş satırdır. Bu tek bir eşitlik
demektir, yani $2x_1 = 6$, ki $x_1 = 3$. Bunu alıp bir sonraki satıra gideriz,
artık $x_1$'i biliyoruz, sonraki satırda sadece $x_2$ bilinmeyen
kalıyor, $3\cdot x_1 + 4 \cdot x_2 = 8$, yani $x_2 = -1/4$. Sonuca
ulaştık. Daha fazla boyut olsaydı durum değişmezdi, aynı işlem daha fazla
tekrarlanırdı. Bu arada bu türden bir çözümün ne kadar hızlı olacağını
belirtmemize gerek yok herhalde.

Demek ki $y^T\Sigma^{-1}y$ hesabı için önce $\Sigma$ üzerinde Cholesky
alıyoruz, sonra $L^{-1}y$ çözdürüyoruz. Elde edilen değerin noktasal
çarpımını alınca $\Sigma$'nin tersini elde etmiş olacağız. 

Örnek (önce uzun yoldan),

```python
import numpy.linalg as lin
Sigma = np.array([[10., 2.],[2., 5.]])
y = np.array([[1.],[2.]])
print (np.dot(np.dot(y.T,lin.inv(Sigma)),y))
```

```
[[ 0.80434783]]
```

Şimdi Cholesky ve `solve_triangular` üzerinden

```python
import scipy.linalg as slin
L = lin.cholesky(Sigma)
x = slin.solve_triangular(L,y,lower=True)
print (np.dot(x.T,x))
```

```
[[ 0.80434783]]
```

Aynı sonuca eriştik.



Çok Boyutlu Gaussian'ı Parçalamak (Partitioning)

Diyelim ki Normal bir vektör $X$'i $X = (X_1,X_2)$ olarak parçaladık. Bunu
Gaussian'a etkileri ne olur? Aynı şekilde $\mu = (\mu_1,\mu_2)$ olarak
parçalayabiliriz. $\Sigma$ ise

$$ \Sigma = 
\left[\begin{array}{rr}
\Sigma_{11} & \Sigma_{12}\\
\Sigma_{21} & \Sigma_{22}
\end{array}\right]
$$

olarak parçalanabilir. $a,b$'nin parçalarının boyutları $p,q$ olsun, $n = p+q$.

Şimdi birleşik Gaussian'ı 

$$
f(x;\mu,\Sigma) = 
\frac{ 1}{(2\pi)^{(p+q)/2} \det(\Sigma)^{1/2}} 
\exp 
\bigg\{ 
-\frac{ 1}{2}
\left[\begin{array}{r}
x_1 - \mu_1\\
x_2 - \mu_2
\end{array}\right]^T
\left[\begin{array}{rr}
\Sigma_{11} & \Sigma_{12}\\
\Sigma_{21} & \Sigma_{22}
\end{array}\right]^{-1}
\left[\begin{array}{r}
x_1 - \mu_1\\
x_2 - \mu_2
\end{array}\right]
\bigg\}
$$

Birleşik yoğunluğu parçalar üzerinden belirtirsek, bu yoğunluğu $X_2$ için
bileşen yoğunluğa ve $X_1$ için bir koşullu yoğunluğa ayırabiliriz. Yani 

$$ f(x_1,x_2) = f(x_1|x_2) f(x_2) $$

tanımındaki parçaları elde etmeye çalışacağız.  Ama bundan önce
bölüntülenmiş matrislere yakından bakalım. 

Bir bölüntülenmiş (partitioned) matrisin tersini almak için, o matrisin
parçalarının tersini almak doğru değildir, yani

$$ 
\left[\begin{array}{rr}
E & F \\
G & H
\end{array}\right] ^{-1} \ne
\left[\begin{array}{rr}
E^{-1} & F ^{-1}\\
G^{-1} & H^{-1}
\end{array}\right]  
$$

Tersini alma işlemi için bazı numaralar lazım. Ana numara bölüntülenmiş matrisi 
köşegen bir matris haline getirmek, çünkü köşegen matrislerin tersi,
köşegendeki elemanların tersidir, yani ters alma operasyonu bu tür
matrislerin "içine işler'', o yüzden bir şekilde bir köşegen matris
elde etmeye uğraşacağız. Bunun için bölüntülenmiş matrisimizi sağdan ve
soldan bazı matrislerle çarpacağız. Ayrıca şunu da bilelim, 

$$ XYZ = W $$

durumunda $Y$'nin tersini almak istersek, sağ ve soldaki $X,Z$
matrislerinin tersini almak gerekmez, niye?

$$ X^{-1}XYZ = X^{-1}W $$

$$ YZZ^{-1} = X^{-1}WZ^{-1} $$

$$ Y = X^{-1}WZ^{-1} $$

Şimdi iki tarafın da tersini alalım, 

$$ Y^{-1} = ZW^{-1}X $$

Tamam, başlayalım. 

$$ M = 
\left[\begin{array}{rr}
E & F \\
G & H
\end{array}\right] 
$$

matrisini köşegen yapacağız. Eğer sadece alt sol köşeyi sıfırlayasaydık, 
bunu yapacak özel bir matrisle soldan çarpardık,

$$ 
\left[\begin{array}{rr}
I & -FH^{-1} \\
0 & I
\end{array}\right] 
\left[\begin{array}{rr}
E & F \\
G & H
\end{array}\right] = 
\left[\begin{array}{rr}
E & F \\
0 & H
\end{array}\right] 
$$

Sadece üst sağ köşeyi sıfırlamak isteseydik, sağdan çarpardık

$$ 
\left[\begin{array}{rr}
E & F \\
G & H
\end{array}\right] 
\left[\begin{array}{rr}
I & 0 \\
-H^{-1}G & I
\end{array}\right] 
=
\left[\begin{array}{rr}
E & 0 \\
G & H
\end{array}\right] 
$$

Hepsini biraraya koyalım, 

$$ 
\left[\begin{array}{rr}
I & -FH^{-1} \\
0 & I
\end{array}\right] 
\left[\begin{array}{rr}
E & F \\
G & H
\end{array}\right] 
\left[\begin{array}{rr}
I & 0 \\
-H^{-1}G & I
\end{array}\right] 
= 
\left[\begin{array}{rr}
E-FH^{-1}G & 0 \\
0 & H
\end{array}\right] 
\qquad (2)
$$

Bu çarpımın doğruluğu çarpım elle yapılarak kontrol edilebilir.

Üstte gördüğümüz gibi 

$$ XYZ = W $$

ifadesindeki $Y$'nin tersi 

$$ Y^{-1} = ZW^{-1}X $$

ile olur. 

$$ 
\underbrace{
\left[\begin{array}{rr}
I & -FH^{-1} \\
0 & I
\end{array}\right] 
}_{X}
\underbrace{
\left[\begin{array}{rr}
E & F \\
G & H
\end{array}\right] 
}_{Y}
\underbrace{
\left[\begin{array}{rr}
I & 0 \\
-H^{-1}G & I
\end{array}\right] 
}_{Z}
= 
\underbrace{
\left[\begin{array}{rr}
E-FH^{-1}G & 0 \\
0 & H
\end{array}\right] 
}_{W}
$$

O zaman 

$$ 
\left[\begin{array}{rr}
E & F \\
G & H
\end{array}\right]^{-1}
=
\left[\begin{array}{rr}
I & 0 \\
-H^{-1}G & I
\end{array}\right] 
\left[\begin{array}{rr}
E-FH^{-1}G & 0 \\
0 & H
\end{array}\right]^{-1}
\left[\begin{array}{rr}
I & -FH^{-1} \\
0 & I
\end{array}\right] 
$$

Daha kısa olması eşitliğin sağ tarafında, ortadaki matris için
$E-FH^{-1}G$ yerine $M/H$ kullanalım (bu arada $M/H$ lineer cebirde "$M$'in
$H$'e göre Schur tamamlayıcısı (complement)'' olarak bilinir),

$$ 
\left[\begin{array}{rr}
E & F \\
G & H
\end{array}\right]^{-1}
=
\left[\begin{array}{rr}
I & 0 \\
-H^{-1}G & I
\end{array}\right] 
\left[\begin{array}{rr}
(M/H)^{-1} & 0 \\
0 & H^{-1}
\end{array}\right]
\left[\begin{array}{rr}
I & -FH^{-1} \\
0 & I
\end{array}\right] 
\qquad (3)
$$

Eşitliğin sağ tarafındaki çarpımı gerçekleştirirsek, 

$$ =
\left[\begin{array}{rr}
(M/H)^{-1} & -(M/H)^{-1}FH^{-1} \\
-H^{-1}G(M/H)^{-1} & H^{-1}+H^{-1}G(M/H)^{-1}FH^{-1} 
\end{array}\right]
$$

Bu final ifade bölüntülenmiş bir matrisin tersini o matrisin içindeki parçalar
üzerinden temsil eden bir ifadedir. 

İçinde bir köşesi sıfır olan bölüntülenmiş matrislerde determinantlar şöyle
işler,

$$ 
\det \bigg(
\left[\begin{array}{rr}
E & 0 \\
G & H
\end{array}\right]
\bigg) 
= 
\det \bigg(
\left[\begin{array}{rr}
E & F \\
0 & H
\end{array}\right] 
\bigg) =
\det(E)\det(H)
$$

Ayrıca 

$$ \det(AB) = \det(A)\det(B) $$

O zaman (2)'nin determinantını alırsak, $\det$ yerine $||$ kullandık, 

$$ |M| = |M/H||H| 
\qquad (4)
$$

Bu ifade gayet doğal duruyor (bir raslantı herhalde, ya da Schur tamamlayıcısı 
işareti özellikle böyle seçilmiş),

Bölüntülenmiş bir matrisin devriğini almak için her bloğunun ayrı ayrı devriği
alınır, ve tüm blokların yanı bölüntülenmiş tamamının bir daha devriği alınır,
yani

$$ 
\left[\begin{array}{rr}
A & B \\ C & D 
\end{array}\right]^T = 
\left[\begin{array}{rr}
A^T & C^T \\ B^T & D^T
\end{array}\right]
$$

Şimdi çok değişkenli Normal için bileşen ve koşullu yoğunluk hesaplarına
gelelim. Gaussian formülünün $\exp$ kısmını alırsak, 

$$ \exp 
\bigg\{ 
-\frac{ 1}{2}
\left[\begin{array}{r}
x_1 - \mu_1\\
x_2 - \mu_2
\end{array}\right]^T
\left[\begin{array}{rr}
\Sigma_{11} & \Sigma_{12}\\
\Sigma_{21} & \Sigma_{22}
\end{array}\right]^{-1}
\left[\begin{array}{r}
x_1 - \mu_1\\
x_2 - \mu_2
\end{array}\right]
\bigg\}
$$

(3)'teki açılımı kullanırsak, ve $E = \Sigma_{11},F=\Sigma_{12},..$ olacak şekilde,

$$
\exp 
\bigg\{ 
-\frac{ 1}{2}
\left[\begin{array}{r}
x_1 - \mu_1\\
x_2 - \mu_2
\end{array}\right]^T
\left[\begin{array}{rr}
I & 0 \\ 
-\Sigma_{22}^{-1}\Sigma_{21} & I
\end{array}\right]
\left[\begin{array}{rr}
(\Sigma/\Sigma_{22}) & 0 \\ 
0 & \Sigma_{22}^{-1} 
\end{array}\right]
\left[\begin{array}{rr}
I & -\Sigma_{12}\Sigma_{22}^{-1}  \\ 
0 & I
\end{array}\right]
\left[\begin{array}{r}
x_1 - \mu_1\\
x_2 - \mu_2
\end{array}\right]
\bigg\}
$$

Açılımı tamamen yaparsak, 

$$ 
 \begin{array}{lll}
= && \exp \bigg\{
-\frac{1 }{2} 
(x_1 - \mu_1 - \Sigma_{12}\Sigma_{22}^{-1} (x_2 - \mu_2))^T 
(\Sigma/\Sigma_{22})^{-1} 
(x_1 - \mu_1 - \Sigma_{12}\Sigma_{22}^{-1} (x_2 - \mu_2))
\bigg\} \cdot \\
&& \exp \bigg\{
1\frac{ 1}{2}(x_2-\mu_2)^T\Sigma_{22}^{-1} (x_2-\mu_2)
 \bigg\}
\end{array}
$$

Not: $\Sigma_{12}^T = \Sigma_{21}$. Üstte birinci $\exp$ içinde sol bölümde
devriğin içindeki ifadelerden, mesela $x_1^T,\mu_1^T$'den ve $\Sigma_{21}$'li
ifadeden devrik işlemini çekip, büyük paranteze alınınca bu değişim oldu.

Şimdi mesela 1. $\exp$'ye dikkat edersek, ortada $(\Sigma/\Sigma_{22})^{-1}$
var, ve bu ifadenin solunda ve sağında birbirinin devriği olan aynı terimler
duruyor. İfadenin tamamı bir Normal dağılım. Aynı şey 2. $\exp$ için geçerli.

İşin $\exp$ tarafını halletik. Şimdi $\exp$ öncesindeki kesiri (4) kullanarak
parçalayalım, 

$$ 
\frac{ 1}{(2\pi)^{(p+q)/2} \det(\Sigma)^{1/2}}  = 
\frac{ 1}{(2\pi)^{(p+q)/2} \bigg(\det(\Sigma/\Sigma_{22})\det(\Sigma_{22})\bigg)^{1/2}} 
$$

$$ =
\bigg( \frac{ 1}{(2\pi)^{p/2} \det(\Sigma/\Sigma_{22})^{1/2}} \bigg)
\bigg( \frac{ 1}{(2\pi)^{q/2} \det(\Sigma_{22})^{1/2}} \bigg)
$$

Bu parçaların her birini ayrı bir $\exp$ önünde kullanabiliriz, ve ikinci $\exp$
ifadesinin 

$$ 
\frac{ 1}{(2\pi)^{q/2} \det(\Sigma_{22})^{1/2}}
\exp \bigg\{
\frac{ 1}{2}(x_2-\mu_2)^T\Sigma_{22}^{-1} (x_2-\mu_2)
 \bigg\}
$$


olduğunu görüyoruz. Bu ifade $f(x_2)$ bileşen yoğunluğudur! O zaman geri
kalanlar, yani diğer kesir ve birinci $\exp$ hep beraber $f(x_1|x_2)$
yoğunluğu olmalıdır. Yani,

$$ 
\frac{ 1}{(2\pi)^{p/2} \det(\Sigma/\Sigma_{22})^{1/2}} \cdot
$$

$$ 
\exp \bigg\{
-\frac{1 }{2} 
(x_1 - \mu_1 - \Sigma_{12}\Sigma_{22}^{-1} (x_2 - \mu_2))^T 
(\Sigma/\Sigma_{22})^{-1} 
(x_1 - \mu_1 - \Sigma_{12}\Sigma_{22}^{-1} (x_2 - \mu_2))
\bigg\}
$$

Buradan genel bir kural çıkartabiliriz, 

1) $X_2$'nin bileşen yoğunluğu $X_2 \sim N(\mu_2, \Sigma_{22})$

2) $X_2 = x_2$ olmak üzere $X_1$'in koşullu dağılımı 

$$
X_1 | X_2 = x_2 \sim 
N\bigg(\mu_1 + \Sigma_{12}\Sigma_{22}^{-1} (x_2 -\mu_2) \ , \
\Sigma/\Sigma_{22} \bigg)
$$

$\Sigma/\Sigma_{22}$ nedir? Hatırlarsak, $M/H = E-FH^{-1}G$, ve 
$E = \Sigma_{11},F=\Sigma_{12},..$ o zaman 

$$ \Sigma/\Sigma_{22} = \Sigma_{11}-\Sigma_{12} \Sigma_{22}^{-1} \Sigma_{21} $$

Yani

$$ X_1 | X_2 = x_2 \sim 
N\bigg(\mu_1 + \Sigma_{12}\Sigma_{22}^{-1} (x_2 -\mu_2) \ , \
\Sigma_{11}-\Sigma_{12} \Sigma_{22}^{-1} \Sigma_{21}
\bigg)
$$ 

log-toplam-exp (log-sum-exp trick)

Bu numaranın ilk kısmı nisbeten basit. Bazı yapay öğrenim algoritmaları için
olasılık değerlerinin birbiriyle çarpılması gerekiyor, mesela 

$$ r = p_1 \cdot p_2 \dots p_n $$

Olasılıklar 1'den küçük olduğu için 1'den küçük değerlerin çarpımı aşırı
küçülebilir, ve küçüklüğün taşması (underflow) ortaya çıkabilir. Eğer
çarpım yerine $\log$ alırsak, çarpımlar toplama dönüşür, sonra sonucu
$\exp$ ile tersine çeviririz, ve $\log$'u alınan değerler çok küçülmez,
çarpma yernie toplama işlemi kullanıldığı için de nihai değer de küçüklüğe
doğru taşmaz.

$$ \log r = \log p_1 + \log p_2 + \dots + \log p_n $$

$$ r = \exp(\log p_1 + \log p_2 + \dots + \log p_n )$$

Bir diğer durum içinde $exp$ ifadesi taşıyan bir olasılık değerinin çok
küçük değerler taşıyabilmesidir. Mesela çok değişkenli Gaussian karışımları
için alttaki gibi bir hesap sürekli yapılır, 

$$ = \sum_i w_i
\frac{ 1}{(2\pi)^{k/2} \det(\Sigma)^{1/2}} \exp 
\bigg\{ 
-\frac{ 1}{2}(x-\mu)^T\Sigma^{-1}(x-\mu)
\bigg\}
$$

ki $0 \le w_i \le 1$ şeklinde bir ağırlık değeridir. Üstteki formülün
çoğunlukla $\log$'u alınır, ve, mesela bir örnek üzerinde görürsek (ve
ağırlıkları bir kenara bırakırsak), 

$$ \log(e^{-1000} + e^{-1001}) $$ 

gibi hesaplar olabilir. Üstteki değerler tamamen uyduruk denemez,
uygulamalarda pek çok kez karşımıza çıkan değerler bunlar. Her neyse, eğer
üstteki ifadeyi kodla hesaplarsak, 

```python
print (np.log(np.exp(-1000) + np.exp(-1001)))
```

```
-inf
```

Bu durumdan kurtulmak için bir numara şudur; $\exp$ ifadeleri arasında en
büyük olanını dışarı çekeriz, ve $\log$'lar çarpımı toplam yapar,

$$ \log(e^{-1000}(e^{0} + e^{-1} ))$$

$$ -1000 + \log(1 + e^{-1})$$

Bunu hesaplarsak, 

```python
print (-1000 + np.log(1+np.exp(-1)))
```

```
-999.686738312
```

Bu numaranın yaptığı nedir? Maksimumu dışarı çekerek en az bir değerin
küçüklüğü taşmamasını garantilemiş oluyoruz. Ayrıca, bu şekilde, geri kalan
terimlerde de aşırı ufalanlar terimler kalma şansı azalıyor. 

Kaynaklar

[1] Flannery, *Numerical Recipes, 3rd Edition*

[2] Tapaswi, *Log-Sum-Exp Trick*, [http://makarandtapaswi.wordpress.com/2012/07/18/log-sum-exp-trick/](http://makarandtapaswi.wordpress.com/2012/07/18/log-sum-exp-trick/)



