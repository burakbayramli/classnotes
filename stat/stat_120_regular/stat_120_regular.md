# Regresyon, Ridge, Lasso, Çapraz Sağlama, Regülarize Etmek

Konumuz regresyon çeşitleri, ve örnek veri olarak diyabet hastalığı
olan kişilerden alınmış bazı temel verilerle hastalığın bir sene
sonraki ilerleme miktarı kullanılacak. Regresyon sayesinde temel
veriler ile hastalığın ilerlemesi arasında bir bağlantı bulunabilir,
bu sayede hem veri açıklanır / daha iyi anlaşılır (hangi değişken
önemlidir, hangisi değildir), hem de başka bir hastanın temel
verilerini kullanarak o hastanın diyabetinin bir sene sonra ne
olacağını tahmin etmek mümkün olur. Kullanılan temel veriler kişinin
yaşı, cinsiyeti, vücut kütle endeksi (body mass index) ortalama
tansiyonu ve altı kere alınmış kan serum ölçümleridir.

```python
from pandas import *
diabetes = read_csv("diabetes.csv",sep=';')
diabetes_y = diabetes['response']
diabetes_x = diabetes.drop("response",axis=1)
diabetes_x_train = diabetes_x[:-20]
diabetes_x_test  = diabetes_x[-20:]
diabetes_y_train = diabetes_y[:-20]
diabetes_y_test  = diabetes_y[-20:]
```

İlk önce basit regresyonu hatırlayalım. Bu tekniği daha önce pek çok yönden
gördük. *Lineer Cebir*, *Çok Değişkenli Calculus* ders notlarında
bu tekniğin türetilmesi mevcut. Formül

$$ \hat{w} = (X^TX) ^{-1} X^{T}y $$

Sayısal olarak hemen bu hesabı yapabiliriz. Bir hatırlatma: veri setine $y$
ekseninin nerede kesildiğinin bulunabilmesi için suni bir ekstra kesi,
"intercept'' adlı kolon ekleyeceğiz, bu kolon iki boyutta $y=ax+c$
formülündeki $c$'nin bulunabilmesi içindir. Pandas ile bu ekstra kolonu
eklemek çok basit, ismen mevcut olmayan kolon erişildiği anda o kolon hemen
yoktan yaratılır.

```python
import numpy.linalg as la
x_tmp = diabetes_x_train.copy()
x_tmp['intercept'] = 1
xTx = np.dot(x_tmp.T,x_tmp )
ws = np.dot(la.inv(xTx),np.dot(x_tmp.T,diabetes_y_train))
print ws
```

```
[  3.03499452e-01  -2.37639315e+02   5.10530605e+02   3.27736981e+02
  -8.14131711e+02   4.92814589e+02   1.02848453e+02   1.84606489e+02
   7.43519617e+02   7.60951724e+01   1.52764307e+02]
```

Aynı hesabı bir de `scikit-learn` paketini kullanarak yapalım. Bu
paketin `LinearRegression` çağrısı kesi ekleme işini otomatik olarak
hallediyor, eğer kesi olmasın isteseydik, `fit_intercept=False`
diyecektik.

```python
from sklearn import linear_model, cross_validation
lin = linear_model.LinearRegression()
lin.fit(diabetes_x_train, diabetes_y_train)
print lin.coef_
print "score", lin.score(diabetes_x_test, diabetes_y_test), 
```

```
[  3.03499452e-01  -2.37639315e+02   5.10530605e+02   3.27736981e+02
  -8.14131711e+02   4.92814589e+02   1.02848453e+02   1.84606489e+02
   7.43519617e+02   7.60951724e+01]
score 0.585075302278
```

Sonuçlar birbirine oldukça yakın. Şimdi diğer tekniklere gelelim.

Sırt Regresyonu (Ridge Regression)

Klasik regresyon ile

$$ \hat{w} = \arg \min_w ||y-Xw||^2  $$

problemini çözdüğümüzü biliyoruz, ki $||\cdot||^2$ Öklit normunun karesini
temsil ediyor. Fakat bazı durumlarda $X^TX$'in eşsiz (singular) olması mümkün ki
böyle bir durumda $(X^TX)^{-1}$'in tersini almamız mümkün olmazdı. Eşsizlik ne
zaman ortaya çıkar? Eğer elimizde veri noktasından daha fazla boyut var ise
mesela... Diyelim ki veri olarak 10 tane kolon var, ama sadece 9 tane veri
satırı. Sırt Regregyonunun çıkış noktası budur.

Fakat ek olarak bu teknik kestirme hesaplarımıza (estimation) bir yanlılık
(bias) eklemek için de kullanılabilir ve bu meyil kestirme hesaplarının
iyileşmesine faydalı olabilir.

Meyili nasıl ekleriz? Diyelim ki bizim tanımlayacağımız bir $\lambda$ ile
tüm $ws$'lerin toplamına bir üst sınır tanımlayabiliriz. Böylelikle
regresyonun bulacağı katsayıların çok fazla büyümesine bir "ceza" getirmiş
olacağız, ve bu cezayı içeren regresyon hesabı o cezadan kaçınmak için
mecburen bulacağı katsayıları ufak tutacak, hatta bazılarını sıfıra indirebilecek.
Bu azaltmaya istatistikte küçülme (shrinkage) ismi veriliyor. 

Sırt regresyonu için bu küçültme şöyle

$$ \hat{w}_{sirt} = \arg \min_w ( ||y-Xw||^2 + \lambda||w||^2 )  $$

Görüldüğü üzere $w$'nin büyüklüğünü, bir $\lambda$ katsayısı üzerinden
minimizasyon problemine dahil ettik, böylece diğer parametreler ile büyüklük te
minimize edilecek. Üstteki tanım sınırı tanımlanmamış (unconstrained) bir
optimizasyon problemidir. Sınırlı olarak

$$ \min_w ||y-Xw||^2  $$
$$ ||w||^2 \le \tau \textrm{ koşuluna göre (subject to) }$$

ki $\lambda$ Lagrange çarpanıdır. Aslında şimdiye kadar üstteki
çevrimin tersini gördük çoğunlukla (yani sınırlı problemden sınırsıza
gitmeyi), bu gidiş tarzını görmek te iyi oldu.

Neyse baştaki sınırsız problemi çözmek için ifadenin gradyanını alalım,

$$ \nabla \big( ||y-Xw||^2 + \lambda||w||^2 \big) $$

$$ \nabla \big( (y-Xw)^T (y-Xw) + \lambda w^Tw \big) $$

$$ \nabla \big(  (y^T-w^TX^T)(y-Xw) + \lambda w^Tw  \big) $$

$$ \nabla ( y^Ty - y^TXw - w^TX^Ty + w^TX^TXw + \lambda w^Tw   )  $$

$$  - y^TX - X^Ty + 2X^TXw + 2\lambda w   $$

$$  - 2X^Ty + 2X^TXw + 2\lambda w   $$

$$   2X^TXw + 2\lambda w - 2X^Ty   $$

$$   2(X^TX + \lambda I ) w - 2X^Ty   $$

Minimizasyon için üstteki ifadeyi sıfıra eşitleyebiliriz

$$   2(X^TX + \lambda I ) w - 2X^Ty  = 0 $$

O zaman

$$   (X^TX + \lambda I ) w = X^Ty  $$

$$   \hat{w} = (X^TX + \lambda I)^{-1} X^Ty  $$

Bu son ifade en az kareler (least squares) yani normal regresyon çözüm
formülüne çok benziyor, sadece ek olarak bir $\lambda I$ toplama
işlemi var.  Demek ki sırt regresyonunu kullanmak için zaten
yaptığımız hesaba, zaten bizim kendimizin karar verdiği bir $\lambda$
üzerinden $\lambda I$ eklersek, geri kalan tüm işlemler aynı olacak. 

Kontrol edelim

```python
lam = 0.2
wridge = np.dot(la.inv(xTx+lam*np.eye(xTx.shape[0])),\
                np.dot(x_tmp.T,diabetes_y_train))
print wridge
```

```
[  16.70807829 -179.42288145  447.64999897  285.41866481  -51.7991733
  -75.09876191 -192.46341288  123.61066573  387.91385823  105.53294479
  152.7637018 ]
```

Şimdi `scikit-learn` ile aynı hesabı yapalım

```python
ridge = linear_model.Ridge(alpha=0.2)
ridge.fit(diabetes_x_train, diabetes_y_train) 
print ridge.score(diabetes_x_test, diabetes_y_test), ridge.coef_
```

```
0.553680030106 [  16.69330211 -179.414259    447.63706059  285.40960442  -51.79094255
  -75.08327488 -192.45037659  123.60400024  387.91106403  105.55514774]
```

Bir yöntem daha var, bu yönteme Lasso ismi veriliyor. Lasso'ya göre cezalandırma

$$ \sum_{k=1}^{n} w_k^2 \le \lambda $$

üzerinden olur. Bu yöntemin tüm detaylarına şimdilik
inmeyeceğiz.

Örnek olarak bir $\lambda$ ile onun bulduğu katsayılara bakalım.

```python
lasso = linear_model.Lasso(alpha=0.3)
lasso.fit(diabetes_x_train, diabetes_y_train)
print lasso.coef_
```

```
[   0.           -0.          497.3407568   199.17441037   -0.           -0.
 -118.89291549    0.          430.93795945    0.        ]
```

Lasso bazı katsayıları sıfıra indirdi! Bu katsayıların ağırlık verdiği
değişkenleri, eğer Lasso'ya inanırsak, modelden tamamen atmak mümkündür.

Bu arada Sırt ve Lasso yöntemlerinin metotlarına "regülarize etmek
(regularization)" ismi de veriliyor. 

k-Katlamalı Çapraz Sağlama (k-fold Cross-Validation)

Bir yapay öğrenim algoritmasını kullanmadan önce veriyi iki parçaya ayırmak
ise yarar; bu parçalar tipik olarak eğitim verisi (training set) diğeri ise
test verisi (validation set) olarak isimlendirilir. İsimlerden belli
olacağı üzere, algoritma eğitim seti üzerinde eğitilir; ve başarısı test
verisi üzerinden rapor edilir. Bir bakıma modelin oluşturulması bir set
üzerindendir, sonra "al şimdi hiç görmediğin bir veri seti, bakalım ne
yapacaksın" sorusunun cevabı, sağlaması bu şekilde yapılır.

Not: AIC istatistiği, standart şartlar altında, çapraz saglama ile
eşdeğerdedir, ki bu durumda iki farklı veri öbeğine gerek yok, eğitim
verisi yeterli.

k-Katlamalı Çapraz Sağlama bu iki parçalı eğitim / test kavramını bir adım
öteye taşır. Ufak bir k seçeriz, ki bu genellikle 5 ila 10 arasında bir
sayı olur, ve tüm verimizi rasgele bir şekilde ama k tane ve eşit
büyüklükte olacak şekilde parçalara ayırırız. Bu parçalara "katlar (folds)"
ismi verilir bazen (ki isim buradan geliyor). Sonra teker teker her parçayı
test verisi yaparız ve geri kalan tüm parçaları eğitim verisi olarak
kullanırız. Bu işlemi tüm parçalar için tekrarlarız.

Bu yaklaşım niye faydalıdır? Çünkü veriyi rasgele şekillerde bölüp, pek çok
yönden eğitim / test için kullanınca verinin herhangi bir şekilde bizi
yönlendirmesi / aldatması daha az mümkün hale gelir.

Ve işte bu özelliği, ek olarak, çapraz sağlamayı "model seçmek" için
vazgeçilmez bir araç haline getirir.

Model seçmek nedir? Model seçimi üstteki bağlamda optimal bir $\lambda$
bulmaktır mesela, yani her modeli temsil eden bir $\lambda$ var ise, en iyi
$\lambda$'yi bulmak, en iyi modeli bulmak anlamına geliyor, çapraz sağlama
bunu sağlıyor. Çapraz sağlama için `scikit-learn`'un sağladığı
fonksiyonlar vardır, önce katları tanımlarız, sonra bu değiştirilmiş
regresyon fonksiyonlarına katlama usulünü geçeriz.

```python
k_fold = cross_validation.KFold(n=420, n_folds=7)
```

Katları üstteki gibi tanımladık. 420 tane veri noktasını 7 kata bol dedik.
Şimdi bu katları kullanalım,

```python
ridge_cv = linear_model.RidgeCV(cv=k_fold)
ridge_cv.fit(np.array(diabetes_x), np.array(diabetes_y))
print ridge_cv.alpha_
```

```
0.1
```

Üstteki sonuç $\lambda = 0.1$'i gösteriyor. Bu $\lambda$ daha optimalmış
demek ki. Lasso için benzer şekilde

```python
lasso_cv = linear_model.LassoCV(cv=k_fold)
print lasso_cv.fit(diabetes_x, diabetes_y)
```

```
LassoCV(alphas=None, copy_X=True,
    cv=sklearn.cross_validation.KFold(n=420, n_folds=7), eps=0.001,
    fit_intercept=True, max_iter=1000, n_alphas=100, normalize=False,
    precompute=auto, tol=0.0001, verbose=False)
```

```python
print lasso_cv.alpha_
```

```
0.00283958719118
```

```python
print lasso_cv.score(diabetes_x_test, diabetes_y_test) 
```

```
0.597090337358
```

Şimdi veri setinin bir kısmı üzerinde teker teker hangi algoritmanın
daha başarılı olduğunu görelim. 

```python
def predict(row):
    j = row; i = row-1
    new_data = diabetes_x[i:j]
    print diabetes_y[i:j], "lasso",lasso_cv.predict(new_data), \
    	    "ridge",ridge_cv.predict(new_data), \
      	    "linear",lin.predict(new_data)	    

predict(-2) # sondan ikinci veri satiri
predict(-3)
predict(-4)
predict(-5)
predict(-8)
```

```
439    132
Name: response, dtype: int64 lasso [ 122.2361344] ridge [ 127.1821212] linear [ 123.56604986]
438    104
Name: response, dtype: int64 lasso [ 101.85154189] ridge [ 108.89678818] linear [ 102.5713971]
437    178
Name: response, dtype: int64 lasso [ 192.95670241] ridge [ 189.58095011] linear [ 194.03798086]
436    48
Name: response, dtype: int64 lasso [ 52.8903924] ridge [ 57.66611598] linear [ 52.5445869]
433    72
Name: response, dtype: int64 lasso [ 60.42852107] ridge [ 66.3661042] linear [ 61.19831285]
```

Üstteki sonuçlara göre gerçek değeri 132 olan 439. satırda lasso 122.2,
sırt (ridge) 127.1, basit regresyon ise 123.5 bulmuş. O veri noktası için
sırt yöntemi daha başarılı çıktı.

Sonuçlara bakınca bazen sırt, bazen normal regresyon başarılı çıkıyor.
Hangi yöntem kazanmış o zaman? Bir o, bir bu öndeyse, hangi yöntemi
kullanacağımızı nasıl bileceğiz?

Aslında her seferinde tek bir metotu kullanmak gerekmiyor. Bu metotları bir
takım (ensemble) halinde işletebiliriz. Her test noktasını, her seferinde
tüm metotlara sorarız, gelen sonuçların mesela.. ortalamasını alırız. Bu
şekilde tek başına işleyen tüm metotlardan tutarlı olarak her seferinde
daha iyi sonuca ulaşacak bir sonuç elde edebiliriz. Zaten Kaggle gibi
yarışmalarda çoğunlukla birinciliği kazanan metotlar bu türden takım
yöntemlerini kullanan metotlar, mesela Netflix yarışmasını kNN ve SVD
metotlarını takım halinde işleten bir grup kazandı.

Kaynaklar

[1] Figueiredo, *Lecture Notes on Linear Regression*, [www.lx.it.pt/~mtf/Figueiredo_Linear_Regression.pdf](www.lx.it.pt/~mtf/Figueiredo_Linear_Regression.pdf)

[3] Harrington, P., *Machine Learning in Action*

[4] Shalizi, *Data Analysis from an Elementary Point of View*


