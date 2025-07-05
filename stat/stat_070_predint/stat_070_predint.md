# Tahmin Aralıkları (Prediction Interval)

Lineer Regresyon yazısında regresyon katsayıları $\beta$'yi veriden
hesaplamayı öğrendik. Bu bir anlamda alttaki denklemde verili $y,A$ ile
geri kalanları tahmin etmektir. 

$$ y = A\beta + \epsilon  $$

ki

$$ \epsilon \sim Normal(\mathbf{0}, \sigma^2 \mathbf{I}) $$

Yani katsayıların $A$ ile çarpımları artı gürültü ($\sigma$ ile parametrize
edilmiş bir Gaussian üzerinden) bu sonucu verecektir. Tahmin edici,

$$ \hat{\beta} = (A^TA)^{-1}A^Ty $$

olarak bilinir. Bu formülü pek çok yazıda gördük, mesela [3]. O zaman

$$ \hat{\beta} = (A^TA)^{-1}A^T(A\beta + \epsilon) $$

$$ 
\hat{\beta} = \beta  + (A^TA)^{-1}A^T \epsilon 
\qquad (1)
$$

Eğer $E( \hat{\beta} )$ hesaplamak istersek, 

$$ E( \hat{\beta} ) = E(  \beta  + (A^TA)^{-1}A^T \epsilon )$$

Fakat $E(\epsilon) = 0$ olduğu için üstteki hemen $E( \hat{\beta} ) = \beta$
haline geliyor. Vektör rasgele değişkenler üzerinde varyans, ya da kovaryans
hesabını daha önce görmüştük, bu hesabı $\hat{\beta}$ üzerinde uygularsak,

$$ Var(\hat{\beta}) = 
E \big[
(\hat{\beta} - E(\hat{\beta}))  
(\hat{\beta} - E(\hat{\beta}))^T 
\big]
$$

Biraz önce $E( \hat{\beta} ) = \beta$ demiştik, o zaman üstteki 

$$ 
Var(\hat{\beta}) = 
E \big[
(\hat{\beta} - \beta)  
(\hat{\beta} - \beta)^T 
\big]
\qquad (2)
$$

olur. Üstte $\hat{\beta} - \beta$ var, bu (1)'den $\beta$ çıkartılıyor
anlamına gelir, o zaman oradaki $\beta$ kaybolur, geriye 

$$ \hat{\beta} - \beta = \beta  + (A^TA)^{-1}A^T \epsilon - \beta 
$$

$$ = (A^TA)^{-1}A^T \epsilon   $$

Üstteki ifadeyi (2) içine koyalım, 

$$ E \bigg[
\big( (A^TA)^{-1}A^T \epsilon \big)
\big( (A^TA)^{-1}A^T \epsilon \big)^T
\bigg]
$$

Beklenti içini açalım, 

$$  = E [(A^TA)^{-1}A^T \epsilon \epsilon^T A (A^TA)^{-1}] $$

Tersi işleminin devriği kayboldu çünkü $A^TA$ simetriktir, onun tersi de
simetriktir, simetrik matrisin devriği yine kendisidir.

$$  = (A^TA)^{-1}A^TA E[\epsilon \epsilon^T]  (A^TA)^{-1} $$

$$  = E[\epsilon \epsilon^T]  (A^TA)^{-1} $$

$$ 
Var(\hat{\beta})  = \sigma^2  (A^TA)^{-1} 
\qquad (3)
$$

Yeni bir tahmin $a$ için 

$$ \hat{y}_a = a^T \hat{\beta}  $$

$\beta$ yerine $\hat{\beta}$ kullandık. Şimdi tüm ifadenin varyansına
bakalım,

$$ Var(\hat{y}_a) = Var(a^T \hat{\beta}) $$

Bundan önce $Var(a^T \hat{\beta}) = \big[a^T(A^TA)^{-1}a \big] \sigma^2$ olduğunu ispatlamak lazım, 
[1, sf 617] olduğu gibi - öncelikle $Var(a^T \hat{\beta})$ formülünde $a$ ve $\hat{\beta}$ nin birer
vektör olduğunu hatırlayalım, o zaman $a^T \hat{\beta}$ bir noktasal
çarpımdır, yani $a_1\hat{\beta}_1 + ... + a_n\hat{\beta}_n$.  Demek ki

$$ Var(a^T \hat{\beta}) =  Var(a_1\hat{\beta}_1 + ... + a_n\hat{\beta}_n)$$

Şimdi [4] bölümünden hatırlayacağımız üzere,

$$ Var(X_1+ .. + X_n ) = Var(X_1) + .. + Var(X_n) + 2 \sum_{i<j}^{} Cov(X_i,X_j) $$

Bizim elimizde $a_i\hat{\beta}_i$'lar var tabii, o zaman

$$ Var(a^T \hat{\beta})  = Var(a_1\hat{\beta}_1) + .. + Var(a_n\hat{\beta}_n) + 
2 \sum_{i<j}^{}  Cov(a_i \hat{\beta}_i,a_j \hat{\beta}_j) 
$$

$$ Var(a_i\hat{\beta}_i) = a_i^2Var(\hat{\beta}_i)$$ 

olduğunu hatırlayalım, o zaman iki üstteki

$$ =  a_1^2Var(\hat{\beta}_1) + .. +  a_n^2Var(\hat{\beta}_n) + 
2 \sum_{i<j} Cov(a_i \hat{\beta}_i,a_j \hat{\beta}_j) 
$$

Peki $Var(\hat{\beta}_i)$ nedir? (3)'u hatırlayalım, buradaki matris
çarpımından hareketle, her $Var(\hat{\beta}_i) = c_{ii} \sigma^2$
diyebiliriz ki $c_{ii}$, $(A^TA)^{-1}$ matrisinin (köşegeninde bulunan) bir
öğesidir.


$$ 
=  a_1^2c_{11} \sigma^2 + .. +  a_n^2c_{nn} \sigma^2 + 
2 \sum_{i<j} Cov(a_i \hat{\beta}_i,a_j \hat{\beta}_j) 
$$

Aynı şekilde $Cov(a_i\hat{\beta}_i,a_j \hat{\beta}_j) = 2a_ia_jc_{ij}\sigma^2$
diyebiliriz,

$$ =  a_1^2c_{11} \sigma^2 + .. +  a_n^2 c_{nn} \sigma^2 + 2 \sum_{i<j}
a_ia_jc_{ij}\sigma^2 $$

$$ =  \big[a_1^2c_{11}  + .. +  a_n^2 c_{nn}  + 2 \sum_{i<j}  a_ia_jc_{ij}\big]\sigma^2 $$

Üstteki ifadeyi rahat bir şekilde $\big[a^T(A^TA)^{-1}a \big] \sigma^2$ olarak yazabiliriz.

Şimdi güven aralığı yaratmanın zamanı geldi. Hatırlayalım ki
$\hat{\beta}_1,\hat{\beta}_2,,.$ tahmin edicilerinin kendileri birer
rasgele değişkendir, ve bu değişkenler Normal dağılıma sahiptirler. O zaman
$a^T\hat{\beta}$ da normal olarak dağılmıştır ve bu dağılımın beklentisinin
$E(a^T\hat{\beta}) = a^T\beta$ olduğunu biliyoruz (dikkat eşitliğin sağında
şapkasız $\beta$ var). O zaman "gerçek'' $\beta$ için bir güvenlik aralığı
oluşturmak için $a^T\hat{\beta} - a^T\beta$'nin da Normal olarak
dağılmasının zorunlu olduğundan hareketle,

$$ Z = 
\frac{a^T\hat{\beta} -a^T \beta }
{ \sqrt{ Var(a^T \hat{\beta}) } } = 
\frac{a^T\hat{\beta} -a^T \beta }
{ \sigma \sqrt{ a^T(A^TA)^{-1}a  } }
$$

Böylece bir standart normal yarattık, ve bu formülü daha önce güvenlik
aralığı için yaptığımız gibi düzenlersek, 

$$ a^T\hat{\beta} \pm z_{\alpha/2} \sigma \sqrt{ a^T(A^TA)^{-1}a  } $$

Daha önce gördüğümüz gibi $\sigma$ yerine $S$ koyabiliriz, o zaman Öğrenci
T dağılımı elde ederiz (yazının sonunda $\sigma,S$ teorik bağlantısının
sebepleri bulunabilir),


$$ T = 
\frac{a^T\hat{\beta} -a^T \beta }
{ S \sqrt{ a^T(A^TA)^{-1}a  } }
$$

ki bu güven aralığı

$$ a^T\hat{\beta} \pm t_{\alpha/2} S \sqrt{ a^T(A^TA)^{-1}a  } $$

olarak hesaplanabilecektir, T dağılımının serbestlik derecesi
$n-(k+1)$'dir, ki $n$ eldeki veri nokta sayısı, $k$ işe kaç $\beta$
değişkeninin olduğudur. 

Örnek

Basit bir örnek üzerinde görelim ([1, sf 620]'den alındı), 

```python
import pandas as pd
import numpy as np
df = pd.read_csv('11.1.csv',sep=' ')
print (df.head())
import statsmodels.formula.api as smf
results = smf.ols('y ~ x', data=df).fit()
mse = np.sum(results.resid**2) / (len(df)-2)
s = np.sqrt(mse)
print ('mse', mse, 's', s)
print (results.summary())
```

```
   x  y
0 -2  0
1 -1  0
2  0  1
3  1  1
4  2  3
mse 0.366666666667 s 0.605530070819
                            OLS Regression Results                            
==============================================================================
Dep. Variable:                      y   R-squared:                       0.817
Model:                            OLS   Adj. R-squared:                  0.756
Method:                 Least Squares   F-statistic:                     13.36
Date:                Mon, 11 May 2015   Prob (F-statistic):             0.0354
Time:                        17:26:07   Log-Likelihood:                -3.3094
No. Observations:                   5   AIC:                             10.62
Df Residuals:                       3   BIC:                             9.838
Df Model:                           1                                         
Covariance Type:            nonrobust                                         
==============================================================================
                 coef    std err          t      P>|t|      [95.0% Conf. Int.]
------------------------------------------------------------------------------
Intercept      1.0000      0.271      3.693      0.034         0.138     1.862
x              0.7000      0.191      3.656      0.035         0.091     1.309
==============================================================================
Omnibus:                          nan   Durbin-Watson:                   2.509
Prob(Omnibus):                    nan   Jarque-Bera (JB):                0.396
Skew:                          -0.174   Prob(JB):                        0.821
Kurtosis:                       1.667   Cond. No.                         1.41
==============================================================================

```

```python
import numpy.linalg as lin
A = df[['x']]
A['intercept'] = 1.
A = A[['intercept','x']]
ATA_inv = lin.inv(np.dot(A.T,A))
print (ATA_inv)
beta_hat = np.array(results.params)
a = np.array([[1,1]]).T
```

```
[[ 0.2  0. ]
 [ 0.   0.1]]
```

```python
pm = np.dot(np.dot(a.T, ATA_inv),a)[0][0]
pred = np.dot(a.T,beta_hat)[0]
print (pm, pred)
```

```
0.3 1.7
```

```python
from scipy.stats.distributions import t
t95_val = t.ppf(0.95,len(df)-2)
print ('tval', t95_val)
print (t95_val*s*pm)
print ('Yuzde 90 guven araligi', \
      (pred - np.array([1,-1])*t95_val*s*np.sqrt(pm)))
```

```
tval 2.3533634348
0.427509698202
Yuzde 90 guven araligi [ 0.91947765  2.48052235]
```

Görüldüğü gibi [1, sf 620] ile aynı sonucu aldık.

Başkanlık Yarışı Tahminleri

Daha önce [5] yazısında gördüğümüz 2016 başkanlık yarışı tahminini şimdi bu
yeni yöntemimizi kullanarak yapalım.

```python
import statsmodels.formula.api as smf
import pandas as pd
df1 = pd.read_csv('../stat_055_linreg/prez.csv',sep=',')
regr = 'incumbent_vote ~ gdp_growth + net_approval + two_terms'
results1 = smf.ols(regr, data=df1).fit()
A1 = df1.copy()
A1['intercept'] = 1.
A1 = A1[['intercept','gdp_growth','net_approval','two_terms']]
```

```python
import numpy.linalg as lin
from scipy.stats.distributions import t
t975_val1 = t.ppf(0.975,len(df1)-2)
beta_hat1 = np.array(results1.params)
ATA_inv1 = lin.inv(np.dot(A1.T,A1))
a1 = np.array([[1., 2.0, 0., 1]]).T
pm1 = np.dot(np.dot(a1.T, ATA_inv1),a1)[0][0]
pred1 = np.dot(a1.T,beta_hat1)[0]
mse1 = np.sum(results1.resid**2) / (len(df1)-2)
s1 = np.sqrt(mse1)
print ('Yuzde 95 Guven Araligi', \
      (pred1 - np.array([1,-1])*t975_val1*s1*np.sqrt(pm1)))
```

```
Yuzde 95 Guven Araligi [ 46.95198025  49.64353527]
```

Yani Demokratların kazanma şansı neredeyse hiç yok gibi. Önceki başkanlık
yarışı tahmini *katsayıların* güven aralıklarını kullanmıştı; şimdi
nihai tahminin güven aralığına baktık. Aradaki fark şudur - katsayıların
güven aralıklarını kullandığımızda onları en kötüleri birarada ve en
iyileri birada olacak şekilde yanyana kullanmış olduk; bu tür bir kullanım
bu katsayıların arasındaki korelasyonu dikkate almaz, çünkü, belki bir
katsayı X'in en kötümser olduğu noktada katsayı Y daha iyimser bir tahminde
bulunacaktır, çünkü aradaki bağlantı böyledir...? Bu durumlar ilk
kullanımda yakalanamazdı.  Bu sebeple ilk yöntemle hesaplanan güven aralığı
ikincisine nazaran daha geniş olacaktı, ki bunun olduğunu gördük.

$\sigma,\hat{\sigma},S$ İlişkileri

Öncelikle mümkün bazı notasyonel karışıklığı düzeltmeye uğraşalım;
kitaplarda $\sigma,\hat{\sigma}$ kullanımı tek boyutlu verinin nüfus
standart hatası ve onun tahmin edicisi (estimatör) için de kullanılıyor. Bu
yazıda bu farklı, bu yazıdaki $\sigma$ bir lineer modelin hatasını temsil
eden $\sigma$. 

Bu tür bir $\sigma$'nin tahmin edicisi $\hat{\sigma}$ şu şekilde tanımlı, 

$$ \hat{\sigma}^2 = \frac{1}{n} \sum_{i=1}^{n} (Y_i - \hat{Y}_i)^2 $$

İspat için [2, sf 557-558]. Fakat üstteki kodda $n-2$ kullanımı görüyoruz,
bu nereden geliyor? Bunun için $n/(n-2) \hat{\sigma}^2$ formülünün
$\sigma^2$ için bir yansız tahmin edici (unbiaşed estimatör) olduğunu
bilmemiz lazım. İspat için bakınız [2, sf 560]. Yansızlık tanımı için {\em
  Örneklem Dağılımları} yazısı.

Tüm bunları biraraya koyarsak, $Y_i - \hat{Y}_i$ regresyondan bize
döndürülen `resid` dizini, ve bu "artıkların'' karelerini alıp
toplayınca (ki artıklar tahmin ile gerçek verinin arasındaki fark), ve
onları $n-2$ ile bölünce $\sigma^2$ için bir yansız tahmin edici $S^2$'yi
nasıl elde ettiğimizi görebiliriz herhalde.

Ek

$\hat{\beta}$ Dağılımı

Lineer regresyonu $Y=X\beta + \epsilon$ olarak modellediğini farzedelim, ki
$X,Y,\beta$ çok boyutlu değişkenler / matris / vektör ve $\epsilon \sim
N(0,\sigma I)$ yani cok boyutlu bir Gaussian.  Soru su: Acaba $\beta$'nın
tahmin edicisi $\hat{\beta}$'nin dağılımı nedir?

Tahmin edici hesabi

$$ \hat{\beta} = (X^TX)^{-1}X^TY $$

olduğunu biliyoruz. $Y$'yi yerine koyarsak,

$$  = (X^TX)^{-1}X^T(X\beta + \epsilon) $$

$$  = \cancel{(X^TX)^{-1}X^TX}\beta + (X^TX)^{-1}X^T\epsilon $$

$$  = \beta + (X^TX)^{-1}X^T\epsilon $$

Bir yan not, biliyoruz ki çok boyutlu Gaussian mesela $G \sim N(\phi,\rho)$'a
$BG + A$ şekilde ilgin (affine) transform uygulayınca sonuç 
$N(\phi+A, B\rho B^T)$ oluyor. Burada $\epsilon$ bir çok boyutlu
Gaussian. O zaman üstteki transformu hesaplayabiliriz. $\beta$ toplamı
basit, esas iki taraftan $(X^TX)^{-1}X^T$ ve onun devriği
ile çarpılan standart sapmaya ne olacak ona bakalım,

$$ (X^TX)^{-1}X^T \sigma X(X^{-1}X^{-T})^{T}  $$

$$ = (X^TX)^{-1}X^T \sigma X X^{-1}X^{-T}  $$

$$ = \sigma (X^TX)^{-1}  $$

Sonra $\beta$ toplamını hatırlarız, yani $\hat{\beta} \sim N(\beta, \sigma
(X^TX)^{-1} )$ olarak dağılmıştır, demek ki katsayılarımızın regresyon 
tahmini "gerçek'' katsayılar etrafında  merkezlenen bir Gaussian'dır.

Kaynaklar

[1] Wackerly, *Mathematical Statistics*, 7th Edition

[2] Larsen, *Introduction to Mathematical Statistics and Its Applications*, 5th Edition

[3] Bayramlı, Lineer Cebir, *Ders 15,16*

[4] Bayramlı, Istatistik, *Beklenti, Kovaryans ve Korelasyon* 

[5] Bayramlı, Istatistik, *Lineer Regresyon*






