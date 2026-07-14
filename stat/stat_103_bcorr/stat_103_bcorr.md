# Korelasyon, P-Değerleri, Bayes Yaklaşımı

Bir korelasyon katsayı hesabının p-değerini, yani onun istatistiki
olarak ne kadar anlamlı (significant) olduğunu hesaplamak mümkündür.

### Frekansçı Yaklaşım

Korelasyon $\rho$'yu daha önce gördük, tahmin edicisi $r$'dir, 

$$ 
\hat{\rho} = r = \frac{S_{xy}}{\sqrt{S_{xx}S_{yy}}} 
\tag{1} 
$$

ki örneklem hesapları $S_{xx},S_{xy},S_{yy}$ 

$$ S_{xx} = \sum_{i=1}^{n} (x_i-\bar{x})^2 $$

$$ S_{xy} = \sum_{i=1}^{n} (x_i-\bar{x})(y_i-\bar{y}) $$

$$ S_{yy} = \sum_{i=1}^{n} (y_i-\bar{y})^2 $$

olsun; bu hesapların teorik varyans ile olan bağlantısı görülebilir. Eğer
$X,Y$ iki değişkenli (bivariate) bir normal dağılımından geliyorsa, o zaman
ortada bir regresyon varmış gibi gösterebiliriz

$$ E(Y|X=x) = \beta_0 + \beta_1 x + \epsilon $$

ki $\beta_1 = \sigma_Y / \sigma_X \cdot \rho$ olur. Detaylar için
[1]. Soru şu, $r$ için bir istatistiksel önemlilik (significance)
hesabı nasıl yapardık? Yani, eğer $-1 \le r \le 1$ işe, ve $r=0$ hiç
korelasyon olmama durumu ise, acaba bu "sıfır olmama'' durumunu test
edebilir miydim? Evet. Yukarıdaki normallik faraziyesi doğru ise
$\beta_1 = 0$ olmama durumunu test etmek $\rho = 0$ olmama testi ile
aynı, bu durumda

$$ t_0 = \frac{\hat{\beta_1}}{\sqrt{ Var(\hat{\beta_1} ) }} $$

gibi bir test istatistiği yaratırız, ki bu istatistik Öğrenci t dağılımına
sahip olurdu çünkü sıfır hipotezi $\hat{\beta_1} = 0$, ve üstteki
istatistik sıfır hipotezi altında ile Öğrenci t dağılımına sahip olmak
zorundadır, çünkü bölünen normal dağılmış, bölen chi karenin karekökü
olarak dağılmış. Eğer $t_o$ hesabı veriye uygulandıktan sonra hipotezin
öngördüğü dağılıma uymaz ise, sıfır hipotezini reddederiz. 

Bu noktada lineer regresyon ile alakalı bilgiler devreye sokulabilir,
[1]'den biliyoruz ki,

$$ Var(\hat{\beta_1}) = \frac{\sigma^2}{S_{xx}} $$

$\sigma$ yerine örneklemden gelen $S$ kullanırsak ve üstteki formüle koyarsak,

$$ t_0 = \frac{\hat{\beta_1}}{\sqrt{S / S_{xx}}} $$

Bu ifadeyi $r$ bazında ifade edebilir miyiz?  Deneyelim, $\hat{\beta_1} =
S_{xy} / S_{xx}$ ve $r = \hat{\beta_1}\sqrt{\frac{S_{xx}}{S_{yy}}}$ olduğunu
biliyoruz [1], ayrıca

$$ S = \frac{SSE}{n-2}, \qquad SSE = S_{yy} - \hat{\beta_1} S_{xy} $$

ki $SSE$ hata karelerinin toplamıdır (sum of squared errors),

$$ t_0 = \frac{  \sqrt{S_{xx}} \hat{\beta_1} \sqrt{n-2} }{\sqrt{SSE}} $$

$$ = \frac{  \sqrt{S_{xx}} \hat{\beta_1} \sqrt{n-2} }{\sqrt{S_{yy} - \hat{\beta_1} S_{xy}}} $$

Bölümün iki kısmını $\sqrt{S_y}$ ile bölelim, 

$$ = 
\frac{ \sqrt{S_{xx}/S_{yy}} \hat{\beta_1} \sqrt{n-2} }
{\sqrt{1 - \hat{\beta_1} S_{xy}/S_{yy}}} 
$$

Bölünen kısmında bir $r$ ortaya çıktı,

$$ = 
\frac{ r\sqrt{n-2} }
{\sqrt{1 - \hat{\beta_1} S_{xy}/S_{yy}}} 
$$

Bölen kısmındaki $\hat{\beta_1}$ yerine $\hat{\beta_1} = S_{xy} / S_{xx}$
koyarsak yine (1)'deki $r$ tanımına geliriz, ve alttaki basitleştirilmiş
ifade ortaya çıkar,

$$ t_o = \sqrt{\frac{(n-2)r^2}{(1-r^2)}} $$

Bu istatistik $n-2$ derece serbestliğe sahip bir Öğrenci t dağılımıdır. 

Örnek

Possum adı verilen bir tür hayvanın dişilerinin tüm uzunluğu ve kafa ölçümü
`totlngth,hdlngth` değişkenleri arasında korelasyon olup olmadığı
merak edilmektedir.

```python
import pandas as pd
import scipy.stats

def p_corr(df1, df2):
    corr = df1.corr(df2)
    N = np.sum(df1.notnull())
    t = corr*np.sqrt((N-2)/(1-corr**2))
    p = 1-scipy.stats.t.cdf(abs(t),N-2)  # one-tailed
    return corr, t, p

df = pd.read_csv('fossum.csv')
c,tval, pval = p_corr(df.totlngth,df.hdlngth)
print (c, pval)
```

```
0.779239322172 3.75045772216e-10
```

p-değeri çok küçük, demek ki korelason olmadığı tezi reddedildi. Korelasyon
var.

### Bayes











[devam edecek]

Kaynaklar

[1] Wackerly, *Mathematical Statistics, 7th Edition*
