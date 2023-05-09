# Zaman Serisi Tahmini (Forecasting), R, VAR, rpy2

Zaman serisi modellemesinde VAR modelleri bugunlerde en populer
olanlari. VAR, vector autoregression kelimelerinden geliyor, yani tek
bir zaman serisi degil, birkac tanesine birden, ayni anda modellemeye
ugrasiyoruz.

Autregression, zaman serisinin kendi kendisiyle regresyona
sokulmasidir; bilindigi gibi regresyon y = f(x) modellemesinde
gurultulu elde edilen y ile, x degerleri arasindaki baglantinin
bulunmasina yardim eder (eger f(x) lineer ise iyi sonuclar da
bulur). Tek boyutlu zaman serisi modellemesi icin soyle bir numara
kullanilir, serinin kopyasi alinir, bir geri kaydirilir, x bu
kaydilirilmis seri, y esas seri olur, bu ikili regresyona
sokulur. Boylece zaman serisinin kendisini ve regresyon mekanizmasi
kullanilarak zaman serisi tahmini yapilabilir.

VAR ise bunu cok boyutlu yapar. Her seriyi hem kendisi, hem de diger
tum serilerin p kadar gecmis degeri goz onune alinir. Oldukca guclu
bir metottur.

Bu alanda unlu isimlerden Sims'i bilmek gerekir, 1980 yilinda yazdigi
ve kendi alanini elestirdigi bir makalede makroekonomide yapisal
modeller yerine, ciplak veriye bakmak gerektigini, ve bunu yapmak icin
her zaman serilerine tek baslarina degil tum diger serilere de
baglantilarini goz onune alarak incelemek gerektigini soyler. VAR
matematigi buradan cikmistir. Granger ismi de vardir, VAR modellemesi
sonrasi serilerin "birbirine ne kadar etki ettigini" hesaplayan
"Granger istatistigi" mesela ona aittir.

Isin matematigine sonra daha detayli girebiliriz, simdilik kodlama
acisindan ornekleri verelim. Bu alanda R kodculari cok aktif, o yuzden
bir R paketi vars kullanacagiz, ve onu Python uzerinden cagiracagiz.

Diyelim ki bir predict-1.csv icinde bir ulkenin GDP ve tuketim
verileri (cons) var, 1959-2009 arasi icin (bu oldukca standart bir
veri seti). Once R kurulur

```
sudo apt-get install r-base-dev r-base python-rpy2

Sonra R'ye girilir

> install.packages("vars")
```

Simdi su R kodu kullanilabilir

```
library("vars")file = "predict-1.csv"a <- read.csv(file, header = TRUE, sep = ",", na.strings="")impute.med <- function(x) {    z <- median(x, na.rm = TRUE)    x[is.na(x)] <- z    return(x)}a2 <- sapply(a, function(x){    if(is.numeric(x) & any(is.na(x))){            impute.med(x)        } else {            x        }    })out <- VAR(a2, p = 2, type = "const")out.prd <- predict(out, n.ahead = 30, ci = 0.95)
```

Bunu Python'dan cagirmak icin rpy2 kullaniriz,

```
import os, sys
import numpy as np
import rpy2.robjects
from datetime import date, timedelta
f = file("predict.R")
code = ''.join(f.readlines())
result = rpy2.robjects.r(code)
res = [['gdp','cons']]
for i in range(30):
    res.append([str(result[0][0][i]),str(result[0][1][i]) ] )
res = np.array(res)
np.savetxt('predict-2.csv',res,delimiter=",",fmt='%s')
```

Python isledikten sonra sonuc predict-2.csv icinde olacak. Sonuclar
2009 sonrasi 30 sene sonrasi icin gdp ve tuketim rakamlarini tahmin
edecek.

Eger pur Python kullanmak isteseydik, scikits statsmodels adinda bir paketi de kullanabilirdik. Bu durumda hic R kodlamasi olmayacak, kurmak icin

https://github.com/statsmodels/statsmodels

Bu kod

```
import os
import numpy as np
import statsmodels.api as sm
from statsmodels.tsa.api
import VAR
  def pad(data):
    bad_indexes = np.isnan(data)
    good_indexes = np.logical_not(bad_indexes)
    good_data = data[good_indexes]
    interpolated = np.interp(bad_indexes.nonzero()[0], good_indexes.nonzero()[0], good_data)
    data[bad_indexes] = interpolated
    return datadata = np.genfromtxt("predict-1.csv", skip_header=1, delimiter=',')
data = np.apply_along_axis(pad, 0, data)
model = VAR(data)
res = model.fit(2)
f = res.forecast(data[-2:], 30)
np.savetxt('predict-3.csv',f,delimiter=",",fmt='%s')
```

Ustteki kod sonuclar predict-3.csv icine yazar.

VAR ile zaman serisi tahminlerinde onemli bazi konular incelenen
verinin (zaman serisinin) duragan (stationary), ve beraber entegre
(co-integrated) olup olmadigidir -- bu durumlarda bazi ek numaralar
kullanmak gerekebilir, mesela duragan bir veri seti yoksa serinin
farklarini kullanmak gibi..








