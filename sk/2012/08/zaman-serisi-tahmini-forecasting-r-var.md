# Zaman Serisi Tahmini (Forecasting), R, VAR, rpy2

Zaman serisi modellemesinde VAR modelleri bugünlerde en popüler
olanları. VAR, vector autoregression kelimelerinden geliyor, yani tek
bir zaman serisi değil, birkaç tanesine birden, aynı anda modellemeye
uğraşıyoruz.

Autregression, zaman serisinin kendi kendisiyle regresyona
sokulmasıdır; bilindiği gibi regresyon y = f(x) modellemesinde
gürültülü elde edilen y ile, x değerleri arasındaki bağlantının
bulunmasına yardım eder (eğer f(x) lineer ise iyi sonuçlar da
bulur). Tek boyutlu zaman serisi modellemesi için şöyle bir numara
kullanılır, serinin kopyası alınır, bir geri kaydırılır, x bu
kaydılırılmış seri, y esas seri olur, bu ikili regresyona
sokulur. Böylece zaman serisinin kendisini ve regresyon mekanizması
kullanılarak zaman serisi tahmini yapılabilir.

VAR ise bunu çok boyutlu yapar. Her seriyi hem kendisi, hem de diğer
tüm serilerin p kadar geçmiş değeri göz önüne alınır. Oldukça güçlü
bir metottur.

Bu alanda ünlü isimlerden Sims'i bilmek gerekir, 1980 yılında yazdığı
ve kendi alanını eleştirdiği bir makalede makroekonomide yapısal
modeller yerine, çıplak veriye bakmak gerektiğini, ve bunu yapmak için
her zaman serilerine tek başlarına değil tüm diğer serilere de
bağlantılarını göz önüne alarak incelemek gerektiğini söyler. VAR
matematiği buradan çıkmıştır. Granger ismi de vardır, VAR modellemesi
sonrası serilerin "birbirine ne kadar etki ettiğini" hesaplayan
"Granger istatistiği" mesela ona aittir.

İşin matematiğine sonra daha detaylı girebiliriz, şimdilik kodlama
açısından örnekleri verelim. Bu alanda R kodcuları çok aktif, o yüzden
bir R paketi vars kullanacağız, ve onu Python üzerinden çağıracağız.

Diyelim ki bir predict-1.csv içinde bir ülkenin GDP ve tüketim
verileri (cons) var, 1959-2009 arası için (bu oldukca standart bir
veri seti). Önce R kurulur

```
sudo apt-get install r-base-dev r-base python-rpy2

Sonra R'ye girilir

> install.packages("vars")
```

Şimdi şu R kodu kullanılabilir

```
library("vars")file = "predict-1.csv"a <- read.csv(file, header = TRUE, sep = ",", na.strings="")impute.med <- function(x) {    z <- median(x, na.rm = TRUE)    x[is.na(x)] <- z    return(x)}a2 <- sapply(a, function(x){    if(is.numeric(x) & any(is.na(x))){            impute.med(x)        } else {            x        }    })out <- VAR(a2, p = 2, type = "const")out.prd <- predict(out, n.ahead = 30, ci = 0.95)
```

Bunu Python'dan çağırmak için rpy2 kullanırız,

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

Python işledikten sonra sonuç predict-2.csv içinde olacak. Sonuçlar
2009 sonrası 30 sene sonrası için gdp ve tüketim rakamlarını tahmin
edecek.

Eğer pür Python kullanmak isteseydik, scikits statsmodels adında bir
paketi de kullanabilirdik. Bu durumda hiç R kodlaması olmayacak,
kurmak için

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

Üstteki kod sonuçlar predict-3.csv içine yazar.

VAR ile zaman serisi tahminlerinde önemli bazı konular incelenen
verinin (zaman serisinin) durağan (stationary), ve beraber entegre
(co-integrated) olup olmadığıdır -- bu durumlarda bazı ek numaralar
kullanmak gerekebilir, mesela durağan bir veri seti yoksa serinin
farklarını kullanmak gibi..








