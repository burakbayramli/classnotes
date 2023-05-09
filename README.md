
ODE, Çok Değişkenli Calculus, Lineer Cebir, Hesapsal Bilim,
İstatistik, Fonksiyonel Analiz video derslerinden, ya da ders
kitaplarından alınan notların Latex ile yazılmış ve PDF olarak
üretilmiş dosyaları burada bulunabilir. Örnek Python kodları gerektiği
yerde yazı içinde ya da onunla beraber aynı dizinde olacaktır.

Bu notlarda yazılanları takip etmek için önkoşul bilgiler şunlar:

* Trigonometri
* Tek Değişkenli Calculus
* Modülo Matematiği
* Cebir

Yani üniversite sınavına hazırlık için gereken konular (gerçi artık
Calculus gerekiyor mu bilmiyorum, benim zamanımda gerekiyordu).

### Web, PDF

Lineer Cebir (Linear Algebra)

https://burakbayramli.github.io/dersblog/linear

Diferansiyel Denklemler (Ordinary Differential Equations)

https://burakbayramli.github.io/dersblog/ode

Çok Değişkenli Calculus (Multivariable Calculus)

https://burakbayramli.github.io/dersblog/calc_multi

Hesapsal Bilim (Computational Science)

https://burakbayramli.github.io/dersblog/compscieng

İstatistik, Yapay Öğrenim, Veri Analizi (Statistics, Machine Learning, Data Analysis)

https://burakbayramli.github.io/dersblog/stat

Zaman Serileri ve Finans (Time Series and Finance)

https://burakbayramli.github.io/dersblog/tser

Fonksiyonel Analiz (Functional Analysis)

https://burakbayramli.github.io/dersblog/func_analysis

Bilgisayar Bilim, Yapay Zeka (Computer Science, AI)

https://burakbayramli.github.io/dersblog/algs/index.html

Gayri Lineer Dinamik ve Kaos (Non-Linear Dynamics and Chaos)

https://burakbayramli.github.io/dersblog/chaos

Yapay Görüş (Computer Vision)

https://burakbayramli.github.io/dersblog/vision

Fizik

https://burakbayramli.github.io/dersblog/phy/index.html

IT, Bilisim

https://burakbayramli.github.io/dersblog/sk/index.html

### Kodlar, TeX

[Linear Algebra](linear)

[Diferansiyel Denklemler](ode)

[Çok Değişkenli Calculus](calc_multi)

[Hesapsal Bilim](compscieng)

[İstatistik, Yapay Öğrenim, Veri Analizi](stat)

[Zaman Serileri ve Finans](tser)

[Kısmi Diferansiyel Denklemler](pde)

[Fonksiyonel Analiz](func_analysis)

[Yapay Zeka, Çetrefillik](app_math)

[Gayri Lineer Dinamik ve Kaos](chaos)

[Çoklu Bakış Açı Geometrisi](vision)

[Fizik](phy)


İçeriğe ek yapmak isteyenler [için](CONTRIBUTING.md).

Dokümanların içinde görülen kod Jupyter not defteri ya da ipython
ortamı içinden işletilebilir, kurmak icin `pip install jupyter` ve
`pip install ipython`.

Diger temel kurulus bilgileri

https://burakbayramli.github.io/dersblog/sk/2016/01/python-dil-ogrenimi.html

Belgelerde görülen her bölümdeki kodlar kendi başına (bölümün dizini
içinden) işleyebilecek şekilde ayarlanmıştır. Bu kodlar notların
Github tex dosyalarından kopyalayarak alınabilir, ya da ayrı ayrı elle
girilir. Eğer kodlar not defteri dışında, dosya bazlı, pür Python
olarak işletilmek istenirse,

```
import numpy as np
import matploblib.pylab as plt
```

ibarelerini her script'in başına eklemek lazım. Bu durumda kodlar
`dosya.py` gibi bir dosya içinde kaydedilir, ve `python dosya.py` ile
komut satırından işletilir.

Gereken dış paketler [requirements.txt](requirements.txt) içinde.

Python paket kurulumu Ubuntu Linux üzerinde `pip install [paket ismi]`
ile yapılabiliyor.

Eğer kod içinde `import` edilen bir paket / modül ustteki listeden
değil ise, ve bu dahil (import) edilen kod parçası doküman içinde
gösterilmiyor ise, o zaman bu kod çoğunlukla aynı ya da paralel bir
dizinde `.py` dosyası olarak eklenmiştir (çünkü `import func` çağrısı
`func.py` adlı bir dosyayı dahil eder). Bu dosyayı bulmak için bu
Github projesinin alt dizinlerine bakmak yeterli. Mesela *Zaman
Serileri, Koentegrasyon* yazısında `pyconometrics` adlı bir modülün
import edildiğini görüyoruz. Bu dosya üstteki listedeki paket
listesinden gelmiyor. Projenin alt dizinlerine bakıyoruz, `tser/tser_coint`
altında `pyconometrics.py` adlı bir dosyayı görüyoruz. Gerekli kod burada.

[Ek Konular](sk/2020/08/classnotes-add.md)

## English

Here are lecture notes on ODE, Multivariate Calculus, Linear Algebra,
Computational Science, Computer Science, Statistics, Functional
Analysis written in Latex, in Turkish. All necessary Python code and
data is either in the document itself or included in the same
directory as the article, residing in this GH repository.

Anaconda is the suggested Python installation, and for the necessary
side packages. Use `conda install` to do this, or `pip install` on the
list above.

R

The R installation command for Ubuntu, as well as the R package
installation command is shown above in the TR section.

Blog

https://burakbayramli.github.io/dersblog

## LICENSE

The code is licensed under GPL v3. See COPYING for details.






