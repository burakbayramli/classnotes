
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

[Elektrik ve Manyetik Etkileşimler](elecmag)

[Çoklu Bakış Açı Geometrisi](vision)

### PDF

Lineer Cebir (Linear Algebra)

http://sayilarvekuramlar.blogspot.com/2015/12/lineer-cebir-linear-algebra.html

Diferansiyel Denklemler (Ordinary Differential Equations)

http://sayilarvekuramlar.blogspot.com/2015/12/diferansiyel-denklemler.html

Çok Değişkenli Calculus (Multivariable Calculus)

http://sayilarvekuramlar.blogspot.com/2015/12/cok-degiskenli-calculus-multivariable.html

Hesapsal Bilim (Computational Science)

http://sayilarvekuramlar.blogspot.co.uk/2015/12/hesapsal-bilim-computational-science.html

İstatistik, Yapay Öğrenim, Veri Analizi (Statistics, Machine Learning, Data Analysis)

http://sayilarvekuramlar.blogspot.co.uk/2015/12/istatistik-ve-veri-analizi.html

Zaman Serileri ve Finans (Time Series and Finance)

http://sayilarvekuramlar.blogspot.com/2015/12/zaman-serileri-ve-finans.html

Kısmi Diferansiyel Denklemler (Partial Differential Equations)

http://sayilarvekuramlar.blogspot.com/2015/12/kismi-diferansiyel-denklemler-partial.html

Fonksiyonel Analiz (Functional Analysis)

http://sayilarvekuramlar.blogspot.com/2015/12/fonsiyonel-analiz-functional-analysis.html

Yapay Zeka, Çetrefillik (AI, Computational Complexity)

http://sayilarvekuramlar.blogspot.com/2015/12/bilgisayar-bilim-yapay-zeka.html

Gayri Lineer Dinamik ve Kaos (Non-Linear Dynamics and Chaos)

http://sayilarvekuramlar.blogspot.com/2015/12/gayr-lineer-dinamik-ve-kaos-chaos-non.html

Elektrik ve Manyetik Etkileşimler (Electric and Magnetic Interactions)

http://sayilarvekuramlar.blogspot.com/2015/01/elektrik-ve-manyetik-etkilesimler.html

Çoklu Bakış Açı Geometrisi (Multiple View Geometry)

http://sayilarvekuramlar.blogspot.com/2015/12/coklu-baks-ac-geometrisi-multiple-view.html

İçeriğe ek yapmak isteyenler [için](CONTRIBUTING.md).

Dokümanların içinde görülen kod Jupyter not defteri ya da ipython
ortamı içinden işletilebilir, kurmak icin `pip install jupyter` ve
`pip install ipython`.

Diger temel kurulus bilgileri

http://sayilarvekuramlar.blogspot.com/2016/01/python-dil-ogrenimi.html

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

```
matplotlib==2.0
pandas
numpy
scipy
ipython
statsmodels
arch
rpy2
cvxopt
sympy
nimfa
astroML
mahotas
pandas-datareader
scikit-learn
scikit-image
lmfit==0.8.3
autograd==1.1.7
sounddevice
tensorflow==1.4.1
keras==2.0.8
librosa
python_speech_features
gym==0.9.4
slycot
control
```

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

### Emacs Bağlantısı

Github projelerimizden biriyle Emacs'te LaTeX doküman *içinde* iken
Python kodlarını
[emacs-ipython](https://github.com/burakbayramli/emacs-ipython) adlı
bir teknoloji üzerinden direk belge içinde işletmek mümkün (arka
planda ipython'a bağlanıyor, yani aynı temel yapı kullanılıyor). Bu
durumda, emacs-ipython gereken tüm ipython ayarlarını kendisi yapıyor.

### R

Bazen Python içinden R kütüphanelerini çağırmak gerekebiliyor. R ayrı
bir dildir, ama Python içinden onun kütüphanelerini çağırabiliyoruz.

```
sudo apt-get install r-base-dev r-base python-rpy2
```

R kütüphanelerini R içinden kurmak lazım. Komut satırında R yazın, ve

> install.packages("[kutuphane ismi"])

Bir servis / makina listesi gösteren menü çıkacak, bu menüden bir
ülkeyi seçin, ve paket kurulacaktır. Bizim notlar için gereken paketler,

```
lme4
tseries
urca
```

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

http://sayilarvekuramlar.blogspot.com

Latex Format

The format of these documents, fonts, the pseudocode look-and-feel was
taken from Andrew Cotter's thesis called *Stochastic Optimization for
Machine Learning*.

## LICENSE

The code is licensed under GPL v3. See COPYING for details.






