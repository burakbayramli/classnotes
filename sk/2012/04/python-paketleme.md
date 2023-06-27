# Python Paketleme

Python paketleme sistemiyle kaynak kodumuzu paketleyerek ona `pip
install` ya da `setup.py install` sonrası herhangi bir dış dizin ya da
yeni bilgisayardan `import` ile erisebilmemiz sağlanır. Paketler
global kurulabilir ama çoğunlukla `virtualenv` benzeri sanal
geliştirme ortamı içinde kurulurlar.

Kurulum için bir `setup.py` gerekir. Mesela `modul1` adında paylaşmak
istediğimiz bir kod var, kodlar `/home/vs/dizin/modul1` altında, bu
dizinde en üst seviyede bir `setup.py` koyarız, 

```
from setuptools import setup

setup(name='modul1',
      version='0.1',
      description='Modul 1 cok onemli seyler yapar',
      url='https://github.com/user/modul1',
      author='Burak Bayramli',
      author_email='dd@post.com',
      license='MIT',
      packages=['modul1'],
      install_requires=['dateutil'],
      zip_safe=False)
```

olabilir. 

Not: Paketleme servisinin isimleme yöntemi sebebiyle kodlarımızın
`modul1` altında bir `modul1` alt dizini içinde olması lazım.

Bu altdizin içinde mutlaka bir `__init__.py` dosyası gerekli, içinde
en basit kullanım için

```python
from .modul1 import *
```

olmalı. Bu sayede `import modul1` deyince `__init__.py` yükleniyor,
yüklenen bu dosya da diğer kod dosyalarımızı yükleyip kullanıma
açıyor. Böylece mesela `modul1/modul1` altında `source.py` diye bir
dosya varsa, onun içinde de `def callme()` gibi bir fonksiyon varsa,
üstteki tanım sayesinde `modul1.callme()` çağrısı artık yapılabilir.

Şimdi en üst dizinde iken `python setup.py install` ile kurulum
gerçekleştirebiliriz.

`install_requires` seçeneğine verilen liste install sırasında `pip` ile
otomatik kurulacak ek programların listesidir. Üstteki örnekte
`dateutil` adlı dış Python paketi kurulacaktır.

Eğer `setup.py develop` dersek belli bazı sembolik bağlantılar
sayesinde `modul1` geliştirme dizininde yaptığımız değişiklikler direk
kurulu kütüphane üzerinde etki yaratır. Bu geliştirme (develop) için
çok faydalı.

Eğer global ortamdaysak `install`` çağrısı global, sanal ortamda isek
sanal ortama kurulum yapar. Sanal ortam hakkındaki virtualenv yaziları
[2, 3].

Eğer paket içine veri dosyaları da eklemek istiyorsak, `setup.py` içinde

```
    include_package_data=True,
    package_data={
        "": ["*.zip","*.csv"]
    },
```

eki yapabiliriz. Bu durumda `modul1/modul1` altındaki tüm belirtilen
sonekli dosyalar toparlanıp pakete dahil edilecektir. `setup.py install`
sonrası, mesela benim `/home/burak/Documents/env3` sanal geliştirme
ortamım için kurulum

```
/home/burak/Documents/env3/lib/python3.6/site-packages/modul1-0.1--py3.6.egg
```

dizini altına gitti, oraya baktım, veri dosyaları oraya koyulmuştu. 

Tabii dikkat, bir sorun daha var, paketlenen veriye kod nasıl
erişecek? Bir çözüm işleyen kod dosyasının adresinin kod içinde,

```
data_dir = os.path.dirname(__file__)
```

ile alınması, ve kod içinde tüm veri erişimlerini mesela `veri.zip`
için `data_dir + "/veri.zip"` olarak değiştirmek. Ibare `__file__` o
anda içinde olunan dosyanın tüm adresidir, onun baz dizinini alıp veri
dosya erişimini ona göre ayarlıyoruz.

PyPi'da Paket Yayinlama, Pip Hazırlığı

Bildiğimiz gibi Python dosyalarının `pip` komutu ile kurulabilmesini
sağlayan bir altyapı var. Bu yapı

https://pypi.org/

adresinde, kodumuzu herkesin kullanımına açmak için oraya da
gönderebiliriz [1]. Eğer `setup.py` kurulumu tam yapılmışsa,

```
python setup.py sdist bdist_wheel
```

ile PyPi için gerekli dosyaları da üretmek mümkün, bu dosyalar `dist`
dizini altında konuluyor. Dikkat, önce `python setup.py build` komutu
işletin, sonra üsttekini, `dist` altında iki tane dosya lazım, `.zip`
ve `.whl` (`install` işletmeye gerek yok, bu komut bir .egg dosyası
yaratır, PyPi artık bu dosyaları taşımak istemiyor).

Nihai paketin PyPi'a gönderilmesi için PyPi'a üye olunması lazım, ve  `twine` adlı
araç gerekli, onu

```
pip install twine
```

ile kurabiliriz. Kurduktan sonra proje en üst dizininde

```
twine upload dist/*
```

işletiriz, sorulunca kullanıcı ve şifre girilir, ve kod PyPi sistemine transfer
edilir. Artık

https://pypi.org/project/modul1/

adresinde projemizi görebiliriz. 'Release history' seçilince orada şimdiye
kadar yayınladığımız tüm versiyonlar görülür. Bu versiyon numaraları `setup.py`
içinde tanımladığımız `version` tanımını baz alıyor tabii. Versiyon konusundan
devam edersek, eğer `setup.py` içindeki versiyonu değiştirip sistemi derleyip
PyPi'a `twine` ile tekrar gönderirsek, yeni versiyon 'Release history' altında
gözükecektir, kullanıcılar PyPi sayfasından ya da `pip install modul1==[versiyon]`
ile o versiyonlardan herhangi birini kurabilirler.

PyPi dokümantasyonu ile kolay entegre için güzel bir numara şu; nasıl olsa
Github için bir `README.md` yazıyoruz, bir tane daha ayrı dosyayı PyPi için
yaratmaya gerek yok, eğer `setup.py` içinde

```python
readme=open("README.md").read()

setuptools.setup(
  ...
  long_description=readme,
  long_description_content_type="text/markdown",    
  ..
)
```

tanımlarsak, bu paketi gönderince `README.md` içinde olan belgeleme direk
PyPİ sayfası olarak gözükecektir.

Not

PyPI'a versiyon arttırıp yeni kodu derleyip göndermeden önce kendi
yerel kurulumuzda `lib/python../site-packages` gibi bir dizinde
olacak, eski sürümün dizinini kendimiz silersek daha iyi olur.

Kaynaklar

[1] https://realpython.com/pypi-publish-python-package/#preparing-your-package-for-publication

[2] [Virtualenv](../../2018/08/virtualenv-python-izole-sanal-calsma.html)

[3] http://guide.python-distribute.org/creation.html

