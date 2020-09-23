# Python Paketleme

Eger herhangi bir dizinden import paket şeklinde bir diğer koda erişim
yapabilmek istiyorsak, Python paketleme sisteminin öğrenmemiz
gerekiyor. Paketler nereden kurulduklarına göre global ya da sanal
ortam içinde benzer şekillerde kurulurlar.

Kurulum icin en basit durumda bir setup.py gerekir. Mesela
modul1 adinda paylasmak istedigimiz bir kod parcamiz var, kodlari
/vs/vs/dizin/modul1 altinda olsun, bu dizine bir setup.py koyariz,
icinde

```
from setuptools import setup

setup(name='modul1',
      version='0.1',
      description='Modul 1 cok onemli seyler yapar',
      url='https://github.com/isim/modul1',
      author='Burak Bayramli',
      author_email='dd@post.com',
      license='MIT',
      packages=['modul1'],
      install_requires=[
          'dateutil',
      ],
      zip_safe=False)
```

olabilir. 

Not: Herhangi bir sebepten dolayı modul1 dizini altında bir modul1 alt
dizini daha lazım.

Ayrıca bu altdızın içinde mutlaka bir __init__.py dosyası olmalı, boş
olabilir önemli değil.

Simdi ayni dizinde iken python setup.py install ile kurulumu
yapabiliriz.

install_requires secenegine verilen liste install sirasinda pip ile
otomatik kurulacak ek programların listesidir. Üstteki örnekte
dateutil adlı dış Python paketi kurulacaktır.

Eğer setup.py develop dersek belli bazı sembolik bağlantılar sayesinde
modul1 geliştirme dizininde yaptığımız değişiklikler direk kurulu
kütüphane üzerinde etki yaratır. Bu, adi uzerinde, gelistirme
(develop) icin cok faydali.

Eğer global ortamda isek install çağrısı global, sanal ortamda isek
install çağrısı sanal ortama kurulum yapar. Sanal ortam
hakkındaki virtualenv yazisi.

http://guide.python-distribute.org/creation.html




