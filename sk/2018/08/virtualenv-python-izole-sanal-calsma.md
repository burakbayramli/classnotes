# virtualenv, Python İzole, Sanal Çalışma Alanı (Python Virtual Environment)

Farklı projelerde çalışırken kurulması gereken farklı paketler
olabilir, bu paketlerden bazıları diğerleri ile tam uyumlu
olmayabilir, ya da bazı projelerde Python 2 diğerinde Python 3
kullanmak gerekebilir. Bu tür ihtiyaçlar için virtualenv ya da
Anaconda kullanmak mümkün, Anaconda daha önce islendi, virtualenv'e
göre daha iddialı bir sistem denebilir. Anaconda hem sanal ortam, hem
de pip yerine geçmek isteyen bir paket idare sistemiyle de geliyor
(Anaconda'da paket kurmak için pip install yerine conda install denir
mesela).

Biz daha az iddialı ama bir o kadar faydalı, sadece sanal, izole ortam
yaratan virtualenv'i anlatalım.

Ubuntu'da mesela python kurduğumuzda, bu `sudo apt-get` ile olabilir,
komut satırında çağırdığımız python global, makinanın tümü için
geçerli bir çağrıdır. Ardından pip install ile paket kurduğumuzda bu
global python için yine global paket kurulumu yapmış oluruz. Kurulan
paketler /usr/local/lib/python2.7/dist-packages gibi yerlere
kurulur. İşte çakışma, potansiyel problemler burada ortaya çıkar.

Sanal ortam ile belli izole dizinler altında, ayrı python'u ayrı
paketleri olan sanal ortamlar yaratabiliriz.

```
sudo apt install python3-virtualenv
```

kurduktan sonra mesela bir Python 2 ortamı yaratalım,

```
virtualenv -p /usr/bin/python2 py2a
```

Sonuc

```
Running virtualenv with interpreter /usr/bin/python2
New python executable in /home/burak/py2a/bin/python2
Also creating executable in /home/burak/py2a/bin/python
Installing setuptools, pip, wheel...done.
```

Şimdi `ls` ile dizine bakalım, bir py2a alt dizini göreceğiz. Şimdi

```
source py2a/bin/activate
```

işletelim. Böylece biraz önce tanımladığımız sanal ortama girmiş
oluyoruz. Bu komut, komut satır işaretini değiştirecek ve içinde
olduğumuz sanal ortam ismi orada gözükecek, 

```
(py2a) burak@burak-Aspire-R3-131T:~$
```

oldu. Burada which python isletirsek

```
/home/burak/py2a/bin/python
```

görürüz, yani py2a altındaki python kullanılıyor. Gerçi bu bir
sembolik bağlantı, ve başta verdiğimiz python'a bağlanıyor, fakat
diğer her tür irdeleme için bu Python o sanal ortamda yerel kabul
edilebilir. Burada artık pip install işletince Python paketleri sanal
ortam içinde kurulacak. "Dışarıdaki" global python bunları
göremeyecek. Örnek için bir pip
install ardından py2a/lib/python2.7/site-packages altına bakılabilir. 

Sanal ortamları yerel dizinler üzerinden idare etmenin güzel tarafı 1)
Güvenlik - artık paketler, kurulumu, herşey yerel bir dizin üzerinden
idare edilebilir 2) Kolaylık - eğer bir ortamı silmek istiyorsak onun
sanal dizinini sildiğimiz anda bu ortam silinmiş olur. 

Çıkmak için deactivate işletilir ve normal komut satırına geri dönülür.

Script ile nasil program isletilir? Bazen sadece py2a/bin/python
script.py çağrısı yaparak pür bu ortamdaki paketleri
işletebiliyoruz. Fakat bu her zaman işlemeyebilir, o zaman komut
script'i içinde, sh ya da bash mesela, önce source sonra Python
çağrısı yapmak lazım.

Python 3

Ubuntu'da artık Python 3 yorumlayıcısı ayrı bir python3 komutu ile
geliyor, apt-get ile python3 kurmak mumkun. Bu
komut /usr/bin/python3 dosyasında; istersek onun üzerinden ayrı bir
sanal ortam da yaratabilirdik, 

virtualenv -p /usr/bin/python3 py3a

ve

source py3a/bin/activate

ile bu yeni ortama gireriz. Burada python --version deyince

```
Python 3.5.2
```

görülecektir.

Not: Ubuntu Python 3 için mesela pip3 sağlamış, apt-get ile kurulan
paketleri de python3-xxx diye kurulabiliyor. Bir bakıma Ubuntu da bir
kolaylık sağlayarak Python 2 ile 3'ü ayrı isimlendirme üzerinden
birbirinden ayırmış, fakat nihai izolasyon için hala virtualenv lazım
olacaktır.

Spesifik Python Versiyonları

Diyelim ki çok spesifik bir Python versiyonu gerekiyor, bizde 2.7.1
var, ama illa ki 2.7.7 lazım. Olur mu? Olur. Öyle bir paket vardır ki
belli bir Python versiyonunda desteklenmektedir sadece, vs. Bu gibi
durumlarda Python'u kaynaklarından derlemek gerekebilir, ve sonra
virtualenv ile sanal ortam yaratılırken o derlenen Python
kullanılır. Derlemek için 

```
wget http://www.python.org/ftp/python/2.7.7/Python-2.7.7.tgz

tar zxvf Python-2.7.7.tgz

cd Python-2.7.7

./configure --prefix=/home/burak/opt/python-2.7.7

make

make install
```

Artık

virtualenv -p /home/burak/opt/python-2.7.7/bin/python py2b

diyebiliriz. 

source py2b/bin/activate

ile ortama girince python --version çağrısının

Python 2.7.7

cevabını verdiğini göreceğiz.

Detaylı görsel tarif için [video](virtualenv-python-izole-sanal-calsma-video.html).

Kaynaklar

[Dreamhost 1](https://help.dreamhost.com/hc/en-us/articles/115000218612-Installing-a-custom-version-of-Python-2)

[Dreamhost 2](https://help.dreamhost.com/hc/en-us/articles/215489338-Installing-and-using-virtualenv-with-Python-2)
