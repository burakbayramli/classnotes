# Lokal Dosya Arama - recoll

Kendi diskimizdeki dosyalari, PDF, EPUB, ne olursa olsun,
indeksleyebilmemizi saglayan `recoll` araci var. Kurmak icin

```
sudo apt-get install recoll
```

Komutta `recoll` yazılınca GUI açılır. `Preferences | Indexing configuration` 
ile indekslenecek dizinler eklenebilir, çıkartılabilir. Komut
satırından indeksleme için `recollindex` çağrısı yeterli. Ben tipik olarak
bir kitaplar üst dizinini veriyorum, ve oraya yeni doküman eklendikçe arada
sırada `recollindex` çağrısı yapıp indeksi güncelliyorum.

Doküman bulmak için `recoll` GUI'sinde kutuya istenen kelimeler yazılır
ve sonuçlar altta listelenir.

Komut Satırından Aramak

Direk komut satırından arama için `recoll -t` çağrısı ardından anahtar
kelimeler verilir, ve sonuç listelenebilir. Bu çağrıyı sarmalayan basit bir
Python script alttadır,

```python
import subprocess, re, sys
q = "kelime"
p = subprocess.Popen(['recoll','-t',q], stdout=subprocess.PIPE)
for line in p.stdout:
    res = re.findall('\[file:\/\/(.*?)\]', str(line), re.DOTALL)
    print (res)
    if (len(res)>0):
        print ("%s:1:1" % res[0])
```

Fakat bu çıktıdan arama sonucunu temsil eden özet paragraflar
çıkartmak zor olabilir.

Bir diğer yöntem `recoll` yazılımının kendi kaynağı içindeki Python
kodlarını bir Python paketi olarak kurmak. Fakat bunu için tüm recoll
kodunu almak ve derlemek gerekiyor.

Kaynaklar [1] adresinde, kurulus tarifi [2]. Onceden bazi paketleri
kurmak lazim, bizim uyguladigimiz komutlar,

```
sudo apt install libxslt1-dev zlib1g-dev libxapian-dev libx11-dev libaspell-dev
```

Ardından recoll açılmış zip dizinine gidilir, ve

```
./configure --disable-qtgui --disable-python-chm
```

Bu bir Makefile hazırlayacak, aynı zamanda Python paketini de kurulmaya
hazır hale getirecek. Eğer tüm recoll bu kaynaktan gelsin istiyorsak

```
make
sudo make install
```

ile kurulumu yapabiliriz.

Bu bittikten sonra `recoll --version` ile yeni kurulum yapıldığını kontrol
ederiz, şimdi sıra Python'a geldi. Alt dizin `python/recoll` altında şimdi
bir `setup.py` dosyası olmalı. Yanlız dikkat: bu dosyayı yazanlar kodu
biraz eksik bırakmış, mesela virtualenv gibi geliştirme ortamı kullananlar
ortamlarına girip hangi `setup.py` üzerinde `install` işletirlerse o
kurulumun içinde olduğu ortamın parçası olmasını beklerler. Bu arkadaşlar
böyle yapmamış, tüm hazırlığı sistem seviyesindeki Python için yapmışlar.

O sebeple eğer bir virtualenv ortamındaysak mesela `deactivate` ile
dışarı çıkıyoruz, ve oradan eski usul, ilk Python öğrendiğimiz
emekleme günlerinde olduğu gibi haldır huldur bir `sudo python3
setup.py install` işletiyoruz. Bu komut kurulumu sistem seviyesinde
yapacaktır. Şimdi eğer bu paketi geliştirme ortamından kullanmak istersek
bir takla atarak sistem Python'undaki recoll'dan geliştirme
ortamına bir sembolik bağlantı oluşturmak yeterli. Mesela ben kendi `env3`
adındaki ortamımın dizini `env3/lib/python3.6/site-packages` altında

```
/usr/local/lib/python3.6/dist-packages/Recoll-1.32.7-py3.6-linux-x86_64.egg/recoll
```

adresini gösteren bir `recoll` sembolik bağlantısı yarattım, ve her şey
ortam içinden normal şekilde çalışmaya başladı. Not: üstteki dizin herkes
için aynı olmayabilir, mesela python3.6 yerine başka bir dizin olabilir,
bunu kontrol etmek gerekebilir.

Bu paketi kullanan bir kodu altta görebiliriz, bir kelime arayıp ilk 5
sonucu özetleriyle beraber gösteriyor.

```python
from recoll import recoll
db = recoll.connect()
db.setAbstractParams(maxchars=300, contextwords=4)
q = "kelime"
query = db.query()
nres = query.execute(q)
print("Result count: %d" % nres)
for i in range(5):
    doc = query.fetchone()
    print("Result #%d" % (query.rownumber))
    for k in ("title", "size", "url"):
        print("%s : %s" % (k, getattr(doc, k)))
    print("%s\n" % db.makeDocAbstract(doc, query))
```


Kaynaklar

[1] [LesBonsComptes Download](https://www.lesbonscomptes.com/recoll/pages/download.html)

[2] [LesBonsComptes Install](https://www.lesbonscomptes.com/recoll/usermanual/usermanual.html#RCL.INSTALL)
