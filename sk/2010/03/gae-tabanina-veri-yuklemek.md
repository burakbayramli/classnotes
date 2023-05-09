# GAE Tabanina Veri Yuklemek

Google App Engine ortamında uygulama için gereken bazı başlangıç
verileri olabilir. Bu verileri dışarıdan GAE'ye dahil etmek için
toptan yükleyici olarak "appçfg.py upload_data" kullanılabilir. Bu
komut ile düz dosyayı GAE tabanına toptan (bulk) olarak göndermek
mümkün. Fakat bundan önce düz dosyadaki her satırın bir objeye nasıl
tekabül ettiğini tanımlamamız gerekiyor.

GAE veri tabanı bir anahtar/değer tabanı (key/value store). Bu tür
tabanlar hakkında birkaç yazı yazdık; bir nevi Hash dictionary'nin
dağıtık çalışan versiyonu olan bu tabanlar devasa boyutlarda yük olan
Web ortamların demirbaşı haline geldi. Amazon, Facebook hep benzer
yaklaşımları kullanıyorlar.GAE ortamındaki Bigtable teknolojisi
Google'ın kendi ürünleri için de kullandığı taban (Gmail gibi). Bulut
kavramının, özellikle Google ortamında olduğu gibi idareli ve tanımlı
bir ortam için, önemli bir avantaj şudur. Tabana yazarsınız, ve işin
ölçeklenmesiyle hiç ilgilenmezsiniz. Eğer verinin kopyalanması
gerekiyorsa (mesela coğrafi lokasyana göre, kullanıcıya daha yakın bir
makinaya veri yakın olacaksa), taban onu arka planda
yapar. Yedeklemeyi yapar. Ölçeklemeyi yapar. Programcı sadece get(),
put() komutlarıyla uğraşır.

Veri yüklemeye gelelim. `app.yaml` dosyasında

```
handlers:
- url: /remote_api
script: $PYTHON_LIB/google/appengine/ext/remote_api/handler.py
login: admin
```

tanimini eklemeniz lazim. Duz dosyadaki ve veri tabanindaki kayiti,
class'i soyle tanimlariz. Mesela bir models.py dosyasi icinde

```
from google.appengine.ext import dbclass
Txn(db.Model):
  code = db.StringProperty()
  date = db.DateProperty()
  low = db.FloatProperty()
  high = db.FloatProperty()
  price = db.FloatProperty()
  increase = db.FloatProperty()
  volume = db.FloatProperty()
```

Sonra loader.py adli bir class icinde

```
import datetime
from google.appengine.ext import db
from google.appengine.tools import bulkloader
from models import *

class TxnLoader(bulkloader.Loader):
   def __init__(self):
      bulkloader.Loader.__init__( self, 'Txn', [('code', str), ('date',
lambda x: datetime.datetime.strptime(x, '%Y%m%d').date()), ('low',
float), ('high', float), ('price', float), ('increase', float),
('volume', float) ])loaders = [TxnLoader]
```

Eger Python yukleme cevre degiskenini degistirmek istemiyorsak, o
zaman

import syssys.path.append('[GELISTIRME DIZINI]')ibaresini de kullanmak
gerekecek.

Verileri gelistirme ortamina yuklemek icin

dev_appserver.py baslatiriz, ve

appcfg.py upload_data --config_file=loader.py --filename=[VERI
DOSYASI]--kind=Txn --url=http://localhost:8080/remote_api [GELISTIRME
DIZINI]

komutunu kullaniriz. Sonuc ortamina yuklemek icin once "appcfg.py
update" ile kodlari gondeririz, sonra appcfg.py upload_data
--config_file=loader.py --filename=[VERI DOSYASI]--kind=Txn
[GELISTIRME DIZINI]





