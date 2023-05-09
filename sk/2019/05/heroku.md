# Heroku

İdare edilen (managed) bulut ortamlarında Google'ın App Engine
yaklaşımına benzer bir yaklaşım Heroku. Gerçi esnek bir sistem
ayarlama şekli var, veri tabanı, Django, onun yerine Flask gibi
yazılımları ortamımıza ekleyebiliyoruz. 

Heroku sanal ortamının işlemci birimi dyno; bir dyno bir sanal Linux
uygulama kabıdır (container). Ölçeklerken "dyno zamanından"
bahsedilir, bir dyno bir saat kullanılmışsa bir dyno saati
tüketilmiştir. Her uygulamaya Heroku tarafından ayda 750 bedava dyno
saati verilir. Bu pek çok basit uygulama için yeterlidir. İsteyenler
ek para ödeyerek daha fazla dyno saati satın alabilirler. Heroku bu
şekilde para kazanmayı umuyor muhakkak.

Not: Dyno'lar 30 dakika kullanılmamışsa uykuya dalar, tekrar uyanırken
biraz yavaşlık olabilir. Belki 10 dakikada bir uygulamayı dışarıdan
"dürterek", ping yaparak uyanık tutmak düşünülebilir.

Üye olmak için 

https://signup.heroku.com/signup/dc

Artık konsola girebiliriz. Heroku ile idare edilen uygulamalarımız,
projelerimiz var, bu projeleri istediğimiz gibi ölçekleyebiliyoruz,
onlara kaynak yöneltebiliyoruz. Mevcut uygulamaları görmek için 

https://dashboard.heroku.com/apps

Mevcut bir app'i silmek için app ismine tıklanır, Settings kısmında en
altta "Delete app" düğmesine basılıp app ismi bir daha girilip silme
yapılır. Yeni app için Create New App seçin. isim global olarak özgün
olmalı. Yerel bilgisayarda (daha önce kurulmadıysa bir kerelik)

```
sudo snap install heroku --classic
```

Simdi,

```
heroku login
```

dedikten sonra soruda enter'e basın, tarayıcıya gidiyor, bu geçici, iş
bittikten sonra tarayıcı kapatılıp komut satırına dönülebilir. Biraz
garip bir giriş yapma şekli ama işliyor.

Bir uygulama kuralım. Alttaki repo'da daha önce yaratılmış çok basit
bir Heroku uygulaması var. 

```
git clone git@github.com:franccesco/flask-heroku-example.git

cd flask-heroku-example
```

Ya da sıfırdan bir dizin içinde yeni kodlar koyup onları `git init`,
ve `git add` ile bir repo'ya dahil edebilirdik. Sonuç aynı. Simdi,

```
heroku apps:create flask-heroku-example-[bir şeyler, özgün isim olsun diye]
```

ile Heroku projesi yaratılır. Bu noktada hala app sonuç ortamına
gönderilmedi. Sayfanızı ziyaret ederseniz,

```
Heroku | Welcome to your new app!
```

mesajını görürsünüz.

```
git push heroku master
```

ile kod gönderin. Eğer problem çıkarsa `.git/config` içinde

```
url = https://git.heroku.com/flask-microblog-[...].git
```

olduğunu kontrol edin. Ve `git push` tekrarlayın.

Mimari açıdan bilinmesi gereken önemli bir faktör `push` yaptığımızda
Heroku'nun servisleri tekrar başlattığı... Bu sırada birkaç işlem
oluyor, repo'daki kod paketlenir, repo ile bağlı olan uygulama bulunup
onun dyno'ları indirilip yeni kodla tekrar başlatılır. Bu önemli,
çünkü mesela diyelim ki `static` altında sadece birkaç ufak sayfa
değiştirdik, bunları göndersek `push` bunu anlayıp servisleri tekrar
başlatmayabilirdi belki diye düşünebilirdik. Bu doğru değil. Bu tekrar
başlatma sırasında kullanıcı sisteme ufak bir süre erisemeyebilir, bu
sebeple, mesela bir içerik idare sistemi yazıyorsak bunu repo
üzerinden yapmamak gerekir. 


```
heroku open
```

ile tarayıcıyı direk uygulamanın işlediği URL'i ziyared edecek şekilde
açabiliyoruz.

Heroku'nun çok Github repo merkezli işlediğini farketmişizdir
herhalde. Kod gönderirken repo dizini içinde olmak lazım, `heroku
open` deyince o repo ile alakalı olan URL biliniyor, vs. Yani Heroku
uygulaması ile repo birbiri ile alakalı hale geliyor. Sistemleri böyle
işliyor.

Ölçekleme

```
heroku ps:scale web=1
```

```
Scaling dynos... done, now running web at 1:Free
```

`web=2` diyebilirdik.

Ayarlar

Üstte gösterilen örnek Python bazlı projeydi. Bu tür projelerin (ve
genel Heroku projelerinin) ayarı için `Pipfile` ve `Procfile`
dosyaları var. Bu dosyalar Heroku proje dizininde en üstte
görülebiliyor. Dosya `runtime.txt` içinde hangi python versiyonu
istediğimiz seçilebilir.

Dosya Sistemi

Dikkat: idare edilen bir ortam olduğu için Heroku "disk" ve "dosya
sistemi" erişimini garanti etmiyor. Daha doğrusu bir dosya sistemi var
ama bu sistem birdenbire değişip bir başka makinanın sistemine
dönüşebiliyor (herhalde arka planda yük dağıtımı yaparken kod bir o
bir makinaya kaydırılıyor). Bu yüzden Heroku disk sistemi için "uçucu
(ephemeral)" deniyor. Diske yazılan bir şeyin orada kalacağına
güvenmemek lazım. Kalıcı olmasını istediğimiz şeyleri Heroku
tarafından desteklenen Posgresql tabanına yazmak lazım.

Log

Geliştirme makinanızda `heroku logs --tail` ile nihai ortamdaki
`print` ve benzeri komutlarının çıktısını takip edebilirsiniz.

Python Paket Kullanımı

Eğer servis içinde python'un kullanması gereken ek paketler varsa,
bunları `Pipfile` içine koyabiliriz, mesela ek iki paket olarak

```
...
requests = "*"
urllib3 = "*"
```

ekleyebilirdim. Eğer versiyon numarasını da vermek istersem, mesela

```
bcrypt = ">=1.1"
cryptography = "==2.3"
```

kullanımı işliyor. Bu arada eğer varsa `Pipfile.lock` dosyasını
silin. Eğer problem çıkarsa uygulamanızı silip tekrar yaratın. Not:
İnternet'te `requirements.txt` kullanımı ile ilgili bazı tavsiyeler
var ama bunlar işlemiyor.

Uygulamamızın kaç bedava saati kaldığını görmek için

```
heroku ps -a [uygulama ismi]
```

Örnek sonuç

```
Free dyno hours quota remaining this month: 546h 5m (99%)
Free dyno usage for this app: 0h 55m (0%)
For more information on dyno sleeping and how to upgrade, see:
https://devcenter.heroku.com/articles/dyno-sleeping

=== web (Free): gunicorn app:app (1)
web.1: up 2019/05/27 12:19:55 +0300 (~ -172s ago)
```

Her hesaba her ay 550 bedava dyno zamanı veriliyor. Eğer kredi kartı
verilirse (bir ücret kesileceğinden değil, kayıtlarda bulunması
amacıyla) 450 saat aylık daha ekleniyor. Sadece 550'yi baz alalım, bu
günlük 550 / 30 = 18 kusur saat demektir, bir dyno bir saat işliyorsa
(nasıl işlerse işlesin, çok iş, az iş yapsın aynı) zaman buradan
düşülecektir. Dyno 30 dakika kullanılmadığında uykuya daldığını
düşünürsek, günlük 24 saatinin 18'inde hazır olabilecek bir servis
fena sayılmaz. Eğer iki dyno başlatırsak 550 saat ikisine eşit
bölüştürülür herhalde.

https://devcenter.heroku.com/articles/free-dyno-hours

Komut Satırı

Repo içinde `heroku run python` ile bir Python komut satırı,
yorumlayıcı elde edebiliriz. Tabii aslında normal dizüstünde
çalıştığımızda `python` ya da `ipython` işletince olanlardan ufak bir
farkı var, yeni yorumlayıcı başlatmıyoruz, Heroku servisimize bir
anlamda "bağlanıyoruz". Bu durumda, ve Flask çerçevesinde, mesela
`app.py` içinde bir Flask uygulamamız varsa, `import app` ile bu
uygulamanın koduna erisebiliriz. 

Veri Tabani Eklemek

Uygulamamıza bir servis olarak bir Postgresql tabanı "ekleyebiliriz".

https://data.heroku.com/

gidiyoruz, seçeneklerden Heroku Postgres seçiyoruz, "Create one"
tıklıyoruz, sonraki ekranda "Install Heroku Postgres"'e
tıklıyoruz. Sonraki pencerede PG'nin hangi uygulamaya atanacağı (app
to provision to) soruluyor, bu kutuya istediğimiz uygulamanın ismini
yazıyoruz. Seçip "Provision add-on" diyoruz. Belli PG seviyeleri var,
bedava en az kapsamlı olan "Hobby Dev" seviyesi. Olağan değer bu
olacaktır zaten.

PG eklendikten sonra tabana tıklarız, çıkan ekranda Settings ve View
Credentials ile tabana erişmek için gereken makina, taban ismi,
kullanıcı, vs bilgileri görebiliriz. Fakat bu bilgileri alıp bir
kenara yazmak, sonra uygulamanıza dışarıdan (bir ayar dosyası, ya da
python kodu içinde) eklemek yerine, taban URL'ini sistem çevre
değişkeni `os.environ['DATABASE_URL']` ile almak en iyisi. Bu bilgi
oraya Heroku sistemi tarafından set ediliyor. Bu şekilde okumak bir
diğer açıdan daha önemli, Heroku bazen güvenlik açısından taban erişim
bilgilerini rasgele bir şekilde değiştirebiliyor, sonra servisimizi
tekrar başlatıyor. Bu durumda Heroku yeni taban bilgisini çevre
değişkenine atar, ama koda, ayar dosyasına yazmışsak, değişimleri
otomatik olarak uygulayamayız.

Flask, SQLAlchemy Kullanım Kalıbı

SQLAlchemy bir ORM paketi, `class` tanımları ile SQL tabana erişmemizi
sağlıyor, hatta tabloları sıfırdan yaratmamızı sağlıyor. Bu paket için
bir kullanım kalıbı şöyle olabilir, Flask `app.py` içinde, ana script seviyesinde

```python
class ConfigClass(object):
    ...
    SQLALCHEMY_DATABASE_URI = os.environ['DATABASE_URL']
    SQLALCHEMY_TRACK_MODIFICATIONS = False
    ..
    
app = Flask(__name__)

app.config.from_object(__name__+'.ConfigClass')

db = SQLAlchemy(app)

class User(db.Model, UserMixin):
    __tablename__ = 'users'
    id = db.Column(db.Integer, primary_key=True)
    active = db.Column('is_active', db.Boolean(), nullable=False, server_default='1')
    ...

def create_all():
    db.create_all()
    if not User.query.filter(...).first():
        user = User(
            ...
        )
        db.session.commit()
```

Burada `db.create_all()` ile taban yaratmak, gerekli bazı başlangıç
verilerini eklemek için `create_all()` metotunu yazdık. Bazıları bu
metotu ana script seviyesine koyabilir, böylece her script
yüklendiğinde bu çağrı yapılmış olur. Ama `db.create_all()` metotu
taban mevcut ise tablo yaratmayı tekrarlamaz, veri eklemeyi de mevcut
veriye bakacak şekilde yazabiliriz, vs. Fakat eğer tek bir kez elle
kendimiz bu işi tetiklemek istiyorsak (benim tercihim), `heroku run
python` ile komut satırına girip `import app` ve `app.create_all()`
ile çağrıyı bir kerelik kendimiz yapabiliriz.

Komut satırında SQL sorguları elle yapabiliriz bu arada,

```
>>> res = app.db.engine.execute('select * from users');
>>> print (list(res))
```

gibi.. 



[1] https://blog.miguelgrinberg.com/post/the-flask-mega-tutorial-part-xviii-deployment-on-heroku

[2] https://devcenter.heroku.com/articles/getting-started-with-python

[3] https://realpython.com/flask-by-example-part-2-postgres-sqlalchemy-and-alembic/

[4] http://blog.sahildiwan.com/posts/flask-and-postgresql-app-deployed-on-heroku/
