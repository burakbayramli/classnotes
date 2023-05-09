# Hizmeti Kullanılmaz Kılma Saldırısı (Denial of Service Attacks -DoS-), Hız Kısıtlamak (Rate Limiting)

Web sitemize saldırı yapmak isteyenler olabilir, bunlar ardı ardına
siteye bağlanarak işlemci (para) israfına sebep olabilir, ya da
sistemi aşırı yükleyerek başkası tarafından kullanılmaz hale
getirebilirler. Ya da en azından bir script ile ardı ardına farklı
saldırı yöntemlerini otomatik şekilde deneyebilirler.

Burada ortak bir nokta ardı ardına, sisteme çok hızlı bir şekilde
bağlanılıyor olması. O zaman sisteme erişim hızını kısıtlayabilirsek,
belki genel saldırıdan korunmuş oluruz.

Hız kısıtlamak için bazı seçenekler; eğer bir idare edilen ortamda
isek, mesela Google App Engine, Heroku gibi, makinalar onların bulut
ortamında işlediği için bu tür saldırıları onların engellemesini
bekleriz. Bu en iyi seçenek, hiç bir şey yapmaya gerek yok. 

Kendimiz bir dış servis kullanmak istersek,
[Cloudflare](https://www.cloudflare.com). Bu servisle sitemizin ismini
kontrol eden isim servisine (nameserver) DNS makinaları verdiğimizde,
genel DNS makinaları yerine Cloudflare DNS makinalarını veriyoruz,
böylece isim sorgulama işlemi onlar üzerinden akıyor. Hız gözleme,
filtreleme, kısıtlama işini onlar yapıyor.

Uygulama seviyesinde kendimiz bazı taklalar atabiliriz; mesela Flask
seviyesinde `flask_limiter` paketi var. Sınırlamaları her URL
`@app.route` için ayrı ayrı tanımlayabiliyoruz, eğer bir kullanıcı
sınırlamaları aşmışsa onlara 429 hata kodu donduruluyor. Özgün
"kullanıcı" tanımı ise `get_remote_address` ile okunabilen bağlananın
IP adresi olabilir.

```python
from flask import Flask
from flask_limiter import Limiter
from flask_limiter.util import get_remote_address

app = Flask(__name__)
limiter = Limiter(
    app,
    key_func=get_remote_address,
    default_limits=["200 per day", "50 per hour"]
)

@app.route("/slow")
@limiter.limit("1 per day")
def slow():
    return "24"

@app.route("/slowmin")
@limiter.limit("2 per minute")
def slowmin():
    return "24"

@app.route("/fast")
def fast():
    return "42"

if __name__ == '__main__':
    app.debug = True
    app.run(host="localhost",port=5000)       
```

Bu şekilde çetrefil dağıtık (distributed) DoS saldırıları
durdurulamayabilir, ama baz tek kişinin açıklara bakmak için mesela
ardı ardına enjeksiyon (injection) tekniklerini denediği durum
zorlaştırılabilir.

Özgün kullanıcı tanımı için İP adresi yeterli olmayabilir, İnternet
bağlantısının ortak kullanıldığı yerlerde pek çok kişi bir proxy
üzerinden dışarı bağlanır, onlar dışarıya tek bir İP adresi gibi
gözükebilirler, o zaman üstteki `key_func` için farklı bir çağrı
kullanabilir.


