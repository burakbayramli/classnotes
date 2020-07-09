# Flask ile Dinamik Web Sayfalari Gelistirmek

Kurmak için sanal ortam kurup ortamdan,

pip install flask

Basit bir Flask uygulaması tek bir Python dosyası içinden servis
başlatma, Web aksiyonları, tıklama idaresi, diğer sayfalara
yönlendirme gibi işlemleri halledebilir. Dinamik sayfalar için
şablonlar (templates) özelliği var, değişken ile belirtilen yerler o
değişkenlerin değeri dışarıdan verilerek doldurulur.

Ilk main.py

```
from flask import Flask, render_template

app = Flask(__name__)

@app.route('/')
def index():
    return render_template('/index.html')

@app.route('/location/')
def location(coordinates):
    return render_template('/location.html', location=coordinates)

if __name__ == '__main__':
    app.debug = True
    app.run(host="localhost", port=5000)
```


Şablonların olağan olarak templates adında bir alt dizinde olduğu
kabul edilir.

Not: `app.debug=True` ile servis başlatınca kodda yapılan değişim
servisin tekrar yüklenmesine sebep olur, bu geliştirme açısından
faydalı.

Not: Chrome ile sayfa içeriği onbellekte tutuluyor ve kod değiştiği
farkedilemiyor. Bu sebeple geliştirme için Firefox daha iyi olabilir.

Sayfalar


index.html

```
<html>



  <h3>Benim Uygulamam</h3>


  <div id="menu1">
    <a href="/location/32324">
      My Location
    </a>
    <br/>
    ...
  </div>

</html>
```

location.html

```
<html>
  <h1>Location at {{ location }}!</h1>
</html>
```

Flask sevisini baslatmak icin python main.py yeterli. Tarayıcı
localhost:5000'e yönlendirilince sonuç görülecektir. Ana sayfada
bağlantıya tıklanınca bu bağlantıdaki url bilgisi Python kodu
üzerinden sonraki sayfaya aktarılacak, orada işlem yapılıp ikinci
sayfa dinamik olarak ekrana basılacak.

CSS

Sayfamiza CSS uygulamak istersek, mesela main.css dosyasinda, bu
dosyayi static alt dizinine koymak lazim (python uzerinden servis
edilmeyen her sayfa, icerik buraya gider), ve html icinde

```
  <head>
    <link rel="stylesheet" type="text/css" href="/static/main.css" media="screen" />
  </head>
```

gerekli.

Hafızada Sonuç, Referans Veri Tutmak

Flask kodumuzun her tarafindan erisilebilen ve orada olacagindan emin
olabilecegimiz bir obje istiyorsak, ve bir suru referans belgesine
bakip (oturum, vs. nedir, ne kadar hafizada tutulur, gibi) duruma ozel
bir sey ogrenmek istemiyorsak kendimiz bir tekil obje (singleton)
yaratabiliriz. Bu objenin ayni Python sureci icinde tek olacagindan
emin oluruz, kodumuzun farkli taraflari arasinda bilgi paylasmak icin
bu nesneyi kullanabiliriz.

class OnlyOne(object):
    class __OnlyOne:
        def __init__(self):
            self.db = None
            self.some_results = []
        def __str__(self):
            return self.val
    instance = None
    def __new__(cls): # __new__ always a classmethod
        if not OnlyOne.instance:
            OnlyOne.instance = OnlyOne.__OnlyOne()
            OnlyOne.instance.edible = # pandas yuklemesi yap
        return OnlyOne.instance
    def __getattr__(self, name):
        return getattr(self.instance, name)
    def __setattr__(self, name):
        return setattr(self.instance, name)

Simdi OnlyOne().db ile erişince ilk başta yükleme yapılıp ardından
yüklenen taban kullanılacak. Taban dediğimiz basit bir Pandas
Dataframe objesi. Sonuçları, mesela herhangi bir listelemeden gelecek
sonuçlar için some_results kullanmışız, ona OnlyOne().some_results ile
erisiriz.

HTML

Mobil için UI geliştirenler bilir, HTML görüntüsü küçük
çıkıyor. Mobilde ekranı dolduran türden görüntü için

```
<html>


  <head>
    ...
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
  </head>
  <body>
    ....


</html>
```

JSON Uretmek

Herhangi Python bazlı bir yazılımı dış dünyaya (servis olarak) açmak
için en basit seçeneklerden biri HTTP servisi üzerinden JSON iledir,
ayrıca bizi mantıklı ÜRL'ler kullanmaya teşvik eden REST yaklaşımı iyi
olur. En alt seviyede web servisi olarak mikro servis Flask
olabilir. Flask'in güzel bir özelliği mantıklı ÜRL ile Python
fonksiyonu ilişkisini etiket (annotation) ile halledebilmesi.

```
from flask import Flask, url_for, jsonify
from flask import request, Response
app = Flask(__name__)
@app.route('/')
def api_root():
    return 'Merhaba'
@app.route('/test/url', methods=["PUT", "POST"])
def rq():
    data = request.get_json(force=True)   
    articles = sorted(data['liste'])
    return jsonify({'liste': liste})
if __name__ == '__main__':
    app.debug = True
    app.run(host="localhost", port=8080)
```

Baslatiriz,

`python main.py`

Ve test icin localhost:8080 ziyaret edilir. Merhaba mesaji
gorulur. Sonra curl ile `curl -H "Content-Type: application/json" -d
'{"liste":[3,2,1]}' \http://localhost:8080/test/url`

ozel URL'e JSON gonderilir. Kodun tek yaptigi listeyi alip siralamak,
yani 3,2,1 siralanip 1,2,3 olarak geri gonderilecek.

Dosya Yuklemek (Upload)

Bir siteye yerel dosyalarımıza göndermek / yüklemek için HTML

```
     <form action="/book_uploader" method="POST" enctype = "multipart/form-data">
      <p>
        <input type = "file" name = "file" />
      </p>
      <input type="submit" value="Gonder"/>
    </form>
```


Python icinde

```
@app.route('/book_uploader', methods = ['GET', 'POST'])
def upload_file():

   if request.method == 'POST':
      f = request.files['file']
      fbook =  "/dizin/" + f.filename
      f.save(fbook)     
      return 'file uploaded successfully'

```






