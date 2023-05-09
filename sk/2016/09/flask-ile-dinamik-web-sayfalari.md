# Flask ile Dinamik Web Sayfalari Gelistirmek

### Kuruluş

Kurmak için sanal ortam kurup oradan,

```
pip install flask
```

Basit bir Flask uygulaması tek bir Python dosyası içinden servis
başlatma, Web aksiyonları, tıklama idaresi, diğer sayfalara
yönlendirme gibi işlemleri halledebilir. Dinamik sayfalar için
şablonlar (templates) özelliği var, değişken ile belirtilen yerler o
değişkenlerin değeri dışarıdan verilerek doldurulur.

Ilk main.py

```python
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

Şablonların olağan ayarlara göre `templates` adında bir alt dizinde
olduğu kabul edilir.

`app.debug=True` ile servis başlatınca kodda yapılan değişim
servisin tekrar yüklenmesine sebep olur, bu geliştirme açısından
faydalı.

Chrome ile sayfa içeriği önbellekte tutuluyor ve kod değiştiği bazen
farkedilemiyor. Bu sebeple geliştirme için Firefox daha iyi olabilir.
Ya da Chrome'un güncellemesini CTRL-R ile zorlamak lazım.

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

### Form Verisi, Metin Kutusu

Örnek olarak bir girdi kutusu düşünelim, o kutuya girilen parametre
ile bir işlem yapılıyor, sonuçlar aynı sayfada gösteriliyor, bir arama
fonksiyonu mesela;

[Sayfa](sayfa1.txt)

Sonuçlar bir `results` adındaki bir liste içinde olacak bu liste olduğu
gibi ekrana basılacak. Servis tarafı,


```python
from flask import Flask, render_template, request

@app.route('/search')
def search():
    return render_template('/search.html')

@app.route('/submit_search', methods=['POST'])
def submit_search():
    print ("Arananan kelime:", request.form['search'])
    results = []
    for i in range(2):
    	results.append([i, 'sonuc '+str(i)])
    return render_template("/search.html",results=results)

```

Tarayıcıda `/search` adresine gideriz oradaki forma bilgi yazıp
tıklayınca girilen veriye `request.form['search']` ile
erisebiliyoruz. İki tane sonuç uydurduk üstte, listeye iki öğeli bir
satır ekledik, ve tüm listeyi sayfaya gönderdik. Sayfa tüm listeyi
gösterdi ama sayfa seviyesinde listeyi gezip satır ve öğelerine teker
teker erisebilirdi mesela,

[Sayfa](sayfa2.txt)

gibi. HTML ve kodun nasıl içiçe geçebildiğini görüyoruz.

Birden Fazla Aksiyon

Eğer form içerisinde iki tane düğme koymak istiyorsak ne yaparız? Bu
durumda her iki `input` için aynı ismi ama farklı `value` değerleri
verebiliriz, mesela

```
<input type="submit" name="action" value="Aksiyon 1"/>
<input type="submit" name="action" value="Aksiyon 2"/>
```

ve sonraki Python kodu içinde hangi aksiyona tıklanmış olduğunu
`request.form['action']` ile okuyabiliriz.

### Form Verisi Secenek Listesi (Dropdown List)

`<form>` içinde gerekli olan komutlar altta gösteriliyor,

[Sayfa](sayfa3.txt)

Secenegi form post edildikten sonra gonderilen metot icinde `request.form.get("secim")`
komutu ile okuyabiliriz. 

### Hafızada Sonuç, Referans Veri Tutmak

Flask kodumuzun her tarafindan erisilebilen ve orada olacagindan emin
olabilecegimiz bir obje istiyorsak, ve bir suru referans belgesine
bakip (oturum, vs. nedir, ne kadar hafizada tutulur, gibi) duruma ozel
bir sey ogrenmek istemiyorsak kendimiz bir tekil obje (singleton)
yaratabiliriz. Bu objenin ayni Python sureci icinde tek olacagindan
emin oluruz, kodumuzun farkli taraflari arasinda bilgi paylasmak icin
bu nesneyi kullanabiliriz.

```python
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
```

Simdi OnlyOne().db ile erişince ilk başta yükleme yapılıp ardından
yüklenen taban kullanılacak. Taban dediğimiz basit bir Pandas
Dataframe objesi. Sonuçları, mesela herhangi bir listelemeden gelecek
sonuçlar için some_results kullanmışız, ona OnlyOne().some_results ile
erisiriz.

### JSON Üretmek

Herhangi Python bazlı bir yazılımı dış dünyaya (servis olarak) açmak
için en basit seçeneklerden biri HTTP servisi üzerinden JSON iledir,
ayrıca bizi mantıklı URL'ler kullanmaya teşvik eden REST yaklaşımı iyi
olur. En alt seviyede web servisi olarak mikro servis Flask
olabilir. Flask'in güzel bir özelliği mantıklı URL ile Python
fonksiyonu ilişkisini etiket (annotation) ile halledebilmesi.

```python
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

Başlatırız,

`python main.py`

Ve test için localhost:8080 ziyaret edilir. Merhaba mesajı
görülür. Sonra curl ile

```
curl -H "Content-Type: application/json" -d '{"liste":[3,2,1]}'
  http://localhost:8080/test/url`
```

ozel URL'e JSON gonderilir. Kodun tek yaptigi listeyi alip siralamak,
yani 3,2,1 siralanip 1,2,3 olarak geri gonderilecek.

Ayni cagriyi Python ile de yapabilirdik, mesela

```python
import requests
response = requests.post('http://localhost:8080/test/url', json={"liste":[3,2,1]})
print("Status code: ", response.status_code)
print(response.json())
```

### Dinamik İmaj, HTML Üretmek

Bazen kontrolü aktardığımız bir sonraki ekranın tamamen bir imaj, ya
da tamamen pür bizim ürettiğimiz (şablon kullanmadan) bir HTML
olmasını isteyebiliriz. Her iki durumda da `send_file` komutu faydalı.
Mesela

```
@app.route('/show_img`)
def travel_maps(coords,resolution):
  ...  
  fout = ... # imaj yarat, /tmp altinda olabilir, isim fout
  return send_file(fout)  
```

HTML dosyaları için benzer mantık yeterli.

### Dosya Yüklemek (Upload)

Bir siteye yerel dosyalarımıza göndermek / yüklemek için HTML

```
 <form action="/book_uploader" method="POST" enctype = "multipart/form-data">
  <p>
    <input type = "file" name = "file" />
  </p>
  <input type="submit" value="Gonder"/>
</form>
```

Python içinde

```python
@app.route('/book_uploader', methods = ['GET', 'POST'])
def upload_file():

   if request.method == 'POST':
      f = request.files['file']
      fbook =  "/dizin/" + f.filename
      f.save(fbook)     
      return 'file uploaded successfully'
```

### Dosya İndirmek

Ekrana basilan degil, eğer eklenti (attachement) olarak indirim
(download) dizinine gidecek bir dosyayı tarayıcıya vermek istiyorsak,
`send_file` kullanabiliriz. Alttaki örnekte `/tmp/out.csv` dosyasının
bir şekilde üretildiğini farzediyoruz,

@app.route('/test_csv')
def test_csv():
    return send_file('/tmp/out.csv',mimetype='text/csv',as_attachment=True)

Artık tarayıcıyla `/test_csv` adresi ziyaret edildiğinde dosya indirilmeye
başlayacaktır. 


### Başlatma Numaraları

En basit kullanımla bir `app.py` içinde olan uygulamayı `python
app.py` ile başlatırız. Fakat diyelim ki programları `/dizin/dizin`
altında olan `env1` adında bir virtualenv ortamı içindeyiz, ayrıca
mesela otomatik başlatım yapmak istiyoruz ve tüm dosya erişimini
vermek lazım o zaman `/dizin/dizin/env1/bin/python /vs1/vs2/app.py`
ile başlatım yapılabilir. `env1` altındaki python'u kullanınca o
virtualenv ortamı otomatik olarak aktif hale gelir ve oradaki tüm
paketler kullanıma girer.

Ya peki Flask başlayınca nereden başlatırsak başlatalım üstünde
olduğumüz dizinin (current directory) `app.py` ile aynı lmasını
istersek? Bu durumda `app.py` kodunun en başına

```python
import os; os.chdir(os.path.dirname(os.path.abspath(__file__)))
```

eklerse başlangıçta ilk yapılan iş kodun olduğu dizine gitmek olacaktır.

Disaridan istemci, tarayici erisimi

Eğer servise dışarıdan erişmek istiyorsak `host` için o erişilecek IP
adresini vermemiz gerekir, makinanın adresi `ifconfig -a` ile
bakılabilir, ve mesela `host="192.168.22.33"` gibi bir seçenek
geçilir.






