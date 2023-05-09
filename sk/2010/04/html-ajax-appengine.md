# HTML -> Ajax -> AppEngine

Akilli telefonlar icin mobil kod gelistirilmesinde bir alternatif daha
sekillenmeye basliyor. iPhone dunyasinda AppStore uygulama deposuna
giren programlarin bazen cok uzun suren kabul surecinden gecmesi,
programcilari alternatif yollar aramaya itti. Diger yandan ayni kodla
birden fazla marka telefon uzerinde program isletebilmek ozelligi
aranir oldu, ve isletilen programin surum yapilir yapilmaz devreye
sokulabilmesi istegi, aynen masaustu ortaminda oldugu gibi tekrar
Web'den servis edilen HTML secenegini gundeme getirdi. Bu sekilde,
hicbir kabul surecinden gecmeden, bir URL uzerinden uygulama HTML
olarak servis edilebilmis olacakti. Ayrica HTML'in bir sonraki surumu
HTML5 mobil icin faydali ozellikler sunuyor; lokasyon verisini
saglayabilme, Net'e baglantisiz (offline) calisabilme, tarayici
uzerinde lokal veri depolayabilme gibi servisleri var.

Hem Android, hem iPhone baglaminda, HTML secenegiyle gorsel kodlama
icin yeni bir dil (API) ogrenmeye gerek kalmayacak, piyasada yaygin
bir HTML / CSS / Javascript tecrubesi mevcut.Bu durum telefon uzerinde
Java kodlamasi acisindan biraz kotu haber olabilir: Su anda Android
dahil olmak uzere mobil kodlama icin HTML5 + Servis Kodlarinin Java
icermesi gerekmiyor. Illa Android Market'ten indirilebilecek bir
uygulama istense dahi, bu tek class'lik bir Java kodu olabilir;
program baslar baslamaz WebView.loadUrl cagrisini yapar ve aradan
cekilir.Biz de bu secenegi takip edecegiz; teknoloji yelpazesindeki
son durum, telefon uzerinde HTML5, CSS / Javascript kodlarinin Ajax
uzerinden Google App Engine Python servis kodlarina baglanip bilgi
alip verdigi bir ortam olacak.Basit bir ornek ile baslayalim: Bir
test.html sayfasi, yuklenir yuklenmez bir GAE servisine baglanip, HTTP
GET uzerinden bilgi alacak, ve bunu Javascript uzerinden Document
objesiyle HTML icine yazacak.

Javascript ile Ajax cagrisi yapmanin en basit yolu bir Javascript
kutuphanesi kullanmak. Prototype kutuphanesi ile bu is cok
basit. prototype.js dosyasini indirip /files/ altina
koyuyoruz.

```
<html><head></head><body>
<script type="text/javascript" src="/files/prototype.js"></script>
<script type="text/javascript">new Ajax.Request( "http://localhost:8080/test",
{ method: "get", parameters: { "test": "value1"}, onSuccess: function(response){
   document.getElementById("test").innerHTML = response.responseText
},
onFailure: function(){ alert('ERROR'); } });
</script>
<h3>

<div id="test"> </div> </h3></body></html>
```

Bu servisin baglandigi Python kodlarina gelelim:

```python
# main.py
import cgi
import logging
import wsgiref.handlers

from google.appengine.ext import webapp
from testpage import *

logging.getLogger().setLevel(logging.DEBUG)

application = webapp.WSGIApplication([('/test', Test),], debug=True)

def main():
  wsgiref.handlers.CGIHandler().run(application)

if __name__ == '__main__':
  main()

# testpage.py#
from google.appengine.ext import webapp
from google.appengine.ext.webapp import template

class Test(webapp.RequestHandler):
   def get(self): tt = self.request.get('test')
      self.response.out.write(str(tt)+",")
      self.response.out.write("item1,") self.response.out.write("item2,")
      self.response.out.write("item3")
```

Uygulamayi ayarlamak icin app.yaml dosyasi soyle olacak:

```
application: testapp
version: 1
runtime: python
api_version: 1
handlers:- url:/filesstatic_dir:
files-url: .*
script: main.py
```

Simdi GAE gelistirme servisini dev_appserver.py ile baslatalim, ve
http://localhost:8080/files/test.html adresini ziyaret edelim. HTML
icinde Ajax ile GET cagrisi yaparken URL uzerinden 'test' adli
parametreyle servise 'value1' degeri gonderdik, bu deger ve onun
yaninda diger bazi degerler geri gonderilecek. Tum bu degerlerin
ekranda basildigini gorecegiz; Javascript bunu bos biraktigimiz 'div'
icinde innerHTML'e degerleri gecerek yapacak.Not: HTML icinde gorulen
http://localhost:8080/test referansi anlatim kolayligi icin
kullanildi, aslinda tum URL yerine sadece /test kullanmak ta
yeterli. Hatta gelistirme, sonuc ortami arasinda rahat gidip
gelebilmek icin boyle yapmak daha mantikli.

