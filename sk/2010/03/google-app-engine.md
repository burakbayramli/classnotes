# Google App Engine

Android mobil uygulamamiz icin bazi servis tarafi ekler gerekince,
test olarak Google App Engine'i (GAE) denedik. GAE'nin Amazon EC2'den
farki belli kendine has API'lar uzerine kurulmus olmasi, Web
uygulamalari Google'in sagladigi fonksiyonlarla calismali. GAE Java
dilini destekliyor, fakat biz Python kullanmaya karar
verdik. Gelistirme daha hizli oldu, Python dilinin esnekligi faydali
oldu. Mevcut cozum yelpazemiz, telefon / client uzerinde Java, servis
tarafinda Python + GAE yonune dogru evriliyor.Ozelde GAE + Python ile,
genelde "bulut" kavrami ile servis tarafi kodlamasi kolaylasmis. En
basit servis Python kodu bir CGI islemcisi (daha cetrefil, taslak HTML
bazli Web kodlamasi icin Django altyapisi destekleniyor), request
uzerinden alinan bilgi islenip response uzerinden cikti verebilen
turden. Boyle bir kod, bir ayar dosyasi, bir tane Python dosyasi
uzerinden yapilabiliyor. Script bir main() cagrisi aslinda, CGI
mentalitesine gore Web Server (Apache gibi) istegi alir, (eskiden) C
ile yazilmis bir main islerkodu (executable) isletir, ve sonuc o
script icinde uretilirdi. Yorumlanan dillerde daha esnek olunabiliyor
tabii, tum script onbellege tasinabiliyor, bu sayede script
seviyesinde (scope) olan her degisken onbellege alinmis oluyor. Bizim
kodun hazirlik olarak uzun bir dosyayi isleyip sonucu hash olarak
tutmasi gerekiyordu, bunu script seviyesinde bir degisken ile
hallettik. Dosya okumasi bir kere yapildi, ardindan tum islemler hash
ile calisti.Gelistirme icin suradan Python SDK
indirilebilirhttp://code.google.com/appengine/downloads.htmlDosyayi
indirip acinca, appcfg.py ve dev_appserver.py gibi dosyalar
goruluyor. Gelistirme yapmak icin bir gelistirme dizini acip, orada
app.yaml adli bir dosya yaratmak lazim. Bu uygulamanizi tarif eden bir
dosya. Icerigi cok basit:application: [UYGULAMA ISMI]version:
1runtime: pythonapi_version: 1handlers:- url: /imagesstatic_dir:
images- url: .*script: main.pyAna CGI script main.py, ki bu script'e
en ust dizin / uzerinden hemen erisilebilir, images adinda statik
dosyalar iceren bir dizinimiz var. Not: Statik dosyalarda 3000
sinirinin uzerine cikilamiyor. Google GAE sisteminin bir dosya
paylasma aracina donusmesini istemiyor herhalde. Her durumda, bu rakam
herhangi bir uygulama icin yeterli bir rakam.Gelistirme sureci soyle
baslatilir:python dev_appserver.py [GELISTIRME DIZINI]Ornek bir kod
soyle olabilir:import cgiimport loggingimport wsgiref.handlersfrom
google.appengine.ext import webappclass
MainPage(webapp.RequestHandler):def get(self): astro =
self.request.get('param1') logging.debug("....")
... self.response.out.write('[HTML
BURAYA]')logging.getLogger().setLevel(logging.DEBUG)application =
webapp.WSGIApplication([ ('/', MainPage), ], debug=True)def main():
wsgiref.handlers.CGIHandler().run(application)if __name__ ==
'__main__': main()Ustteki kod param1 adinda bir parametreyi URL'den
okur ve HTML ureterek bunu response olarak gonderir, arada loglama
yapar. Gelistirme / test sureci Java ortamindan daha iyi, Python
kodunu degistirdiginiz anda degisiklikler hemen kullanima giriyor,
servisi tekrar baslatmak gerekmiyor.GAE sonuc ortaminda kullanim /
performans acisindan su nokta onemli: Bir servis sureci
"baslatmiyoruz", ihtiyaca gore bulut script'i cagiriyor veya
cagirmiyor. Ortam konumsuz (stateless) bir ortam, arda arda iki cagri,
ayni kullanicidan gelse bile degisik server makinalara gidebilir, bu
yuzden konum bilgisi veri tabani ya da onbellek uzerinde
tutulmali. Dert degil.Surec idaresinin bulutta olmasi, ekonomik ayni
zamanda. Eger uygulamayi kullanan yoksa, cagri yapilmaz, ve degerli
mikroislemci zamani, bellek yeri heba edilmis olmaz.Bir diger sinir,
bir request / response dongusunun 30 saniye uzerine cikamamasi. Eger
bu olursa, izleyici bir program istegi otomatik olarak iptal
ediyor. Kontrollu bir ortam olan bulutta bunun yapilmasi da mantikli,
hem sistemin isleyisi, hem de istismar amacli kullanabilecek kisiler
baglaminda. Servis bir Web ortami, ve 30 saniyeden fazla surecek bir
islem ne kullanici, ne de servisci acisindan anlamli bir
islemdir.Gelistirme boyle. Gelistirme bitince, uygulamayi Google'da
kayit etmek lazim.https://appengine.google.com/adresine gidip
kullanici programini kayit eder. Kodu servis tarafina gondermek
icinpython appcfg.py update [GELISTIRME DIZINI]Script appcfg.py
program ismini nereden biliyor? app.yaml icinde yazili. Bu update
islemi kodlarinizi, dosyalarinizi alip sonuc ortamina gonderiyor. Bir
kere Google hesabiniz email ve sifre ile kontrol ediliyor (bir dahaki
seferde edilmeyecek) ve kod sonuc ortamina gonderiliyor. Uygulama
artik [UYGULAMA ISMI].appspot.com uzerinden cagrilabilir.Veri tabani
icin de durum ayni; kontrollu bir ortam, tabanin dosyalarini nerede
tuttugu, nasil dagittigi programciyi ilgilendirmiyor. Google API
uzerinden veri depolama, okunma yapildigi takdirde, verinin dagitimi,
erisiminin olceklenmesi GAE tarafindan hallediliyor.Gelistirme
sirasinda log mesajlarini dev_appserver.py baslattiginiz ekranda
goreceksiniz. Sonuc ortaminda ise log mesajlarina uygulamanizin admin
konsolundan erisebilirsiniz.Fiyatlandirma baglaminda, GAE ayda 5
milyon sayfa altindaki kullanimlar icin bedava. Bu rakam ufak olcekli
bir uygulamayi idare eder, bundan daha fazlasi oldugu durumda ya
uygulama kar etmeye baslamistir, ya da bir sekilde sponsor
edilebilecek durumdadir. Fakat bedava bir baslangic yolunun olmasi
iyi, ve Web girisimci mentalitesine uygun. Denemek bedava, cunku Web
dunyasinda bazen pek cok deney gerekebiliyor.





