# Android -> AppEngine Bağlantısı, Tekil Kullanıcılar

Bir Android mobil uygulamasının servis tarafı bir kodlar ile entegre
edilmesi gerekebilir. Ne amaçla? Belki ağır hesap gerektiren kodlar
var ve bu yükü servis tarafına aktarmak istiyoruz. Ya da, mobil
programın kullanıcılarının birbiri ile mesajlaşması gerekiyor, o zaman
servis makinası bu mesajlaşmayı idare edecek bir trafik polisliği
görevini yapacak.Entegrasyonun şekline gelince; Bu entegrasyon APİ
bazlı olabilir (client Java kodları gerekli noktalarda servise APİ
çağrısı yapar, bilgiyi kendi lokal ekranını değiştirmek için
kullanır), ya da uygulama bir noktadan sonra kontrolü tamamen Web
sayfa bazlı servis tarafına aktarabilir. Bu yazıda işleyeceğimiz
ikinci türden bir bağlantı olacak. Bizim projemizin ihtiyaçları her
telefonun bir kullanıcı gibi gözüktüğü ve bu kullanıcının diğerleri
ile Web sayfaları üzerinden diğerleri ile mesajlaşabildiği bir
sistemdi. Servis tarafında Python Google App Engine (GAE)
kullanıldı.Bu durumda, ilk önce kimlik kavramını halletmek
gerekiyor.

Kullanıcının kimliğini servis tarafına tanıtmak için ve kullanıcının
Google'da hesabı olduğu durumda, kullanıcı / şifre kontrolünün
Android'den Google'a çağrı ile yapılıp, gerekli bilgilerin servis
tarafına aktarıldığı ve GAE'nin otomatik olarak bu kullanıcıya kendi
APİ'si ile rahatça erisebilmesi sayesinde programa entegre edildiği
birn çözüm düşünülebilirdi. Fakat bu çözüm 1) sizin programınızı
kullanacak herkesin google hesabı olmasını gerektirdiği 2)
kullanıcının sizin Android programınıza güvenmeyip kullanıcı / şifre
bilgilerini vermekten kaçınabileceği olasılığından hareketle takip
edilmedi. Bunun yerine zaten kullanıcıya özel, onu tekil olarak
kimlikleyebilecek başka bir çözümü seçtik. O şey aslında telefonun ta
kendisidir. Bir telefon kullanıcıya özel olduğuna göre telefon =
uygulama = kimlik gibi bir irdeleme yapılabilirdi. Yani telefonda
kurulan sizin Android programınız tek kullanıcıya göre
yazılabilirdi.Bunu halletmenin en basit yolu, ismi iyi bilinen
(well-known) tek bir dosya içinde yoksa kimlik yaratmak, varsa o
kimliği Web tarafı ile her iletişimde kimlik bilgisi olarak
kullanmaktı.Kimlik değeri Google Bigtable tabanında her kayıt için
üretilen kimlik değerinden ibaret olabilir. Bu kimliğin üretiminin
servis tarafında yapılması lazım doğal olarak, yoksa kimlik
çakışmaları olabilir.Bizim çözümde APİ usulü verilip alınan tek bilgi
kimlik değeri.

Bunun için çok basit bir çağrı mekanizması kullandık; düz HTTP GET ile
http://[BİZİM ÜRL]/yenikullanıcı?param1=... gibi özel bir ürl ziyaret
ediliyor, bu ziyaret HTTP ile Android içinden yapılıyor, ve bu ziyaret
karşılığında servis tarafı hemen bir kullanıcı yaratıp, onun kimliğini
response üzerinde print ile cevaba basıyor. Bu cevap bağlanan tarafta
okunup, kimlik olarak hemen telefonda iyi bilinen dosyaya yazılacak ve
o dosya, o kimlik telefon için hiç değişmeyecek şekilde set edilmiş
olacak.

Servis tarafi:

```python
class NewUser(webapp.RequestHandler):
  def get(self):
     param1 = self.request.get('param1') ..
     user = AppUser(param1=param1, ...)
     user.put()
     self.response.out.write(str(user.key()))
```

Android tarafinda ise genel baglanma, sonucu okuma kodu soyle:

```java
public String visitUrl(String url) {

HttpClient httpclient = new DefaultHttpClient(); // Prepare a request object

HttpGet httpget = new HttpGet(url); // Execute the request HttpResponse response;
  try {
    response = httpclient.execute(httpget);
    HttpEntity entity = response.getEntity();
    if (entity != null) {
      InputStream instream = entity.getContent();
      String result= convertStreamToString(instream);
     instream.close();
     return result; }
  } catch (ClientProtocolException e)
  {
    e.printStackTrace();
  } catch (IOException e)
  {
    e.printStackTrace();
  }
  
  return "";
}
```

Kimligin dosyada olup olmadiginin kontrolu ve okunup, kullanilmasi ise
sonyle. Bir Activity icinde

WebView webview = new WebView(this); setContentView(webview); ...
File f = new File("/data/data/[UYGULAMA PAKET ISMI]/files/[KIMLIK
DOSYASI]"); if (!f.exists()) { String url = "http://[UYGULAMA
URL]/yenikullanici?param1" + ...; String id = visitUrl(url);
FileOutputStream out = openFileOutput("[KIMLIK DOSYASI]",
Context.MODE_PRIVATE); out.write(serialize(id)); out.close(); }
FileInputStream in = openFileInput("[KIMLIK DOSYASI]"); byte [] bid =
new byte[in.available()]; in.read(bid); in.close(); String id =
(String)deserialize(bid); webview.loadUrl("http://[UYGULAMA
URL/giris?kimlik="+id); } catch (Exception e) { e.printStackTrace(); }
} public byte[] serialize(Object obj) throws java.io.IOException {
ByteArrayOutputStream bos = new ByteArrayOutputStream() ;
ObjectOutputStream out = new ObjectOutputStream(bos) ;
out.writeObject(obj); out.close(); byte[] buf = bos.toByteArray();
return buf; } public Object deserialize(byte [] bytes) throws
Exception { ObjectInputStream in = new ObjectInputStream(new
ByteArrayInputStream(bytes)); Object out = in.readObject();
in.close(); return out; }

Goruldugu gibi kimlik dosyasi araniyor (dikkat: Files objesi ile
/data/data/[DOSYA]/files kontrolu yapilmali, ama openFileOutput(..,
Context.MODE_PRIVATE) ve openFileInput cagrilari sadece [DOSYA] ismini
kullanmali) ve dosya yoksa, visitUrl ile kimlik ureten kodlara
baglaniliyor, deger okunuyor, dosyaya yaziliyor, ve bundan sonra
kontrolun tamamen Web'e aktarilacagi /giris noktasina kimlik bilgisi
kimlik=... ile aktariliyor.Simdi, bir puruz daha kaldi. Kendisi de
basli basina bir tarayici olan Webview objesine giris URL'i veriyoruz,
ve aradan cekiliyoruz, bundan sonra link, dugme tiklamalari, vs. hep
Web sayfalarinda olacak... Yani oyle oldugunu farz ediyorduk. Ilginc
bir sekilde, ilk sayfa sonrasi link tiklamalarinin bizi apayri bir
program olan Android Chrome tarayicisini goturdugunu gorduk. Bunu
engellemek, her tiklamanin yine Webview icinde kalmasini saglamak icin
su eki yapmak lazim. WebView objesini yarattiktan sonra Url takibi
fonksiyonunu tekrar tanimlamak ve yine Webview'in kendi ic loadUrl
cagrisina yonlendirmek gerekiyor.  WebView webview = new
WebView(this); webview.setWebViewClient(new WebViewClient() {
@Override public boolean shouldOverrideUrlLoading(WebView view, String
url) { view.loadUrl(url); return true; } }); setContentView(webview);
Niye her tiklamanin Webview icinde kalmasini istedik? Pek cok sebebi
var. Bir kere servis tarafina /giris?kimlik=.. ile giris yaptiktan
sonra geriye cookie icinde kimligi tekrar donduruyorduk (oturum
yaratmak icin), ve ikinci tiklama apayri bir program baslatinca cookie
kayboluyordu. Ayrica, Webview gorsel duzenlemesi Chrome
duzenlemesinden farkli olabiliyor; Chrome ile sayfalarimizi habire
zoom ettirmemiz gerekiyordu, vs.Bu kadar.

