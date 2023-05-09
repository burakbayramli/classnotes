# Android -> AppEngine Baglantisi, Tekil Kullanicilar

Bir Android mobil uygulamasinin servis tarafi bir kodlar ile entegre
edilmesi gerekebilir. Ne amacla? Belki agir hesap gerektiren kodlar
var ve bu yuku servis tarafina aktarmak istiyoruz. Ya da, mobil
programin kullanicilarinin birbiri ile mesajlasmasi gerekiyor, o zaman
servis makinasi bu mesajlasmayi idare edecek bir trafik polisligi
gorevini yapacak.Entegrasyonun sekline gelince; Bu entegrasyon API
bazli olabilir (client Java kodlari gerekli noktalarda servise API
cagrisi yapar, bilgiyi kendi lokal ekranini degistirmek icin
kullanir), ya da uygulama bir noktadan sonra kontrolu tamamen Web
sayfa bazli servis tarafina aktarabilir. Bu yazida isleyecegimiz
ikinci turden bir baglanti olacak. Bizim projemizin ihtiyaclari her
telefonun bir kullanici gibi gozuktugu ve bu kullanicinin digerleri
ile Web sayfalari uzerinden digerleri ile mesajlasabildigi bir
sistemdi. Servis tarafinda Python Google App Engine (GAE)
kullanildi.Bu durumda, ilk once kimlik kavramini halletmek
gerekiyor.

Kullanicinin kimligini servis tarafina tanitmak icin ve kullanicinin
Google'da hesabi oldugu durumda, kullanici / sifre kontrolunun
Android'den Google'a cagri ile yapilip, gerekli bilgilerin servis
tarafina aktarildigi ve GAE'nin otomatik olarak bu kullaniciya kendi
API'si ile rahatca erisebilmesi sayesinde programa entegre edildigi
bir cozum dusunulebilirdi. Fakat bu cozum 1) sizin programinizi
kullanacak herkesin google hesabi olmasini gerektirdigi 2)
kullanicinin sizin Android programiniza guvenmeyip kullanici / sifre
bilgilerini vermekten kacinabilecegi olasiligindan hareketle takip
edilmedi. Bunun yerine zaten kullaniciya ozel, onu tekil olarak
kimlikleyebilecek baska bir cozumu sectik. O sey aslinda telefonun ta
kendisidir. Bir telefon kullaniciya ozel olduguna gore telefon =
uygulama = kimlik gibi bir irdeleme yapilabilirdi. Yani telefonda
kurulan sizin Android programiniz tek kullaniciya gore
yazilabilirdi.Bunu halletmenin en basit yolu, ismi iyi bilinen
(well-known) tek bir dosya icinde yoksa kimlik yaratmak, varsa o
kimligi Web tarafi ile her iletisimde kimlik bilgisi olarak
kullanmakti.Kimlik degeri Google Bigtable tabaninda her kayit icin
uretilen kimlik degerinden ibaret olabilir. Bu kimligin uretiminin
servis tarafinda yapilmasi lazim dogal olarak, yoksa kimlik
cakismalari olabilir.Bizim cozumde API usulu verilip alinan tek bilgi
kimlik degeri.

Bunun icin cok basit bir cagri mekanizmasi kullandik; duz HTTP GET ile
http://[BIZIM URL]/yenikullanici?param1=... gibi ozel bir url ziyaret
ediliyor, bu ziyaret HTTP ile Android icinden yapiliyor, ve bu ziyaret
karsiliginda servis tarafi hemen bir kullanici yaratip, onun kimligini
response uzerinde print ile cevaba basiyor. Bu cevap baglanan tarafta
okunup, kimlik olarak hemen telefonda iyi bilinen dosyaya yazilacak ve
o dosya, o kimlik telefon icin hic degismeyecek sekilde set edilmis
olacak.

Servis tarafi:class NewUser(webapp.RequestHandler): def get(self):
param1 = self.request.get('param1') ..  user = AppUser(param1=param1,
...)  user.put() self.response.out.write(str(user.key()))Android
tarafinda ise genel baglanma, sonucu okuma kodu soyle: public String
visitUrl(String url) { HttpClient httpclient = new
DefaultHttpClient(); // Prepare a request object HttpGet httpget = new
HttpGet(url); // Execute the request HttpResponse response; try {
response = httpclient.execute(httpget); HttpEntity entity =
response.getEntity(); if (entity != null) { InputStream instream =
entity.getContent(); String result= convertStreamToString(instream);
instream.close(); return result; } } catch (ClientProtocolException e)
{ e.printStackTrace(); } catch (IOException e) { e.printStackTrace();
} return ""; }Kimligin dosyada olup olmadiginin kontrolu ve okunup,
kullanilmasi ise soyle. Bir Activity icinde WebView webview = new
WebView(this); setContentView(webview); ...  File f = new
File("/data/data/[UYGULAMA PAKET ISMI]/files/[KIMLIK DOSYASI]"); if
(!f.exists()) { String url = "http://[UYGULAMA
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
in.close(); return out; }Goruldugu gibi kimlik dosyasi araniyor
(dikkat: Files objesi ile /data/data/[DOSYA]/files kontrolu yapilmali,
ama openFileOutput(.., Context.MODE_PRIVATE) ve openFileInput
cagrilari sadece [DOSYA] ismini kullanmali) ve dosya yoksa, visitUrl
ile kimlik ureten kodlara baglaniliyor, deger okunuyor, dosyaya
yaziliyor, ve bundan sonra kontrolun tamamen Web'e aktarilacagi /giris
noktasina kimlik bilgisi kimlik=... ile aktariliyor.Simdi, bir puruz
daha kaldi. Kendisi de basli basina bir tarayici olan Webview objesine
giris URL'i veriyoruz, ve aradan cekiliyoruz, bundan sonra link, dugme
tiklamalari, vs. hep Web sayfalarinda olacak... Yani oyle oldugunu
farz ediyorduk. Ilginc bir sekilde, ilk sayfa sonrasi link
tiklamalarinin bizi apayri bir program olan Android Chrome
tarayicisini goturdugunu gorduk. Bunu engellemek, her tiklamanin yine
Webview icinde kalmasini saglamak icin su eki yapmak lazim. WebView
objesini yarattiktan sonra Url takibi fonksiyonunu tekrar tanimlamak
ve yine Webview'in kendi ic loadUrl cagrisina yonlendirmek gerekiyor.
WebView webview = new WebView(this); webview.setWebViewClient(new
WebViewClient() { @Override public boolean
shouldOverrideUrlLoading(WebView view, String url) {
view.loadUrl(url); return true; } }); setContentView(webview); Niye
her tiklamanin Webview icinde kalmasini istedik? Pek cok sebebi
var. Bir kere servis tarafina /giris?kimlik=.. ile giris yaptiktan
sonra geriye cookie icinde kimligi tekrar donduruyorduk (oturum
yaratmak icin), ve ikinci tiklama apayri bir program baslatinca cookie
kayboluyordu. Ayrica, Webview gorsel duzenlemesi Chrome
duzenlemesinden farkli olabiliyor; Chrome ile sayfalarimizi habire
zoom ettirmemiz gerekiyordu, vs.Bu kadar.





