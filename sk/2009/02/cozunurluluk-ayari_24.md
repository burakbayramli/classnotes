# Cozunurluluk Ayari

Javascript kullanarak degisik cozunurlukteki ekranlar icin goruntuyu
degistirmeniz gerekiyorsa, Javascript screen objesini kullanarak bunu
yapabilirsiniz. Diyelim ki bir uygulamayi 1280x800 cozunurluluk icin
yazdiniz, ama 1024x768 ekranda soldaki bosluk kismini sifira indirerek
ekrandaki gorsel elementleri gorus alanina sigdirmak istiyorsunuz. O
zaman once HTML kodunuzu alttaki gibi degistirin; <html><head> <script
src="resolution.js"></script> </head> <body onload="styleSwitcher();">
...  </body></html> body icinde yapilan Javascript cagrisi sayesinde
bu sayfa her yuklendiginde styleSwitcher() fonksiyonu
cagirilacaktir. Bu fonksiyonun kodu resolution.js dosyasi icinde.  Bu
Javascript fonksiyonu, ustteki sayfa yuklendiginde screen objesi
uzerinden hangi cozunurlukte oldugunu kontrol edecek. Eger daha
1024x768'te isek, o zaman gerekli CSS elementlerinin boyutlarini
degistirerek konumlandirmalari degistirecek.  function
styleSwitcher(){var width = screen.availWidth;if (width == 768 ){ var
theDiv = document.getElementById('div1'); if (theDiv != null)
theDiv.style.marginLeft = "0%"; ...}  Burada div1 kodundaki elementin
margin-left boyutunu degistirdik ve sifira indirdik.  Fonksiyon icinde
"her tarayiciya ozel" ayarlamalar da yapabilirsiniz. Tarayici ismine
erismek icin navigator.appName cagrisi yeterli; Bu cagridan mesela IE
icin geriye Microsoft Internet Explorer kelimesi geriye gelecek. Biz
butun sitemizi standartlari takip eden Firefox'a gore yapiyoruz ve
IE'nin uyumsuzluklarini yamamak icin ustteki gibi cagrilarla (ve daha
once belirttigimiz CSS numarasi ile) gerekli ekleri yapiyoruz.  var
browser=navigator.appName;if (browser == "Microsoft Internet
Explorer") { var theDiv = document.getElementById('div2'); if (theDiv
!= null) theDiv.style.left = "440"; ...}




