# Yerel Makinada FB Connect Testleri


Yerel Makinada FB Connect Testleri



Yerel makinanizda FB Connect uygulamalari test edebilmek icin birkac ufak numara yeterli. Su yazidaki kodlari kullanalim. Once FB admin sayfasinda uygulamamizin Callback URL ve Connect URL'i olarak (mesela) http://bizimsiteismi.com girelim. Sonra yerel makinamizda (bizde Ubuntu) ustteki adresi 127.0.0.1'e map etmek gerekecek. Bunu yaparak FB baglanti kodlarini 'kandirmayi' umuyoruz; boylece FB adresi olan gercek bir makinaya baglandigini zannederken yerel makinaya baglaniyor olacak.Hemen /etc/hosts dosyasina giriyoruz ve127.0.0.1      bizimsiteismi.comsatirini giriyoruz. Sonra JBoss kurulum dizinine gidiyoruz, ve surada anlatilan degisiklikleri yapiyoruz ki, JBoss 8080 yerine 80 adresinde baslasin. JBoss sudo erisimi olmayan bir kullanici altindaysa, bu degisikligi yaptiktan sonra erisim hatasi alabilirsiniz, cunku Unix'te belli bir port degerinin altindakilere erisim icin root olmak gerekir; o zaman JBoss'u sudo olarak baslatmaniz gerekecek.Sonra, Seam uygulamamizda META-INF/application.xml dosyasina girip context-root ayarini /uygulamaismi gibi bir degerden sadece / degerine set ediyoruz, boylece tarayicimiza http://bizimsiteismi.com girdigimizde direk ana sayfamizi yuklensin.Bu kadar. Eger xd_receiver.htm dosyasini ust seviye dizininize koymayi unutmadiysaniz, daha once paylasmis oldugumuz test FB kodlarinin sayfasini tarayici ile ziyaret ettiginizde arkadaslarinizin listesini FQL ile alabiliyor olmaniz lazim. Statik IP adresi olan bir makinaya hic gerek yok.




