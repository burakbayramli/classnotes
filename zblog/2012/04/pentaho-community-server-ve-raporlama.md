# Pentaho Community Server ve Raporlama




Pentaho Community Server ve Raporlama




Pentaho Community versiyonu kurulusu biraz kulfetli fakat kullanisli bir program. Gelisiguzel (ad-hoc) SQL'leri alarak istenilen sekilde formatlanmasini, hatta bu SQL'ler icin sonradan son kullanicinin girebilecegi parametreler tanimlanmasini, ve sonuclarin HTML yaninda PDF, XLS gibi pek cok formatta dokulmesini saglar. Yapi soyle: Report Designer adli bir zengin GUI programi rapor taslaginin hazirlamasini saglar, programcilar rapor taslagini zaten bildikleri SQL ile kurarlar, taslaklar hazirlaninca onlar bir server'a yayinlanabilir (publish).  Artik kullanicilar servise bir Web GUI uzerinen eriserek istedikleri anda bu raporlari isletirler.

Almak icin

sourceforge.net/projects/pentaho/files/

Bu linkten Business Intelligence Server ve Report Designer projelerinin tar.gz dosyalarini indirin. Once BI server. Bir dizine actiktan sonra [dizin]/biserver-ce dizinine gidin. Burada sh start-pentaho.sh komutu islemeyecek, catalina.sh dosyasindan sikayet edebilecek. Duzeltmek icin

chmod 777 tomcat/bin/*.sh

BI icinde Tomcat var, yani  java jdk de kurulmus olmali. Bir not daha: Mesela Postgresql paketten ciktigi haliyle gerekli JDBC jar dosyalarini icermiyor. Eger bu tur ek tabanlarla calismak istiyorsaniz, gerekli dosyalari bulup, biserver-ce/tomcat/lib altina koyun.

Sonra

biserver-ce/pentaho-solutions/system/publisher_config.xml dosyasina girin ve

<publisher-config>
<publisher-password></publisher-password>
</publisher-config>

icin bir sifre tanimlayin, mesela "admin"

<publisher-config>
<publisher-password>admin</publisher-password>
</publisher-config>

Artik BI calisir. http://localhost:8080/pentaho adresini kontrol edin. Pentaho User Console Login'e tiklayinca bir suru ornek kullanici gorebilirsiniz. Joe (Admin) ile is yapabilirsiniz, kullanici joe, sifre password. Bunlari sonra degistirirsiniz tabii.

Web bazli bir admin konsolu da var, fakat normal servisin parcasi degil. Onun  icin biserver-* altinda /administration-console dizinine gitmek ve start-pac.sh isletmek lazim. Web sayfalari localhost:8099 uzerinde sizi hazir bekliyor olacak. 

Simdi RD kurun. [dizin] altinda sh report-designer.sh ile baslatabilirsiniz. Ornek olarak unlu dellstore DVD dukkani tabanini kullanacagiz, sema ve veri dosyalari surada (dellstore2 baglantisi). Bunlari ise baslamadan once tabaninizda yaratmis olun.

http://pgfoundry.org/projects/dbsamples

File | New ile yeni bir rapora baslayalim. Bos bir sayfa cikacak. Sag bolume gidin ve iki tab icinde Data'ya tiklayin, ve Data Sets uzerinde sag tiklama yapin. JDBC secin.


Soyle bir pencere acilacak

Ilk kez RD kullandigimiz icin taban baglantisi hazirlamamiz gerekiyor, o yuzden sag ustteki yesil + ile baglanti eklemesi yapacagiz. Sonraki kullanimlarda hazir olan baglantilardan birini secerdik, mesela SampleData diye baslayan bir suru taban icin baglanti hazirlanmis, bunlari listeli cikiyor goruldugu gibi. Herneyse, yeni baglanti ekrani soyle

Biz burada dellstore2 tabanimiz icin parametreleri girdik. Test ile test edin, ve tamamsa OK ile ekleyin.


Baglanti solda listelenecek (lread'i biz daha once ekledik). Simdi raporumuza lazim olan veriyi saglamamiz gerekli, onun icin bir de sorgu (query) ekleyecegiz. Simdi "Available Queries" basligi yanindaki yesil + dugmesine tiklayin. Query denen alt pencerye

SELECT
EXTRACT(MONTH FROM orderdate) AS month,
EXTRACT(YEAR FROM orderdate) AS year,
SUM(totalamount) AS total
FROM orders
GROUP BY year, month
ORDER BY year, month;

sorgusunu girdik. Preview ile sonuclari kontrol edebilirsiniz, uc kolon var, tamamsa OK ile eklersiniz. Simdi bos raporun oldugu sayfaya donunce sag tarafta sunu gormemiz lazim,

Sorgu eklenmis ve uc tane kolon gozukuyor. Bu noktada bu uc kolonu surukle-birak ile bos raporun uzerinde istediginiz gibi konumlayabilirsiniz ve sol ust kosedeki goz isaretine tiklayip sonucu gorebilirsiniz.

Fakat bu "serbest sekilli" raporlar icin. Eger onceden tanimli kolonlari duzgun, renkleri ayarli bir rapor goruntusu isterseniz, onceden hazirlanmis sablonlara bakabilirsiniz. Bunun icin  File | Report Wizard secin. Cobalt secenegi fena degil, Next deyin, yine bir sorgu tanimlamak lazim. Data Source diyen pencerede yine + yapin, Choose Type icin JDBC secin, yine ayni sorgu ekrani gelecek. Taban dellstore, ve sorgu sagda yine ayni SQL, ve OK.

Ustteki pencereye donunce sorgu eklenmis halde. Simdi sorgunuz uzerine tek tiklama (yoksa Next aktif hale gelmiyor), ve Next. Cikan ekranda month, total, year kolonlarini secip, alttaki sag ok isareti ile alt sag kutuya tasiyin, ve Finish.

Rapor ekranina donuyoruz ve bu noktada raporun basligini, isterseniz kolonlarin tanimini degistirebiliyorsunuz. Raporun nasil gozukecegini gormek icin sol ustteki goz isaretini kullanabilirsiniz.

Guzel. Simdi raporun yayinlanmasina (publish) gelelim. File | Publish secin, gelen kutuda OK deyin, ve ikinci ekranda

Gerekli bilgileri girin, mesela rapor ismi, tanimi, vs. Sonra bir dizine tiklayin (bu gerekli, ana dizine kayda izin verilmiyor) gecici olarak Steel Wheels altinda kaydedebilirsiniz, ya da dizin ikonlarinin en sagindaki (ustunde gunes isareti olan) ikon ile kendi dizininizi yaratabilirsiniz.

Burada diger onemli bilgi Publish Password kismi, bu sifre bu yazinin basinda belirttigimiz XML dosyasi icine yazdigimiz sifre, yani bizim ornekte "admin" kelimesi. Simdi OK deyin, ve yayin isi bitecek.

Bu raporu ayrica kendi sabit diskinize de kaydedebilirsiniz, bu tipik dosya kaydi, Save ile oluyor. Simdi Pentaho web konsoluna gidelim, ve sol ust kosedeki dosya acma ikonuna tiklayalim,


Steel Wheels'e girelim, orada Dell Store raporunun listelendigini goreceksiniz. Rapora tiklayin ve sonucun listelendigini gorun. Output Type secenegi ile XLS, PDF dosyalari urettirebilirsiniz.

Kaynaklar
http://www.robertomarchetto.com/www/how_to_use_pentaho_report_designer_tutorial

http://www.youtube.com/watch?v=qo1BQfgDV-Y




![](1.png)
![](2.png)
![](Screenshot%2Bat%2B2012-04-06%2B11%253A48%253A00.png)
![](Screenshot%2Bat%2B2012-04-06%2B11%253A53%253A34.png)
![](Screenshot%2Bat%2B2012-04-06%2B12%253A05%253A11.png)
![](Screenshot%2Bat%2B2012-04-06%2B12%253A13%253A31.png)
![](Screenshot%2Bat%2B2012-04-06%2B12%253A16%253A20.png)
![](Screenshot%2Bat%2B2012-04-06%2B12%253A18%253A00.png)
![](Screenshot%2Bat%2B2012-04-06%2B12%253A21%253A23.png)
![](Screenshot%2Bat%2B2012-04-06%2B12%253A26%253A04.png)
![](Screenshot%2Bat%2B2012-04-06%2B12%253A33%253A06.png)
