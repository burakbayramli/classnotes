# Hibernate Shards


Hibernate Shards



MySQL'i olceklemenin yontemlerinden biri master/slave kurulusudur. Bu kurulusta, tum veri yazma islemleri master'a yapilir ve bu yazim operasyonlari master'dan slave'lere belli araliklara replicate edilir. Tum okumalari slave'lere gider. MySQL replicate isini oldukca hizli yapmaktadir, bu sebeple fena bir cozum degildir. Eger uygulamaniz 'cogunlukla okuyan (read-mostly)' turunden bir uygulama ise (youtube gibi), master'in uzerindeki yuk fazla olmaz.Yukun cogu okunan slave MySQL veri tabanlarina gidecektir.Bir diger yontem sudur: Verinin icerigine bakarak, hem yazim hem okuma operasyonlarinin ikisinin de belli/tek (unique) bir MySQL nod'una yonlendirmek. Mesela ilk isminin ilk harfi A-F arasindaki musteri kayitlari Nod#1'e, ilk isminin ilk harfi G-R arasindakiler Nod#2'ye gitsin demek gibi.Bu stratejiye 'yatay olcekleme' stratejisi deniyor, cunku herhangi iki MySQL nod'u arasinda bir hiyerarsi iliskisi yok. Herkes esit.Bu strateji IT dunyasinda oldukca eskidir. ABD'de bir arkadasim bile calistigimiz danismanlik sirketinde (ki danismanlik sirketleri genelde framework islerine girmezler) boyle bir SQL paketi yazmisti - SQL komutlari ile DB arasina oturan bir katman idi bu, SQL icindeki veriye bakiyor, ve elindeki DB baglantilarindan ona gore hangisini kullanmasi gerektigini biliyordu.Bugun bu ara katman Java dunyasi icin Hibernate'dir. Eh, Hibernate hem verimizi, veri komutlarimizi idare ederken bu tur yatay veri bazli dagitim (sharding) yapsa iyi olmaz mi?Yapmaya baslamis. Hibernate Shards projesi, bunu amacliyor. Sistemi ayarladiktan sonra kodunuzun geri kalani hala bilinen Hibernate operasyonlarini kullanmaya devam ediyor,  Hibernate Shards arka planda DB islemini veriye bakarak gereken makinaya yonlendiriyor.Bu konu hakkinda ek yazilar gelecek.




