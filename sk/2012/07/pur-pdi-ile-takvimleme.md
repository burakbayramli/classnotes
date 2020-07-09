# Pur PDI ile Takvimleme




Pur PDI ile Takvimleme




Genellikle takvimleme (scheduling) islemleri cron veya benzeri bir dis program tarafindan yapilir. Ama bir teknoloji egzersizi olarak takvimlemenin tamamini da Pentaho Kettle, PDI ile yapsak olur muydu?



Ustteki gibi bir islem (job) yapisi bunu halledebilir. Basladiktan sonra bir Shell isletiyoruz, icinde Unix cagrisi "date +%H%M > /tmp/sysdate" yapiliyor. Bu cagri mesela saat 14:20 icin 1420 sayilarini /tmp/sysdate icine yazacak. Bir dosya daha var, bu dosya takvimleme dosyamiz, hangi saat ve dakikayi bekledigimizi buraya yazacagiz. Diyelim ki 15:00 icin 1500 ve bu dosya ismi /var/takvim. Bu dosyayi onceden hazir ediyoruz. 

O zaman beklenen ana gelip gelmedigimiz basit bir dosya karsilastirmasindan ibaret olacak. Bunun icin de bir Kettle ogesi var, File Compare. Iki dosya ismi veriliyor, bu ornekte /tmp/sysdate ve /var/takvim ve eger dosyalar esit ise yesil ok takip ediliyor, degil ise kirmizi cizgi. Bizim takvimleme islemi icin kirmizi cizgi "beklemeye devam et" anlamina gelir, beklemek icin de bir oge var zaten, Wait For. Bu kutuya ne kadar beklenecegini soyleyebiliriz, mesela 5 saniye, ve ardindan islemi tekrar basa postalariz (yani Shell cagrisina) boylece en son tarihi tekrar aliriz, sonra onu kontrol ederiz... boyle devam ederiz. Islemin sonunda gorulen Success noktasi yerine takvim aninda calistirmak istedigimiz esas islemler olabilir, mesela bir yerlere mail gondermek, bir tablo okumak, vs gibi.





![](Screenshotat2012-07-31164658.png)
