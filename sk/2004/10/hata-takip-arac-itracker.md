# Hata Takip Aracı - ITracker

Java projemiz için hata takip programı gerektiğinde, İnternet'te ufak
bir arama yaptık. XP projelerini idarede kullandığımız XPlanners
tecrübesi, bizi tekrar aynı şekilde Struts ve J2EE tabanlı bir
uygulama aramaya itti. J2EE bazlı programların kurulumu oldukça kolay
oluyor, ve envai türden işletim sistemi ve veri tabanı ile
çalışabiliyorlar.  Bu yüzden ITracker adlı programı denemeye karar
verdik.  ITracker ITracker'ı kurmak için, itracker_xxx.noaxis.bin.zip
şeklinde olan dosyayı indirin. Bu zip içinde web tabanlı çalışabilen
işlerkodları bulabilirsiniz. Öteki listelenen dosyalar başlangıçta
lazım olmayacaktır.

Mesela içinde axis kelimesi geçen zip, ITracker'ı bir Web Servisi
olarak işletiyor ve öteki programlar ile XML üzerinden iletişim
kurmasını sağlıyor! Başlangıçta çok fazla. (İleride kurcalamak
isterseniz aklınızda olsun).  Bu yazımızda ITracker'ı, JBoss ve
PostgreSQL üzerinde kurmayı tarif edeceğiz.  Kurmak JBoss ve
PostgreSql kurulduktan sonra, aynı makinada birden fazla JBoss servisi
işletip işletmeyeceğinize karar verin. Makina kıtlığı çeken projeler
genelde hata takip programlarını, proje idare araçlarını ve
uygulamalarının test edildiği servisi hep aynı makina üzerine
koyabiliyorlar.

Eğer projeniz JBoss üzerinde geliştirilen/test edilen bir uygulama
ise, ve aynı makinada ITracker ve sizin uygulamanız aynı anda
çalışacaksa, iki JBoss arasında port çakışması yaşanacaktır!  Evet,
aynı JBoss içine birden fazla EAR atabilirsiniz, ama bir uygulamayı
iptal ettiğiniz anda, öteki uygulamada duracaktır. En iyisi, iki
uygulamayı tamamiyle birbirinden izole edin. Ayrı JBoss'lar.  Aynı
makinadaki PostGreSql veri tabanı, herhalde başka uygulamalar
tarafından da kullanılıyordur. ITracker için yeni bir veri tabanı
yaratırsak, idaresi daha rahat olur. Bu veri tabanının ismi itracker
olsun.  createdb itracker Diğer yazımızda tarif edilen admin
kullanıcısı, bu veri tabanı için de geçerli olacak.  Şimdi,

```
ITRACKERDIZINI\sql\postgres\install\create_itracker_core.sql
```

adlı dosyayı, psql itracker ile komut satırına girdikten sonra \i
create_itracker_core.sql kullanarak işletin.  Güzel. Şimdi JBoss
dizininize girin, ve ITracker için gerekli olan veri kaynak dosyasını
yaratın. `itracker-ds.xml` adını verebileceğiniz bu dosyayı
`JBOSSDIZINI/server/default/deploy` altına bırakın. İçeriği şöyle
olabilir (jndi-name mutlaka ITrackerDS olmalı)

```
ITrackerDS jdbc:postgresql://localhost:5432/itrackerorg.postgresql.Driver
admin
```

Şimdi, `JBOSSDIZINI/server/default/conf/standardjbosscmp-jdbc.xml`
dosyasını açın.

Default datasource'u değiştirmeniz gerekecek. DefaultDB kelimesi
yerine şöyle olsun: `... java:/ITrackerDS...`  En son olarak
`ITRACKER/docs/itrackerApplication.properties` dosyasını
`JBOSSDIZINI/server/default/conf` altına koyarsanız, herşey
tamamlanacak.

JBoss'u başlattıktan sonra, `http://makinaismi:port/itracker`
... adresinden programı kullanmaya başlayabilirsiniz. Program otomatik
olarak ilk kullanıcıyı yaratıyor, kullanıcı: admin, şifre: admin ile
programa girip projeniz için gerekli bilgileri girmeye
başlayabilirsiniz.  ITracker resmi sürümü, blogumuz yardımlarıyla
Türkçeleştirilmiştir durumdadır. Kullanıcı olarak sisteme girdikten
sonra, "Tercihlerim" (My Preferences) seçeneği altından Türkçe'yi
seçerek uygulamanın dilini tamamen değiştirmeniz mümkün.

![](itracker.gif)


