# OpenJPA ve Slice


OpenJPA ve Slice



Hibernate Shards'i su anda kontrol ediyorum. Kullanim sekli olarak EJB3 ortamina uyumlu bir kullanimi olmadigi gozukuyor.. Bu isleri biraz zorlastiracak ve acikcasi tercihim degil. Boyle bir paketi test etmeye basladigimda (evaluate), aklimda bekledigim, olmasi gereken bir kullanma sekli vardir; Eger Hibernate uzerinden sharding yapacaksa bir sistem, diyelim, o zaman persistence.xml uzerinde yapacagim bir kac ayar degisikligi ile verinin DB'lere dagilmaya basladigini gormek isterim. Shards'da bunu goremiyorum. SessionFactory, bilmemne uzerinde bir takim hareketler yapmamiz bekleniyor.Simdi rakiplere bakmaya basladim: Mesela javax.persistence JPA standartina uyumlu OpenJPA paketini gordum. Bu paketin kullanimi ustte tarif ettigim gibi. Buraya nasil geldim? Slice denen Hibernate Shards'dan ilham almis bir paket uzerinden.Arkadas Slice paketini OpenJPA uzerinden yazmis, eger iyi bir yatay olcekleme paketi ise o zaman sirf Slice icin OpenJPA'ye gecebilirim. Sonucta standartlar bunun icin degil midir?Testler suruyor.Bazi ek konular: Veriyi yatay bolmeye basladiginiz zaman, mimarinizin bunu yansitmasi lazim. Diyelim ki Facebook'u kodluyorsunuz, User class'inin arkadas listesinde User objeleri var. Adam kendi kendine referans ediyor yani (biz buna basketboldaki hareketten hareketle 'hook shot' derdik). Fakat bu durumda User XXX bir database uzerinde, User YYY baska bir database uzerinde olacak ve arkadas listesi alirken iki database uzerinden JOIN yapmaniz gerekecek (daha dogrusu JPA'in yapmasi gerekecek). Bu tur "attraksiyon" hareketlere simdilik izin yok.Yani, iki shard arasinda iliskiye izin yok. Aslinda olmamasi daha iyi belki de.. Sonuc: Eger User class'inin arkadasi olacaksa, onun arkadaslari Friend gibi degisik bir class'ta yani baska bir tabloda olmali. Ayni User kisisi farkli Friend satirlarinda denormalize olarak olabilmeli, sonuc olarak farkli shard/db uzerine yazilabilmeli.Ozet olarak uzerinden veri dagitimi yaptiginiz veriyle, ana tabloyla iliskide olan her "yan veri", "yan tablo" tamamen o oteki DB'ye gidebilmeli.Facebook'un veri yapisinin da bu sekilde olduguna emin olabilirsiniz. Aynen App Server dunyasindaki "yapiskan oturum (sticky session)" gibi, bir DB'ye veri icin gittiginizde o operasyon surecinde o DB'ye yapismalisiniz.




