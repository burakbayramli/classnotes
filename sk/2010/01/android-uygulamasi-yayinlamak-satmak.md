# Android Uygulamasi Yayinlamak, Satmak


Android Uygulamasi Yayinlamak, Satmak



Oncelikle Turkiye'den satmak hakkinda bazi bilgiler: Uygulamalarinizin satilacagi Android Market birkac ulke haricinde pek cok diger yabanci ulkeye kapali. Google bunun vergilendirme politikalari ile alakali oldugunu soyluyor, ileride yeni ulkeler eklenecegini soyluyor.. AdSense durumunda bu isi halletmislerdi, ama demek ki farkliliklar var (GA ile TR dahil olmak uzere bankalara direk odeme gonderebiliyorlardi). Neyse, Turkiye'den satis yapmak icin Android Market'e simdilik uye olmaya gerek yok, boylece uyelik icin gereken $25'i bosuna odemekten kurtulursunuz. Ama programinizi And. Market uzerinden bedava satmak istiyorsaniz, o zaman hala uyelik lazim. Alternatif satis kanallari SlideME gibi siteler olabilir, biz bizim uygulamayi buraya koyduk. Pocketbudda bir mobil uygulama olarak hayatina devam edecek.Bedava programdan para kazanmak yani reklam kullanmak isteyenler icin AdMob var, hatta statik icerik gosterilecegi durumlarda AdSense reklamlari tasiyan olan statik bir Internet sayfasina yonlendirme yapmak mumkun, fakat her iki durumda da bir Internet baglantisi olmasi gerekiyor. Bu secenek bize pek cazip gelmedi. Simdilik SlideME gibi alternatif piyasalar en iyisi.Simdi uygulama yayinlamanin teknik tarafina gelelim.Once bir anahtar yaratmak lazim:keytool -genkey -v -keystore my-release-key.keystore -alias alias_name -keyalg RSA -validity 10000Komut sonras my-release-key.keystore adli bir dosyada anahtar uretilmis olacak. Sonra, surume hazir bir apk dosyasi yaratmak lazim. Bunun icin Ant ile calisanlar "ant release" komutunu uygular. Boylece [UYGULAMA]-unsigned.apk adli bir dosya ./bin dizininde yaratilmis olur. Sonra bu dosyayi "imzalamak" gerekli. Bunun icinjarsigner -verbose -keystore [KEYSTORE DOSYASI] [BIN DIZINI]/[UYGULAMA]-unsigned.apk alias_nameBu komut apk dosyasini "oldugu yerde" degistirerek surume hazir hale getirir. Bu noktada dosya ismi yaniltici olabilir, bir mv komutu ile bu dosyayi [UYGULAMA].apk haline getirebilirsiniz.Bu kadar. Apk dosyasi bu noktada And. M. ya da SlideME uzerinden yayinlanmaya hazirdir.Dikkat: jarsigner ve keytool programlarini direk JDK/bin altindan geldigine emin olun. Yoksa acaip sonuclar ortaya cikabilir.




