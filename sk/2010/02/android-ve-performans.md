# Android ve Performans

Android uygulamalarinda performans konusuna dikkat etmek gerekiyor;
her ne kadar cep telefonlari akilli tel (smart phone) baglaminda daha
kuvvetli hale gelmis olsalar da, yine de islemci hizi, hafiza
kapasitesi halen bir dizustu / masaustu N cekirdek iceren
islemcinizin, 1 GB+ bellekli makinanizin ustune cikamaz. Optimize
edilmemis kodlarda dizustunde saniyede isleyen kodlarin telefon
uzerinde dakikalar alabildigini gorduk.Optimizasyon icin dikkat
edilmesi gereken konular:Gereginden fazla obje yaratmayin: Gecici
nesne yaratmak yerine, mevcut hafiza uzerinde islem yapmak en
iyisi. String bolmek icin split() cagrisi yerine mevcut String
uzerinde gezen StringTokenizer daha iyi mesela.String islemleri:
String'den double, int tiplerine cevirilerde Integer gibi class'larin
new ile yaratip intValue vs gibi cagrilar yerine, arka planda native C
kodlarini cagiran parseInt, toString gibi static cagrilari
kullanin.Eger yazilim yapiniz, mimariniz o tur bir esneklik
gerektirmiyorsa, tip referanslari yaratilan objenin kendi tipinde
olsun, bir ust sinif degil:

Mesela Map m = new HashMap() kullanimi yerine HashMap m = new
HashMap(), boylece vtable ziplamasindan kurtulunabilir. Ilk gosterilen
uygulama nesnesel programcilikta bir nevi aliskanlik haline
geldi.Cetrefil veri yapilarini onceden hazirlayin: Eger programiniz
mesela duz dosyalardan cetrefil bir obje yapisi kuracaksa, ve bu yapi
referans verisi olarak degismeyecekse, bu hazirlik isini gelistirme
bilgisayarinizda yapip ObjectOutputStream uzerinden kendi res/raw
dizinize geri yazabilirsiniz, ve boylece serialize olmus obje yapiniz
telefonunuza uygulama kodlari ile beraber gonderebilirsiniz. Artik
Activity icinden bu obje yapisini openRawResource ve ObjectInputStream
ile yuklemek mumkun, hazir pismis bir nesne yapisi kullanim icin
bekliyor olur.for ([Tip] x : liste ) { .. .} turu kullanimi iterator
objesi yaratip yokediyor, ayrica "liste" cagrisi bir fonksiyon cagrisi
ise birkac kez cagrilma ihtimali var; sonuc olarak yaptigindan emin
olmadigimiz bir kullanim yerine, klasik for dongusu icinde sayi bazli
indeks kullanip listelere get(i) gibi erismek bellek kullanimi
acisindan daha iyi olur. Sonucta klasik for tipi kullanimin bellegi
nasil kullandigini cok iyi biliyoruz.

Bazen, ustte bahsedildigi gibi, bellekte HashMap bazli nesneleri hazir
etmis olsak bile, eger mevcut bellegin sinirina yaklasmissak, cop
toplayici fazla is yapmaya baslayacak, bu da programin hizli
isleyisini engelleyecektir. Bu gibi durumlarda, buyuk veri setlerini
islemek icin veriyi bir sqlite veri tabanina atip, indeksleri dogru
sekilde koyup, hash kullanim yerine PRIMARY KEY ama disk temelli bir
veri erisim stratejisi daha iyi olabilir. Hatta, bellegin sinirlarini
zorlamak yerine sqlite temelli islemin daha hizli isledigini bile
gozlemleyebilirsiniz. Bu veri tabani oldukca hizli bir
taban. Indeksler dogru yerlerde yaratilirsa, sorgular cok cetrefil
olmadigi surece ustune atilan her turlu isi yapacaktir.Kaynak





