# Dongu Yazmamak, Fonksiyonel Diller, Python


Dongu Yazmamak, Fonksiyonel Diller, Python




Buyuk miktarda veri islerken "for" komutunu kullanan dongulerden kacinmak iyi olur. Python'un fonksiyonel formati ve kutuphanelerin sagladigi hizmet zaten daha kisa / temiz sekilde listeler, numpy vektorleri, matrisleri uzerinde islem yapilmasini saglar. Ama daha onemlisi, bu tek cagrilik fonksiyonel kullanimlarin arka planda C ile isleyen kodlara direk gitmesidir. Yani hizli isleyeceklerdir.

Mesela elimizde bir kelime listesi olsun, bu listenin her elemaninin her diger elemanina olan Levenshtein uzakligini hesaplayip bir matrise yazacagiz. Ilk yaklasim hemen


for w1 in words:

   for  w2 in words:

diye bir dongu yazar. Bu cok yavas isler cunku dongulerin kendisi Python icindedir. Daha iyisi itertools.product(words, words) ile kelimelerin tum kombinasyonunu hesaplatmaktir. Avantaj bir, itertools.product C ile kodlanmis ve hizli. Iki, geriye dondurulen bir oge gezici (iterator) ve her istek icin tek eleman uretiyor, tum hafizayi tum sonuc ile bir anda doldurmuyor.

Simdi bu kombinasyon uzerinde uzaklik hesabini itertools.imap(f, ...)  ile isletiriz. f fonksiyonu

f = lambda (x,y): leven.distance(x,y)

olarak tanimlanir, distance daha once bahsettigimiz mesafe hesabi, o da C ile isleyen bir kodda. Bakis acisindaki degisime dikkat: dongu her seyi kontrol etmiyor, imap fonksiyonu, dongu ve veriyi "eslestiriyor", birini alip otekine uyguluyor.  Bu isleyince  elimizde mesafeler var, ve oge gezici uzerinden bu hesaplar isteyene veriliyor. Peki geziciden Numpy matrisi nasil olustururuz? np.fromiter ile.

words = np.array(['filan', 'fisman', 'sisman', 'paspas']) 
(dim,) = words.shape
f = lambda (x,y): leven.distance(x,y)
res=np.fromiter(itertools.imap(f, itertools.product(words, words)),
                dtype=np.uint8)
A = np.reshape(res,(dim,dim))

Koda tekrar bakarsak, product, imap, fromiter ve leven.distance cagrilarinin hepsi C icinde isliyor. Yani hesap oldukca hizli olacak. Genel olarak kodlama felsefesi de soyle (degismeye basladi). Python, Ruby gibi diller fonksiyonel kodlamaya izin veren, onu ozendiren diller, dongunun veri isledigi degil, fonksiyonlarin dongulere parametre olarak verildigi bir yaklasim bu. Aslina bakilirsa modularite acisindan mantikli. Donguler de cogunlukla birbirine benzeyen ve bir kere kodlayip bir daha kodlamak istemeyecegiz seyler.

Ayrica performans acisindan da veri analiz kodlarini hep "C icinde tutmak" icin, ustte gordugumuz gibi, fonksiyonel tarz daha one cikiyor. 

Not: Gezicilere bir daha deginelim. Ustteki cagri zincirinde surekli gezicilerin dondurdugu tek eleman uzerinde islem yapiliyor, ve gezicilerin kodlari C icinde. Eger tek bir gezici, ufak veri olsaydi gezici kullanimi pek bir fark yaratmayabilirdi. Ama onbinlerce ogeli bir vektor uzerinde arka akraya bir suru islem uyguluyor olsaydik, gezici olmadigi durumda her basamakta 10,000 uyeli bir vektor daha yaratiyor olurduk. Bu hem hafiza hem CPU israfi demek olurdu. 





