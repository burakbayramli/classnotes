# GPSTk, GPS Uydularinin Yeri

Uydu Ile Yer Bulma

[Bu bolume ekler olabilir].

GPS sistemi alicinin (cep telefonu) yerine bulmak icin GPS uydularini
kullanir. Bu uydularin yeri bilinir - alicimiz acildiginda bu bilgi
uydulardan alinir (almanac denen veri), ve ardindan sinyal zaman
farklari uzerinden alicinin uyduya olan mesafesi hesaplanir
(pseudorange), ve en az 4 uydu mesafesi ve uydularin kesin yerleri
uzerinden alicinin yeri hesaplanabilir.

Fakat GPS Test gibi app'lerde bazen goruyoruz ki 10-12 uydu gorulmeden
yer hesabi yapilmiyor. Burada ilerleme yapmak isteyen maceraci (!)
arkadaslar icin alttakileri paylasalim; eger alici/uydu mesafesi
alinabilirse (Android Nougat, API 24'ten itibaren bu ham mesafeyi
veriyor) bu hesap daha hizli bir sekilde yapilabilir belki.

Her gun / saat GPS uydularinin nerede olacaginin tahmini alttaki
sitede ham veri olarak paylasiliyor.

http://www.linz.govt.nz/data/geodetic-services/positionz/rinex-data-archive

(Tabii bu durumda bu veri her gun indirilecek, ama bir muhendislik
al/ver durumu, belki gunun ufak bir kisminda kisa bir Internet
baglantisi uzerinden uydu yer tahminleri alinir, sonra tum gun sadece
uydu verisi ile yere bulunabilir).

Veri RINEX formatinda. 5 Aralik 2016 icin navigasyon dosyasi

https://apps.linz.govt.nz/ftp/positionz/2016/339/auto3390.16n.Z

5 Aralik 339. gun, 1 gunun tum verisi icin 0, n navigasyon demek.

Bu veriyi isleyebilen ve pek cok faydali hesap yapabilen bir proje

https://github.com/SGL-UT/GPSTk

Acilinca dizine girilir

mkdir build

cd build

cmake ..

make

sudo make install

Simdi bir ornek derlemek icin examples altina girilir,

g++ example3.cpp -I/usr/local/include/ -L/usr/local/lib -lgpstk 

Nasil isletilecegi icin GH'a bakilabilir.

Bir de Python bazli bazi kodlar var,

https://github.com/scienceopen/gpstk-examples-python

Fakat bu kod icin derleme asiri hafiza gerektiriyor. Script'lerde bir
bozukluk olmus herhalde. Maceracilar buraya da dalabilir.

Bu uydu hesaplarinin matematigi hakkinda kaynak ararken tanidik bir isim gorduk, Lineer Cebir'in efsanevi hocasi Gilbert Strang meger bu alanda bir kitap yazmis. Kitapta surekli referans edilen Matlab kodlari surada.

Not:

"Fakat Android N piyada gorulen tum telefonlarda yok"; bunun cevabi
biraz daha macera - rivayete gore Cynogen Mod 14, mesela Samsung
Galaxy SIII icin bile Nougat kurulmasini sagliyor. CM ve benzeri
ROM'lar "alternatif Android" kurulumlari sunarlar; burada flashlama
(flashing) gibi kavramlar var; Bu arkadaslar Android'i kaynaklardan
tekrar derleyip, gerekeni uyarlayip istemedikleri seyleri disari
atarak temiz bir Android surumu olusturuyorlar. Acik kaynagin
yararlari.






