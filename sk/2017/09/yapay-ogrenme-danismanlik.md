# Yapay Ogrenme, Danismanlik

Google bugunlerde hizli bir sekilde danismanlik tarafini gelistiriyor,
G. teknolojilerini kullanan danismanlar, aynen IT durumunda oldugu
gibi, sirketlere gidip YO servisleri sunuyorlar, bu is daha da
gelisecek. Google bilindigi gibi derin ogrenme problemlerini cozmek
icin TensorFlow'u gelistirdi, bu urun acik yazilim, boylece
gelistirici kendini bir sirkete  "baglanmis" hissetmiyor, istedigi
zaman kendisi istedigi sekilde kodunu isletebilir, ama Google TF
kodlarini paralel, buyuk olcekte isletmek icin bulut servisi
hazirlamis, bu sekilde musteri cekiyor. TF kodunu buluta verin,
gerekirse 1000 makina uzerinde isletsin. Google TF icin ozel bir cip
bile gelistirdi, CPU yerine TPU (tensor processing unit). Bu cipler su
anda kendi bulut servisi makinalarinda, ama Android telefonlarinda da
gorulmeye baslanir yakinda.

Google danismanlik servislerinde neredeyse her YO problemi icin derin
ogrenme kullaniliyor bu arada (SVM, karar agaci vs modasi gecti),
optimizasyon gerekirse takviyeli ogrenme (DO bu yaklasimlarin bir
parcasi), zaman serileri (siniflama, gelecek tahmini -regresyon-) icin
LSTM, ki bu da geriye yayilma (backprop) ile egitilen bir tur yapay
sinir agi yaklasimi, ve derin sekilde genisletmek mumkun.

Peki YO sirketlerin ne isine yarar? Yapay sinir aglari bilindigi
evrensel yaklasiklayici (universal approximator), herhangi bir
foksiyon `f(\theta)` yi sadece girdi ve ciktilarina bakarak yaklasik
sekilde temsil edebiliyor. Bir anlamda geriye muhendislik yapiyor. Ek
olarak derin ogrenme ile ne kadar cok cok veri varsa bu geriye
muhendislik o kadar iyi oluyor.

Tipik bir sirketin her tarafi fonksiyonlarla dolu; mesela
"musterilerim sadik degil vs sayida musteri beni her ay
terkediyor". Burada bir fonksiyon var, musteri kisisel, aksiyon
verisini gir, fonksiyon "terkedecek" diye evet / hayir olasiligi
versin. Bu fonksiyon tabii ki bilinmiyor, eger sosyoloji, psikoloij
alaninda uzman matematiksel modelciye sorsak, belki bu `f(\theta)` yi
bulurlar, ama o kadar modelci bulunamaz, zaten bilim o kadar
ilerlememis olabilir. Daha hizli sonuc girdi / ciktilari verip geriye
muhendislik, YSA ile f'i bulmak (musteri sadakati icin su arkadas
aynen bunu yapmis). Yaklasim ayrica esnek, musteri sadakati f bulmak
icin kullanilan benzer metot, is makinasinin ne zaman bozulacagini
veren g hesabi icin de kullanilabilir.

Eger f bilinirse sadik olmayacak musteri onceden tahmin edilebilir,
onlara donuk kampanya yapilir, o musteri kaybedilmemis olur. 

Yani YO danismanliginin gelecegi cok iyi. Su siralar TensorFlow bilen
YO programcilari icin yogun talep var. O zaman tavsiye 1) Python
ogrenmeli, hesapsal programlar, veri bilim icin unlu (Matlab'in isi
bitti, zaten ticari, gelistiriciler icin bu kisitlayici bir faktor ve
o yuzden sevilmiyor ) 2) TensorFlow ile yapay ogrenme saglikli bir
sekilde buyuyor.

Su an pur IT danismanligi veren sirketler, YO muhendisleri ile servis
yelpazelerini genisletebilirler. Programcilar TensorFlow ile bilgi
dagarcigini gelistirebilir.

Not: NVidia GPU kartlari uzerinden derin ogrenmeye bir giris yapmisti,
ve gelistiricileri kendine cekme alaninda etkili olmaya cabaliyor,
fakat buyuk bir ihtimalle Amazon, Google onlari golgede
birakacak. NVidia'nin bulut servisleri yok, sadece grafik karti
donanimi ile bilinen bir sirket. Derin YSA altyapi / kodlama ortami
secimi onemli cunku hem saglam hem yaygin teknolojiyi kullanmak o
teknolojiyi bilen programci bulabilmek, o programcinin is bulabilmesi,
programci sorun yasadiginda forumlarda sorusuna cevap verecek
kisilerin mevcudiyeti, egitim materyellerinin yayginligina kadar her
seye etki ediyor.






