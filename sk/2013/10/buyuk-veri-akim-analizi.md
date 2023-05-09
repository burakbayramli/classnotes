# Buyuk Veri, Akim Analizi

Buyuk Veri dunyasinda durum nedir? Tabii buyuk veri deyince Hadoop'tan
bahsetmemek olmaz, once buraya nasil geldigimize bakalim.

Hadoop'un en onde gelen kullanim alanlarindan biri log dosyasi
islemek[ti]. Ozellikle populer olan Web siteleri asiri seviyelerde log
dosyasi uretiyordu (her kullanici tiklamasi), ama Hadoop oncesinde bu
veriyi basit / olcekli  bir sekilde isleyebilecek bir ortam yoktu. Log
analizi yapmak isteyenler koca koca Sun, Oracle makinalari alip
bunlara veri yuklemeye ugrasiyorlardi, ozel kodlar, ozel paketlerle bu
veriler incelenmeye ugrasiliyordu. 2002'de boyle bir projede bizzat
calistik, SQL ile "buyuk veri" analizi yapmak hakikaten zor bir isti,
bu tur veri ambari raporlamasinda uzman olan bir danismana saatte
yuzlerce dolarlar verildigini hatirliyorum.

Google'in Hadoop'un temelini olusturan esle/indirge (map/reduce)
mimarisini aciklamasindan, ve Hadoop'un ortaya cikmasindan sonra isler
degisti. Insanlar log dosyalarini direk, oldugu gibi dosya halinde
alip Hadoop kumesine atabilmeye basladilar (HDFS buyuk dosyalari parca
parca makinalara boler, ama disari tek bir dosyaymis gibi
gosterir). Analiz gerektiginde paralel sekilde isleyecek esle /
indirge sureclerini kodladilar, her yerde bulanabilecek PC
makinalarini birlestirip 100, 200 hatta 700 makinalik kumelerde devasa
verilerini nisbeten ucuz sekilde islemeye basladilar. Sirketler
cogunlukla kendi Hadoop kumelerini de kendileri kurdular.

Bugun Hadoop servisini bulut servisi (cloud service) olarak sunmaya
ugrasan pek cok sirket var tabii, ama halen sirketler kendi kumelerini
kuruyorlar.

Mimariye gelelim: Esle/indirge mimarisi veri satirlarina teker teker
bakan / isleyen, ve veriyi bir akim (stream) gibi goren
mimaridir. Bugunlerde veri akimi islemi (stream processing) surekli
anlik (realtime) baglamda telafuz ediliyor, fakat aslinda Hadoop ta
akim analizi yapiyor, sadece bunu cevrimdisi (offline) olarak
yapiyor. Yani onlarca, binlerce veri satirinin hafizada olacagi
farzedilmiyor, one gelen o tek satir, o tek veri noktasi uzerinde
islem yapiliyor (ve ona tekabul eden, bir veya daha fazla "sonuc
satiri" uretiliyor, bu da bir cikis akimi olusturur, baska bir
islemciye gonderilebilir, vs). Dolayli olarak bu kulturde hafizada az
konum verisi tutmak ana amactir, ya da ne kadar az konum verisi olursa
o kadar iyi olacagi dusunulmektedir [1]. Fena bir yaklasim degildir.

Simdi pek cok veri analizinin, hatta yapay ogrenim (machine learning)
algoritmasinin bu sekilde kodlanabilecegi birdenbire
"kesfedilmistir". Hatta bu tur algoritmalara bugunlerde "utandiracak
boyutlarda paralelize edilebilen" adi veriliyor, mesela KMeans
kumeleme metotu bunlardan biri. Yani paralelize edilmesi o kadar, o
kadar kolay ki, simdiye kadar niye yapmadik turunden bir serzeniste
var bu soylemin icinde. Tabii altyapi (framework) mevcudiyeti cok
onemliydi.. Hadoop'un bazi isleri kolaylastirmasi, bir suru
algoritmanin bu altyapiya gecirilebilecegini farketmesi yonunde
insanlari tesvik etti. 

Ve bugune geliyoruz: veri analizini akim islemesi (stream processing)
olarak gormeye alisilinca, "niye bu isi habire diske yazipi diskten
okuyan sekilde yapalim ki?" dusuncesi basladi, cunku Hadoop isini
gormek icin cok fazla gecici dosya uretiyor, bu da performansi
dusuruyordu... Iste son zamanlarda etrafa sacilan anlik akim veri
analizi (realtime stream processing) urunleri, paketleri bu ihtiyaci
tatmin etmeye ugrasiyor. Storm, S4, Samza bu tur urunler.

Not: Zaten devasa boyutlarda veriyi baska sekilde (ucuz olarak)
islemek mumkun degildir. Terabayt olceginde bir verinin tamamini
hafizaya alip, direk veri erisimi yapabilecegimizi farzedemeyiz. Bu
tur makinalar insa edilebilir muhakkak, fakat sabit disk RAM'den ucuz
oldugu icin buyuk diskli az hafizali makinalar daha ekonomik.





