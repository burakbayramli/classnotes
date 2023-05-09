# Oyun Programini Oynayan Program - FPS Play

Robot konularinda teorilerin test edilmesi icin simulatorler faydali
araclar, grafikleme teknikleri cok ilerledi ve oyun icindeki dunya dis
dunyaya benzemeye basladi. Robotlari kontrol eden kodlar bu tur sanal
ortamlarda test edilmeye baslandi.

Bogazici CmpE 565 dersinde mesela Unreal Tournament adindaki 1SG
(Birinci Sahis Gozunden -FPS-) oyunu uzerine kurulmus USARSim yapisini
odevde kullanmistik, bu ortamda disaridan yazilimlarin baglanip 3D
oyun ortamindaki sanal robotlarin oyun icindeki kontrolu mumkun
oluyor. USARSim'de oyun icindeki fiziksel dunya yapisi, yollar,
engeller, agaclar, vs, goruntu, uzaklik bilgileri sanal algilayicilar
uzerinden disari aktarilabilmekteydi. Bu bilgiyi ve kontrolu saglamak
aslinda 1SG oyunlari icin kolay, grafikleme amaclari icin zaten o
bilgiye sahipler, sadece bir arayuz ile disari aktarmalari
gerekiyor. UT ve USARSim bu arayuzu sagliyor.

Ilginc ek bir yontem su olabilir: Bir 1SG oyununu masaustumuzde
ufaltilmis bir pencerede kosturdugumuz dusunelim. Eger masaustumuzun
ekran goruntusunu (screenshot) alabilirsek, ki bu kolay bir istir, o
zaman oyun penceresinde olanlarin grafigini alabiliriz. Oyuncuyu
kontrol etmek icin ise oyuna aynen gercek bir oyuncunun yapacagi gibi
tus, ve mouse tiklamalari, hareketlerini yollayabiliriz.

Bu fikirden hareketle fps-play adli bir proje baslattik. Test icin
kullandigimiz oyun populer Quake grafik motorunu kullanan bedava
UrbanTerror 1SG oyunudur. Oyun pencerede baslayinca Python bazli
kodlar gtk.gdk.Pixbuf kullanarak surekli olarak ekranin belli bir
bolumunun "fotografini cekiyor", ve bir dongu icinde bu fotograflar
oyuncunun bakisini disari aktariyor. Bu ardi ardina fotograflar OpenCV
kutuphanesi ile imaj isleme algoritmalarina tabi tutularak, imaj ve
arkasindan yapay zeka hesaplarina tabi tutalabilir, etrafinin
haritasini cikarmak, diger oyunculari tanimak, vs. gibi. Bunlardan
sonra oyuna tus, mouse bilgisi gondermek icin xdotool Unix
kutuphanesini kullaniyoruz. Bu arac disaridan herhangi bir programa
sanki gercek bir kisi gibi tus tiklamalari, hatta mouse x,y hareketi
ve tiklamasi gonderebiliyor.

Su anda Github'da bulunan kodlar UrbanTerror oyununu OpenCV ekraninda
gosterip, aksiyon olarak oyuncuyu biraz ileri hareket ettirip,
durdurup, sonra bir el ates ettiriyor.

Bunun ustune neler eklenebilir?

Mesela optik akis (optical flow) kavrami Kalman Filtreleri ile
birlestirilebilir; etraftaki objelerin (yer, duvarlar, engeller)
etrafinda dolastikca onlarin yerlerini haritamiza
yerlestirebiliriz. Etraflarinda gezindigimiz zaman optik akis
vektorleri objelerin sekli hakkinda bize ipucu verecektir, bu
vektorler, objelerin gercek yerinin dis dunyaya olan "bir tercumesi /
yansimasi" olarak gorulebilir. Gizli degisken obje konumu, acik
degisken ise optik akis vektorleridir. Kendi hareketimizi bildigimize
gore x_{t+1} = Ax_t + w formulundeki A degisimini de (translation)
biliyoruz demektir, cunku biz kendi hareketimizi biliyoruz, diger
objeler sabit.

Ekran fotografi, tus yollama temelli bu teknik her turlu oyunu
disaridan oynamak icin kullanilabilir, oyunun disariya API acmasina
gerek kalmaz.

X Windows bazli ekran kapma teknigine gelene kadar diger bazi yollara
baktik, mesela OpenGL'e cengel takmak (bir onceki yazi), fakat bazi
oyunlar libGL.so kutuphanesini direk kendileri program icinden
yukluyorlar (hile yapilmasini engellemek icin, bazilari OpenGL
cengellerini duvarlarin arkasini gormek icin kullaniyor mesela), ve
temiz yollarla grafiklerini cekip cikartmak mumkun olmuyor. Bu
yontemle ekranda gordugumuz her seyi fps-play gorebilir, ve
oynayabilir.





