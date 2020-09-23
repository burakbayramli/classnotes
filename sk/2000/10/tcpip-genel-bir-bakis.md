# TCP/IP'ye Genel Bir Bakis

Internet üzerinde bilgisayarlar nasil adreslenirler ve paketler
yollarini nasil bulurlar. Bu soru bilgisayar aglari ile ugrasan
herkesin bir süre kafasini kurcalamis bir sorudur. Bu soruya yanit
bulabilmek için internet’in temel protokolü olan TCP/IP’nin yapisini
bir miktar bilmek gerekmektedir.

TCP/IP protokolünde tüm bilgisayarlar 32 bitlik 'özgün' bir IP
numarasina sahip olacak sekide adreslenirler (buradan çikarilabilecek
teorik bir sonuç ise internete ayni anda bagli olabilecek bilgisayar
sayisinin en fazla 232 = 4,294,967,296 olabilecegidir) Bunu bir
örnekle ele alirsak, internet üzerinde 3,559,735,316 sayisi ile
adreslenmis bir bilgisayar düsünelim. Bu sayinin heksadesimal
karsiliginin D42D4014 oldugunu kolaylikla hesaplayabiliriz. Bu sekilde
bir gösterimin hemen hiç kimseye birsey ifade etmeyecegi sanirim
oldukça açik bir sekilde görülmektedir. Bu yüzden su sekilde bir yol
izlenir, bu 32 bitlik adres 8 bitlik adresler halinde 4’e ayrilip (D4
2D 40 14 seklinde), daha alisildik bir sayi sistemiyle çalisabilmek
için desimale çevrilirler (0xD4 = 212, 0x2D = 45, 0x40 = 64 ve
0x14=20). Bu gösterim son olarak aralara konan bir nokta ile
birlestirilir ve sonuç olarak IP numarasi olarak tanimlanan notasyona
ulasilir, yani internet üzerinde 3,559,735,316 sayisi ile adreslenmis
bilgisayar 212.45.64.20 IP nolu bilgisayardir. Benzer bir yaklasimi
tersten izleyecek olursak A.B.C.D IP nosuna sahip oldugu bilinen bir
bigisayarin gerçek adresi, A * 224 + B * 216 + C * 28 + D sekline
hesaplanir.

Örnegimizden yola çikarsak 212.45.64.20 için gerçek adres 212 * 224 +
45 * 216 + 64 * 28 + 20 = 3,559,735,316'dir.

IP numarasinin bu sekildeki gösterilimi aslinda internet trafiginin
yönünün nasil bulundugu konusunda hiçbirsey ifade etmez elbette, bir
yigin halinde bulunan 4 milyarin üzerindeki adresin bir kisim gruplara
ayrilmasi zorunludur. Trafigin yönünün belirlenmesi ancak paketlerin
belli IP gruplarindan gelmesi ve belli gruplara yönelmesi ile mümkün
olabilecektir. Bu durumda her IP paketi, kendi numarasinin bagli
oldugu gruplar için tanimlanmis kurallara göre hareket eder. Yapilan
gruplama islemine ise subnetting adi verilir. Bu islem sirasinda IP
adresi ait oldugu grubu ve bu grubun üyeleri arasinda kaçinci sirada
oldugunu belirtmek üzere iki kisma ayrilir. Ilk kisma network
numarasi, ikinci kisma ise uç adresi adi verilir ve islem su sekilde
gerçeklesir.

Tüm internet IP blogunu 255 kisma ayirmayi istedigimizi düsünelim, bu
gruplama sonucunda ortaya çikacak IP numaralarinin 1.x.y.z,
2.x.y.z,..,255.x.y.z seklinde olacagi kolay bir akil yürütme ile
görülebilmektedir. Bu tanimlamada elde edilen IP numaralarinin
olusturdugu bloklarin her birine subnet veya network adi verilmektedir
ve 1.0.0.0 networkü, 2.0.0.0 networkü vs seklinde telaffuz
edilmektedir. Bu durumda örnegin 2 ile baslayan bütün IP numaralarinin
(2.x.y.z) 2.0.0.0 networkünün parçasi oldugu kolayca
anlasilabilir. Dikkati çekmesi gereken bir nokta elde edilen bloklarin
hala devasa boyularda olduklaridir (224 = 16,777,216) ve bu bloklar
kendi içlerinde daha fazla bölünmeye tabi tutulabilirler, örnegin
1.0.0.0 networkünü 1.0.0.0, 1.1.0.0,.. 1.254.0.0, 1.255.0.0 seklinde
255 ayri networke ayirmak da mümkündür, ayni sekilde 1.1.0.0
networkünü de 1.1.1.0, 1.1.2.0,..1.1.255.0 vs seklinde daha da
küçültmek mümkündür, bu isleme her blokta 2 hatta 1 IP kalincaya kadar
devam edilebilir. Burada önemli nokta bu blok büyüklerinin ihtiyaca
göre belirlenmesi geregi ve her blogun bir üst blogun alt kümesi
olmasidir. Daha detayli açiklarsak, 1.0.0.0 networkünden bahsediyor
iseniz otomatik olarak 1.1.0.0 networkünden ve 1.10.5.0 networkünden
de bahsediyorsunuz demektir.

IP numarasini network numarasi ve uç adresi olarak ikiye böldügümüzü
yukarida söylemistik, bunlari örneklerle açiklaylim, test amaciyla
seçtigimiz 212.45.64.20 IP numarasindan yola çikarsak, bu IP’nin hem
212.0.0.0 hem 212.45.0.0 hem de 212.45.64.0 networklerinde yer alan
bir IP oldugu söylenebilir. Burada kritik nokta netwok numarasi olarak
hangisinin alinacagi (212, 212.45, 212.45.64) daha da önemlisi buna
nasil karar verilecegidir. Açikça görülen odur ki bunu bilmek yalnizca
IP numarasi ile mümkün olmamaktadir. Bu nedenle IP numarasinin hangi
bitlerinin network numarasinini temsil ettigini, hangilerinin ise uç
adresini olusturdugunu tanimlayacak baska bir bilgiye ihtiyaç
duyulmaktadir. Buna “subnet mask” adi verilmektedir. Çogu zaman
kullanicilarin kafasini karistirmakla beraber aslinda anlami ve
kullanimi son derece açiktir.

Subnet mask’i daha sonra detayli incelemek üzere kabaca tanimlarsak
network numarasinin bulundugu bit pozisyonlarinda 1, kalan
pozisyonlarda 0 bulunduran bir sayi olarak tarif edebiliriz. Örnegin
212.45.64.20 IP’sini alt bölümlemeye gitmeden 212.0.0.0 blogunun bir
parçasi olarak görmek istiyorsak, network adresini yalnizca ilk 8
bitin olusturdugunu söylüyoruz demektir. Bu durumda subnet maskimiz 8
tane 1 ve 24 tane 0'dan olusacaktir (toplam 32’yi verecek sekilde).

```
 Subnet mask (binary)   : 11111111 00000000 000000000 0000000
Subnet mask (desimal) :        255            0             0          0
Subnet mask              : 255.0.0.0
```

Ya da 212.45.0.0 blogunun bir parçasi olmasini istiyorsak, bu kez
network adresini ilk 16 biti ile tanimlamamiz gerekecektir, bu durumda
subnet mask 16 tane 1 ve 16 tane 0’dan olusacaktir.

```
 Subnet mask (binary)   : 11111111 11111111 000000000 0000000
Subnet mask (desimal) :       255          255             0       ;    0
Subnet mask              : 255.255.0.0
```

Son olarak, 212.45.64.0 blogu için ayni hesaplamayi yaparsak, network
adresi ilk 24 bitte bulunacaktir. Subnet mask ise 24 tane 1 ve 8 tane
0’dan olusacaktir.

```
 Subnet mask (binary)   : 11111111 11111111 11111111 0000000
Subnet mask (desimal) :        255         255        255          0
Subnet mask              : 255.255.255.0
```

Burada subnet mask’i belitrmek için kullanilan farkli bir yöntemden
bahsetmek gerekir, bu da “/” ayraci ile IP numarasina ya da network
numarasina eklenen bir sayidir (212.45.64.20/25 veya 212.45.64.0/19
gibi). Burada verilen sayi subnet maskta ilk kaç bitin 1 oldugunu
gösterir. Örnegin /8, 8 tane 1, 24 tane 0 anlatir, bu da 255.0.0.0
netmaskinin esdegeridir, yine benzer sekilde /16, 16 tane 1, 16 tane
sifiri tanimladigi için 255.255.0.0’in, /24 de 255.255.255.0’in
esdeger gösterimleridir.

Daha çok kullanilan subnetler için konulmus bir takim isimler vardir,
burada bunlardan da bahset gerekir. Ilgili isimler, kisa bir tanimlama
ile Tablo 1’de verilmistir.

```
Tablo1:
NetMask         /'lü gösterim   Ip Adedi             Isim
255.0.0.0       /8                    16,777,216     A Sinifi (A Class)
255.255.0.0     /16                  65,536          B Sinifi (B Class)
255.255.255.0   /24                  256             C Sinifi (C Class)
```

Kullanilabilen IP adedi burada verilenden iki eksik olacaktir (bir
broadcast ve bir network numarasi için)

Kullanicilar arasinda sikça karsilasilan 32’lik bir C class veya
128’lik bir C class gibi kullanimlar tamamen yanlistir. Çünkü bir "C
class" tanimi geregi 256’lik bir bloktur.

Subnetlerin buraya kadar olan kismi hemen herkesçe bilinmesine ragmen
genellikle /24’den küçük bölümlemeler sorun yaratmaktadir. Bilgisayar
aglari ile ugrasirken çogu zaman 32’lik, 64’lik veya 128’lik
subnetlere ihtiyaç duyulmaktadir. Bunlarin nasil yapilacagini dogrudan
örneklerle açiklamanin daha faydali olacagini düsünüyorum.

Elimizde bulunan 212.45.64.0/24'lük bir blogu 8 tane 32’lik subnete
ayimak için neler yapilmasi gerektigine bakalim. Öncelikle 32’lik bir
subnet için maskin ne oldugunu hesaplamak gerekmektedir.32, 2’nin
besinci kuvvetidir, yani 0-31 arasi sayilar binary sistemde 5 bitle
yazilabilirler. Öyleyse subnet maskta sagdan 5 biti uç adresi için
ayirmamiz gerekmektedir, geriye kalan 27 bit ise network kismini
olusturacaktir. Bu durumda 32’lik subnet için masklar,

```
Subnet mask (binary)   : 11111111 11111111 11111111 11100000
Subnet mask (desimal) :          255           255           255           224
Subnet mask       : 255.255.255.224
```

olacaktir. Her subnet için ayrilan 32 IP için sirasiyla network
adresleri 212.45.64.0/27, 212.45.64.32/27, ...212.45.224/27
olacaktir. Ancak burada, bir adet /24’te yalnizca 0 ve 255 olarak iki
IP kullanilamazken (biri network numarasi digeri broadcast) 32’lik
subnetlere ayrildiginda her biri için için 2, toplam 2*8=16 IP
numarasinin kullanilamadigina dikkat edilmelidir. Benzer bir sekilde
212.45.64.0/24 'lük blogumu 4 tane 64’lük subnete ayiralim. 64, 2’nin
altinci kuvvetidir, yani 0..63 arasi sayilar 6 bitle yazilabilirler,
öyleyse, 64’lük bloklar için masklar,

```
Subnet mask (binary)   : 11111111 11111111 11111111 11000000
Subnet mask (desimal) :          255           255           255           192
Subnet mask       : 255.255.255.192
```

seklinde olacaktir. Sirasiyla subnetler de 212.45.64.0/26,
212.45.64.64/26,..., 212.45.64.192/26 olacaktir. Burada kaybedilen IP
sayisi 4*2=8 olarak hesaplanabilir. Son olarak ayni blogu 128’lik iki
bölüme ayirirsak, masklar

```
Subnet mask (binary)   : 11111111 11111111 11111111 10000000
Subnet mask (desimal) :          255           255           255           128
Subnet mask       : 255.255.255.128
```

Seklinde olacaktir. Sirasiyla subnetler 212.45.64.0/25 ve
212.45.64.128/25 olacaktir. Kaybedilen IP sayisi ise 2*2=4’tür. Genel
kullanimda bir blok her zaman esit büyüklükteki bloklara
bölünmez. Elinizdeki bir C sinifi blogu 1 128’lik, 1 64’lük ve 2
32’lik bloga bölmek isteyebilirsiniz. Detaylara girmeden incelersek
böyle bir bölümleme 212.45.64.0/25, 212.45.64.128/26,212.45.64.192/27,
212.45.64.224/27 seklinde yapilabilir.

Burada en çok dikkat edilmesi gereken nokta, istediginiz bir blogu
olusturmak için büyük blogun yerini istediginiz gibi
seçemeyeceginizdir. Örnegin 128’lik bir blogu 212.45.64.64’ten
baslatip 212.45.64.192’de bitiremezsiniz, benzer sekilde
212.45.64.32’de baslayip 212.45.64.96’da biten bir /26 blok da
tanimlayamazsiniz. Baslangiç adresleri kesinlikle bloktaki_IP_sayisi *
n (n tamsayi) formülü ile elde edilebilecek sayilar olmalidir. Örnegin
64’lük bir blok ancak 0,64,128,192 adreslerinden baslayabilir, benzer
sekilde 128’lik bir blok da ancak 0 veya 128 adreslerinden
baslayabilir.

Son olarak verilen bir IP numarasi / subnet mask çifti ile bu IP’nin
ait oldugu blogun nasil bulunabilecegini açiklayarak bu bölümü
bitiriyorum. Bir IP’nin ait oldugu network, IP numarasinin binary hali
ile subnet maskin binary hali arasinda yapilacak birebir "VE" islemi
(bitwise AND) ile bulunur, örneklemek için 212.45.64.20/25 IP
numarasinin ait oldugu networkü bulalim

/25, 25 adet 1 ve 7 adet 0’dan olusan bir netmask anlamina gelmektedir
(255.255.255.128), binary ortamda ifade edecek olursak

```
 Netmask : 11111111 11111111 11111111 10000000
```

IP numarasini binary olarak ifade edersek ise

```
IP     : 11010100 00101101 01000000 00010100
```

Degelerini elde ederiz.

```
Birebir “VE” islemi ile

   11010100 00101101 01000000 00010100
VE 11111111 11111111 11111111 10000000
----------- -----------------------
   11010100 00101101 01000000 00000000
          212          45          64           0
```

Bu sonuç 212.45.64.20/26 IP numarasinin 212.45.64.0/26 networkünde
bulundugunu göstermektedir.

Ayni islem 212.45.64.228/27 IP numarasi için tekrarlanirsa

```
   11010100 00101101 01000000 11100100
VE 11111111 11111111 11111111 11000000
--------- ------------------------
   11010100 00101101 01000000 11000000
          212          45          64         224
```

elde edilir, bu da 212.45.64.228/27 IP numarasinin 212.45.64.224/27
networkünde yeraldigini gösterir.

Yukaridaki yazi yazarin izni ile http://www.ilkertemir.com sitesinden
alinmistir
