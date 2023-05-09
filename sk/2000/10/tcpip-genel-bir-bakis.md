# TCP/IP'ye Genel Bir Bakış

İnternet üzerinde bilgisayarlar nasıl adreslenirler ve paketler
yollarını nasıl bulurlar. Bu soru bilgisayar ağları ile uğraşan
herkesin bir süre kafasını kurcalamış bir sorudur. Bu soruya yanit
bulabilmek için ınternet’in temel protokolü olan TÇP/İP’nin yapısını
bir miktar bilmek gerekmektedir.

TÇP/İP protokolünde tüm bilgisayarlar 32 bitlik 'özgün' bir İP
numarasına sahip olacak sekide adreslenirler (buradan çıkarılabilecek
teorik bir sonuç ise internete aynı anda bağlı olabilecek bilgisayar
sayısının en fazla 232 = 4,294,967,296 olabileceğidir) Bunu bir
örnekle ele alırsak, internet üzerinde 3,559,735,316 sayısı ile
adreslenmiş bir bilgisayar düşünelim. Bu sayının heksadesimal
karşılığının D42D4014 olduğunu kolaylıkla hesaplayabiliriz. Bu şekilde
bir gösterimin hemen hiç kimseye birşey ifade etmeyeceği sanırım
oldukca açık bir şekilde görülmektedir. Bu yüzden şu şekilde bir yol
izlenir, bu 32 bitlik adres 8 bitlik adresler halinde 4’e ayrılıp (D4
2D 40 14 şeklinde), daha alışıldık bir sayı sistemiyle çalışabilmek
için desimale çevrilirler (0xD4 = 212, 0x2D = 45, 0x40 = 64 ve
0x14=20). Bu gösterim son olarak aralara konan bir nokta ile
birleştirilir ve sonuç olarak İP numarası olarak tanımlanan notasyona
ulaşılır, yani internet üzerinde 3,559,735,316 sayısı ile adreslenmiş
bilgisayar 212.45.64.20 İP nolu bilgisayardır. Benzer bir yaklaşımı
tersten izleyecek olursak A.B.C.D İP nosuna sahip olduğu bilinen bir
bigisayarın gerçek adresi, A * 224 + B * 216 + C * 28 + D şekline
hesaplanır.

Örnegimizden yola çikarsak 212.45.64.20 için gerçek adres 212 * 224 +
45 * 216 + 64 * 28 + 20 = 3,559,735,316'dir.

IP numarasının bu şekildeki gösterilimi aslında ınternet trafiğinin
yönünün nasıl bulunduğu konusunda hiçbirşey ifade etmez elbette, bir
yığın halinde bulunan 4 milyarın üzerindeki adresin bir kısım gruplara
ayrılması zorunludur. Trafiğin yönünün belirlenmesi ancak paketlerin
belli İP gruplarından gelmesi ve belli gruplara yönelmesi ile mümkün
olabilecektir. Bu durumda her İP paketi, kendi numarasının bağlı
olduğu gruplar için tanımlanmış kurallara göre hareket eder. Yapılan
gruplama işlemine ise şubnetting adı verilir. Bu işlem sırasında İP
adresi ait olduğu grubu ve bu grubun üyeleri arasında kaçıncı sırada
olduğunu belirtmek üzere iki kısma ayrılır. İlk kısma network
numarası, ikinci kısma ise üç adresi adı verilir ve işlem şu şekilde
gerçekleşir.

Tüm internet İP bloğunu 255 kısma ayırmayı istediğimizi düşünelim, bu
gruplama sonucunda ortaya çıkacak İP numaralarının 1.x.y.z,
2.x.y.z,..,255.x.y.z şeklinde olacağı kolay bir akıl yürütme ile
görülebilmektedir. Bu tanımlamada elde edilen İP numaralarının
oluşturduğu blokların her birine şubnet veya network adı verilmektedir
ve 1.0.0.0 networku, 2.0.0.0 networku vs şeklinde telaffuz
edilmektedir. Bu durumda örneğin 2 ile başlayan bütün İP numaralarının
(2.x.y.z) 2.0.0.0 networkunun parçası olduğu kolayca
anlaşılabilir. Dikkati çekmesi gereken bir nokta elde edilen blokların
hala devasa boyularda olduklarıdır (224 = 16,777,216) ve bu bloklar
kendi içlerinde daha fazla bölünmeye tabi tutulabilirler, örneğin
1.0.0.0 networkunu 1.0.0.0, 1.1.0.0,.. 1.254.0.0, 1.255.0.0 şeklinde
255 ayrı networke ayırmak da mümkündür, aynı şekilde 1.1.0.0
networkunu de 1.1.1.0, 1.1.2.0,..1.1.255.0 vs şeklinde daha da
küçültmek mümkündür, bu işleme her blokta 2 hatta 1 İP kalıncaya kadar
devam edilebilir. Burada önemli nokta bu blok büyüklerinin ihtiyaca
göre belirlenmesi gereği ve her bloğun bir üst bloğun alt kümesi
olmasıdır. Daha detaylı açıklarsak, 1.0.0.0 networkünden bahsediyor
iseniz otomatik olarak 1.1.0.0 networkünden ve 1.10.5.0 networkünden
de bahsediyorsunuz demektir.

İP numarasını network numarası ve üç adresi olarak ikiye böldüğümüzü
yukarıda söylemiştik, bunları örneklerle açıklaylım, test amacıyla
seçtiğimiz 212.45.64.20 İP numarasından yola çıkarsak, bu İP’nin hem
212.0.0.0 hem 212.45.0.0 hem de 212.45.64.0 networklerinde yer alan
bir İP olduğu söylenebilir. Burada kritik nokta netwok numarası olarak
hangisinin alınacağı (212, 212.45, 212.45.64) daha da önemlisi buna
nasıl karar verileceğidir. Açıkça görülen odur ki bunu bilmek yalnızca
İP numarası ile mümkün olmamaktadır. Bu nedenle İP numarasının hangi
bitlerinin network numarasınını temsil ettiğini, hangilerinin ise üç
adresini oluşturduğunu tanımlayacak başka bir bilgiye ihtiyaç
duyulmaktadır. Buna “şubnet mask” adı verilmektedir. Çoğu zaman
kullanıcıların kafasını karıştırmakla beraber aslında anlamı ve
kullanımı son derece açıktır.

Şubnet mask’i daha sonra detaylı incelemek üzere kabaca tanımlarsak
network numarasının bulunduğu bit pozisyonlarında 1, kalan
pozisyonlarda 0 bulunduran bir sayı olarak tarif edebiliriz. Örneğin
212.45.64.20 İP’sini alt bölümlemeye gitmeden 212.0.0.0 bloğunun bir
parçası olarak görmek istiyorsak, network adresini yalnızca ilk 8
bitin oluşturduğunu söylüyoruz demektir. Bu durumda şubnet maskımız 8
tane 1 ve 24 tane 0'dan oluşacaktır (toplam 32’yi verecek şekilde).

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

Son olarak, 212.45.64.0 bloğu için aynı hesaplamayı yaparsak, network
adresi ilk 24 bitte bulunacaktır. Şubnet mask ise 24 tane 1 ve 8 tane
0’dan oluşacaktır.

```
 Subnet mask (binary)   : 11111111 11111111 11111111 0000000
Subnet mask (desimal) :        255         255        255          0
Subnet mask              : 255.255.255.0
```

Burada şubnet mask’i belitrmek için kullanılan farklı bir yöntemden
bahsetmek gerekir, bu da “/” ayraçı ile İP numarasına ya da network
numarasına eklenen bir sayıdır (212.45.64.20/25 veya 212.45.64.0/19
gibi). Burada verilen sayı şubnet maşkta ilk kaç bitin 1 olduğunu
gösterir. Örneğin /8, 8 tane 1, 24 tane 0 anlatır, bu da 255.0.0.0
netmaskının eşdeğeridir, yine benzer şekilde /16, 16 tane 1, 16 tane
sıfırı tanımladığı için 255.255.0.0’in, /24 de 255.255.255.0’in
eşdeğer gösterimleridir.

Daha çok kullanılan şubnetler için konulmuş bir takım isimler vardır,
burada bunlardan da bahset gerekir. İlgili isimler, kısa bir tanımlama
ile Tablo 1’de verilmiştir.

```
Tablo1:
NetMask         /'lü gösterim   Ip Adedi             Isim
255.0.0.0       /8                    16,777,216     A Sinifi (A Class)
255.255.0.0     /16                  65,536          B Sinifi (B Class)
255.255.255.0   /24                  256             C Sinifi (C Class)
```

Kullanılabilen İP adedi burada verilenden iki eksik olacaktır (bir
broadcast ve bir network numarası için)

Kullanıcılar arasında sıkça karşılaşılan 32’lik bir C class veya
128’lık bir C class gibi kullanımlar tamamen yanlıştır. Çünkü bir "C
class" tanımı gereği 256’lik bir bloktur.

Şubnetlerin buraya kadar olan kısmı hemen herkesçe bilinmesine rağmen
genellikle /24’den küçük bölümlemeler sorun yaratmaktadır. Bilgisayar
ağları ile uğraşırken çoğu zaman 32’lik, 64’lik veya 128’lık
şubnetlere ihtiyaç duyulmaktadır. Bunların nasıl yapılacağını doğrudan
örneklerle açıklamanın daha faydalı olacağını düşünüyorum.

Elimizde bulunan 212.45.64.0/24'lük bir bloğu 8 tane 32’lik şubnete
ayımak için neler yapılması gerektiğine bakalım. Öncelikle 32’lik bir
şubnet için maskın ne olduğunu hesaplamak gerekmektedir.32, 2’nin
beşinci kuvvetidir, yani 0-31 arası sayılar binary sistemde 5 bitle
yazılabilirler. Öyleyse şubnet maşkta sağdan 5 biti üç adresi için
ayırmamız gerekmektedir, geriye kalan 27 bit ise network kısmını
oluşturacaktır. Bu durumda 32’lık şubnet için masklar,

```
Subnet mask (binary)   : 11111111 11111111 11111111 11100000
Subnet mask (desimal) :          255           255           255           224
Subnet mask       : 255.255.255.224
```

olacaktır. Her şubnet için ayrılan 32 İP için sırasıyla network
adresleri 212.45.64.0/27, 212.45.64.32/27, ...212.45.224/27
olacaktır. Ancak burada, bir adet /24’te yalnızca 0 ve 255 olarak iki
İP kullanılamazken (biri network numarası diğeri broadcast) 32’lik
şubnetlere ayrıldığında her biri için için 2, toplam 2*8=16 İP
numarasının kullanılamadığına dikkat edilmelidir. Benzer bir şekilde
212.45.64.0/24 'lük bloğumu 4 tane 64’lük şubnete ayıralım. 64, 2’nin
altıncı kuvvetidir, yani 0..63 arası sayılar 6 bitle yazılabilirler,
öyleyse, 64’lük bloklar için masklar,

```
Subnet mask (binary)   : 11111111 11111111 11111111 11000000
Subnet mask (desimal) :          255           255           255           192
Subnet mask       : 255.255.255.192
```

şeklinde olacaktır. Sırasıyla şubnetler de 212.45.64.0/26,
212.45.64.64/26,..., 212.45.64.192/26 olacaktır. Burada kaybedilen İP
sayısı 4*2=8 olarak hesaplanabilir. Son olarak aynı bloğu 128’lik iki
bölüme ayırırsak, masklar

```
Subnet mask (binary)   : 11111111 11111111 11111111 10000000
Subnet mask (desimal) :          255           255           255           128
Subnet mask       : 255.255.255.128
```

Şeklinde olacaktır. Sırasıyla şubnetler 212.45.64.0/25 ve
212.45.64.128/25 olacaktır. Kaybedilen İP sayısı ise 2*2=4’tur. Genel
kullanımda bir blok her zaman eşit büyüklükteki bloklara
bölünmez. Elinizdeki bir C sınıfı bloğu 1 128’lik, 1 64’lük ve 2
32’lik bloğa bölmek isteyebilirsiniz. Detaylara girmeden incelersek
böyle bir bölümleme 212.45.64.0/25, 212.45.64.128/26,212.45.64.192/27,
212.45.64.224/27 şeklinde yapılabilir.

Burada en çok dikkat edilmesi gereken nokta, istediğiniz bir bloğu
oluşturmak için büyük bloğun yerini istediğiniz gibi
seçemeyeceğinizdir. Örneğin 128’lik bir bloğu 212.45.64.64’ten
başlatıp 212.45.64.192’de bitiremezsiniz, benzer şekilde
212.45.64.32’de başlayıp 212.45.64.96’da biten bir /26 blok da
tanımlayamazsınız. Başlangıç adresleri kesinlikle bloktaki_İP_sayısı *
n (n tamsayı) formülü ile elde edilebilecek sayılar olmalıdır. Örneğin
64’lük bir blok ancak 0,64,128,192 adreslerinden başlayabilir, benzer
şekilde 128’lik bir blok da ancak 0 veya 128 adreslerinden
başlayabilir.

Son olarak verilen bir İP numarası / subnet mask çifti ile bu İP’nin
ait olduğu bloğun nasıl bulunabileceğini açıklayarak bu bölümü
bitiriyorum. Bir İP’nin ait olduğu network, İP numarasının binary hali
ile şubnet maskın binary hali arasında yapılacak birebir "VE" işlemi
(bitwise AND) ile bulunur, örneklemek için 212.45.64.20/25 İP
numarasının ait olduğu networku bulalım

/25, 25 adet 1 ve 7 adet 0’dan olusan bir netmask anlamina gelmektedir
(255.255.255.128), binary ortamda ifade edecek olursak

```
 Netmask : 11111111 11111111 11111111 10000000
```

İP numarasını binary olarak ifade edersek ise

```
IP     : 11010100 00101101 01000000 00010100
```

Değelerini elde ederiz.

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

elde edilir, bu da 212.45.64.228/27 İP numarasının 212.45.64.224/27
networkunde yeraldığını gösterir.

Yukarıdaki yazı yazarın izni ile http://www.ilkertemir.com sitesinden
alınmıştır

