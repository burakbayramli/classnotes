# Instanceof, Çokyüzlülük (Polymorphism)

Nesnesel dillerin ve yöntemlerin eniyileştirme sağladığı alanlardan
birisi, tip idaresidir. Tip idaresi, programcının kendi tiplerini
(class'lar sayesinde) kendisinin yaratmasını, ve bu tipler arasında
hiyerarşik ilişkiler kurmasını sağlar.  Her kitap tarafından oldukça
önem verilerek anlatılan kalıtım özelliği (inheritance), bu bağlamda
buzdağının sadece gözüken kısmı olmaktadır. Kalıtım, bizi aynı kodu
tekrar tekrar yazmaktan kurtarır, doğrudur. Fakat tek başına kalıtımla
elde ettiğimizi daha modüler kodlarla da elde edebilirdik. Kalıtımı
daha da güçlü hâle getiren ikinci bir teknik daha vardır: Çokyüzlülük
(polymorphism).  Çokyüzlülük bizi şu şekilde kodlar yazmaktan
kurtarır.

public class Cizer {public void metot (ParametreClass obj) { if (obj
instanceof Ucgen) { ucgenCiz(obj); } if (obj instanceof Dikdortgen) {
dikdortgenCiz(obj); } if (obj instanceof Kare) { kareCiz(obj); }}...}

Java programlarının %99.9'unda instanceof kullanımı
görmemelisiniz. Tekrar söylüyorum: instanceof, nesnesel tasarımın ve
tavsiyelerinin tam zıttıdır. Gördüğünüz yerde yokediniz, gördüğünüz
yerde çokyüzlülük ve kalıtım kullanarak düzeltiniz.  Yukarıdaki örnek,
tipik bir nesnesel tasarım problemidir. Cizer class'ının her grafik
nesne için tanımlanmış cizXX metotları, Cizer class'ından çıkartılıp,
ait oldukları grafik nesnelerin class'larına taşınmalıdır. Metot
ucgenCiz, Ucgen class'ına, metot dikdortgenCiz, Dikdortgen class'ına
ve metot kareCiz, Kare class'ına taşınmalıdır.  Çokyüzlülük için de:
Kodu öyle hâle getirmeliyiz ki, "cağıran" nesne, çağırılan nesnenin
tipinden habersiz olsun, böylece her biri "sorarak" if kullanması
gerekmesin.  O zaman, ciz metotlarını taşırken, hepsinin isimlerini
ortak yapıp, tüm grafik class'larına ortak bir class'da bu metotu
tanımlayacağız.  //// Tum grafik nesne class'larının ortak
class'ı//public class GrafikNesne { public void abstract
ciz();}//public Ucgen extends GrafikNesne { public void ciz() { ...
. Cizer.ucgenCiz metotunun icerigi buraya ..  }}//public Dikdortgen
extends GrafikNesne { public void ciz() { ...  . Cizer.dikdortgenCiz
metotunun icerigi buraya ..  }}//public Kare extends GrafikNesne {
public void ciz() { ..  . Cizer.kareCiz metotunun icerigi buraya ..
}}

Bu değişimden sonra artık kodun herhangi bir yeri Ucgen, Dikdortgen,
Kare nesnelerinden birini yaratıp, GrafikNesne referansı üzerine atama
yaparsa, bu referans üzerinden yapılan ciz() metotu çağrıları, ciz()
GrafikNesne üzerinde tanımlanmış olduğundan (ama gerçekleştirimi
(kodlaması) alt kalıtım yapan nesneler üzerinde olduğundan) işleme anı
sırasında ufak bir JVM kontrolü yaparak aktif nesnenin esas tipini
öğrenecek, ve ciz() çağrısını doğru tipteki nesneye gönderecektir. Tüm
bu kontroller için programcının kılını kıpırdatmasına gerek yoktur.
public class HerhangiBirClass { public void ciz() { GrafikNesne nesne
= new Ucgen(); // dikkat, Ucgen nesne = new Ucgen degil!!!  ..
nesne.ciz(); }} Bu aynı satırda yapılan yaratma ve anında kullanma
vadedilen avantajlar açısından sizi hayal kırıklığına
uğratmasın. Onbinlerce satırlık bir kodun ta bir ucunda yaratılmış bir
Ucgen nesnesinin, öteki ucunda, başkasının yazmış olduğu bir metot
gönderildiğini düşünün. Alan metot, soyutluk seviyesi bakımından
grafik nesnesinin ciz() metotunu "kabul ettiği haricinde" daha fazla
bir bilgiyle ilgilenmeme gibi bir sânsı artık elinde vardır. Böylece
elindeki referansı GrafikNesne seviyesinden daha aşağıda tutmasına
gerek yoktur.  Böylece, ileriki bir günde yeni bir GrafikNesne
kodlandığında (Beşgen diyelim), hiçbir ana listenin, hiçbir merkezi
noktanın değişmesine gerek kalmayacaktır. Tüm ciz() metotlarıni
çağıranlar aynı şekilde çağırmaya devam edeceklerdir, çünkü
ellerindeki GrafikNesne class'ı halen statik olarak geçerli bir
referanstır (Beşgen de GrafikNesne'den kalıtım yapacaktır, ve ciz()
metotunu tekrar tanımlayacaktır).  Instanceof yöntemini kullanıyor
olsaydık, "büyük" listemize bir de instanceof Besgen gibi bir ibare
eklememiz gerekecekti. Nesnesel yöntemlerde buna hiçbir gerek
yoktur. Tip kontrolleri, düzgün yapılan nesnesel model üzerinden JVM
tarafınan otomatik olarak yapılabilen birşeydir. Makinanın yapacağını,
makinaya bırakalım.




