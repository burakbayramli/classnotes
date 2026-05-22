# Sıvı ve Gaz Mekaniği - 2

Boltzmann Taşıma Denklemi

Sert küresel parçacıklardan oluşan ve büyük hızlarla hareket eden
seyrek bir gaz düşünelim. Parçacıkların etkileşimlerini yalnızca
elastik çarpışmalarla sınırlandırıyoruz. Varsayım olarak herhangi bir
anda her bir parçacığın konum vektörünü ve hızını bilmek mümkün
olabilir. Bu tür bilgi, sistemin tam dinamik durumunu verir ve klasik
mekanikle birlikte tüm gelecekteki durumların tam olarak tahmin
edilmesine olanak tanır [1]. Ancak gerçekçi bir simülasyonda bu tür
bir takip, muazzam bilişim kaynakları gerektirir.

Ama alternatif olarak sistemi bir *dağılım* fonksiyonu $f(r, c, t)$
ile tanımlayabiliriz. Burada dağılım, koordinatların konum, hız
vektörleri ve zamandan oluştuğu bir "faz uzayı"nda yer
alır. İstatistiksel Mekanik, $f(r, c, t)$ dağılımının belirli bir
konum ve hıza sahip herhangi bir molekülü bulma olasılığını verdiği
istatistiksel bir imkan sunar.

Not: Elbette dağılım fonksiyonları, temel olasılıkta olduğu gibi,
belirli bir $r, c, t$ değeri için tek bir olasılık vermiyor, gerçek
olasılıklar için bölgelerden söz ederiz (faz uzayının); örneğin $r$
ile $r+\Delta r$ arasındaki bir alan. $[r, r + \Delta r]$ hacim
elemanında yer alan ve hızları $[c, c + \Delta c]$ aralığında olan
$f(c, r, t)\Delta c \Delta r$ kadar molekül bulunduğunu
söyleyebiliriz. Hızlar $c$ için "aralık" ifadesi garip gelebilir;
ancak ilgilendiğimiz faz uzayında durum tam olarak budur. Diyelim ki
$(2, 1)$ noktasında ve çevresindeki $\Delta r$ içinde yalnızca $c =
(3, 4)$ hızına sahip tek bir molekül yoktur. Orada bir "bulut" vardır.

- Bazı moleküllerin $c = (3, 4)$ hızı olabilir.
- Bazılarının $c = (-1, 2)$ hızı olabilir.
- Bazılarının $c = (5, 0)$ hızı olabilir.

$f(r, c, t)\Delta r$ fonksiyonu, o tek noktadaki her olası hız vektörü
için sayıyı verir.

Kuvvet Uygulamak, Dinamikleri Değiştirmek

Şimdi bu sisteme $F$ ile temsil edilen bir kuvvet uygulandığında ne
olduğunu düşünelim. Bu kuvvet bir alan niteliğinde olacaktır; her
yerde hissedilen $F(r)$ kuvvetidir. $t$ anında mevcut bir hız da
vardır. Şimdilik moleküller arasındaki iç çarpışmaları göz ardı
edeceğiz.

![](phy_030_flgas2_01.jpg)

Bu değişiklikleri $f$ üzerinde yansıtmamız gerekir. Hareket eden bir
parçacığın konumu $x$ ve hızı $c$ zamanın $t$ açık birer fonksiyonu
olduğundan, dağılım fonksiyonu $f(x(t), c(t), t)$ olarak yazılabilir.

$$\ud f = \frac{\partial f}{\partial t}\ud t + \frac{\partial f}{\partial r}\ud r + \frac{\partial f}{\partial c}\ud c$$

$f$'nin zamanla doğrudan nasıl değiştiğini (şimdilik dış kuvvet
olmaksızın) öğrenmek istiyorsak, standart çok değişkenli zincir
kuralını kullanarak toplam türevini alırız. $f$'nin zamanla değişen üç
bileşeni olduğundan (konum, hız ve zamanın kendisi), diferansiyel
hesabın zincir kuralı bunu üç temiz parçaya ayırır:

$$\frac{\ud f}{\ud t} = \frac{\partial f}{\partial t} + \frac{\partial f}{\partial r}\frac{\ud r}{\ud t} + \frac{\partial f}{\partial c}\frac{\ud c}{\ud t}$$

Daha sonra uygulanan $F$ kuvvetini hesaba katarak bu takip
türevlerinin gerçek fiziksel tanımlarını yerine koyarız.

Zaman: $\frac{\ud t}{\ud t}$ yalnızca 1'dir.

Hız: $\frac{\ud x}{\ud t}$, hızın $(v)$ tanımıdır.

İvme: $\frac{\ud c}{\ud t}$, ivmenin tanımıdır; Newton'un İkinci
Yasası bunu $\frac{F}{m}$ olarak tanımlar.

Bu değerleri doğrudan zincir kuralına koyarak şunu elde ederiz:

$$
= \frac{\partial f}{\partial t} +
c \cdot \frac{\partial f}{\partial x} +
\frac{F}{m} \cdot \frac{\partial f}{\partial c}
$$

Yukarıdaki denklem Boltzmann taşıma denklemi olarak bilinir [2,
sf. 27]. Vektör gösterimi ile:

$$
\frac{\ud f}{\ud t} = \frac{\partial f}{\partial t} +
c \cdot \frac{\partial f}{\partial r} +
\frac{F}{m}\frac{\partial f}{\partial c}
\tag{1}
$$

Bu formülasyon çarpışma olmadığını varsaydı. Bu durumda, $\ud f/\ud t$
aracılığıyla değişimden sonra yeni konumlarındaki $f(r, c, t)$
parçacıklarını takip ettiğimizde dağılım tamamen aynı
kalırdı. Dolayısıyla $(1)$ için $\ud f/\ud t = 0$ diyebiliriz. Ancak
çarpışmalar söz konusu olsaydı, bunu formülasyona sağ tarafta bir
$\Omega$ aracılığıyla dahil etmemiz gerekirdi:

$$
\frac{\partial f}{\partial t} + c \cdot \frac{\partial f}{\partial r} + \frac{F}{m}\frac{\partial f}{\partial c} = \Omega
$$

Boltzmann Taşıma Denklemi için Alternatif Yollar

Denklem (1)'e ulaşmanın başka bir yolu daha vardır. Birim kütleli bir
gaz molekülüne etki eden dış bir $F$ kuvveti, molekülün hızını $c$'den
$c + F\ud t$'ye ve konumunu $r$'den $r + c\,\ud t$'ye
değiştirecektir. Dış kuvvet uygulanmadan önceki $f(r, c, t)$ molekül
sayısı, moleküller arasında hiçbir çarpışma gerçekleşmemesi halinde,
bozulma sonrasındaki molekül sayısı olan $f(r + c\,\ud t,\, c + F\,\ud
t,\, t + \ud t)$'ye eşittir.

[3, sf. 46] şöyle ifade eder: Her molekülün, $r$ ve $t$'nin bir
fonksiyonu olabilen ancak $c$'nin fonksiyonu olmayan dış bir $mF$
kuvvetine maruz kaldığı bir gazı ele alalım. $t$ ile $t + \ud t$
zamanları arasında, başka bir molekülle çarpışmayan herhangi bir
molekülün $c$ hızı $c + F\,\ud t$'ye, konum vektörü $r$ ise $r +
c\,\ud t$'ye değişecektir. $t$ anında $[r, r + \ud r]$ hacim
elemanında yer alan ve hızları $[c, c + \ud c]$ aralığında olan $f(c,
r, t)\,\ud c\,\ud r$ kadar molekül vardır. $\ud t$ aralığından sonra,
çarpışmaların etkisi göz ardı edilebilseydi, aynı moleküller ve
yalnızca onlar, $[r + c\,\ud t,\, r + c\,\ud t + \ud r]$ hacmini
dolduran ve hızları $[c + F\,\ud t,\, c + F\,\ud t + \ud c]$
aralığında olan kümeyi oluştururdu.

Sonuç olarak şunu elde ederiz:

$$f(c + F\,\ud t,\, r + c\,\ud t,\, t + \ud t) = f(c, r, t)\,\ud c\,\ud r = 0$$

Zaman Diferansiyelini İzole Etmek: Cebirsel değişime başlamak için
denklemin her iki tarafını toplam diferansiyel hacim elemanı $\ud
c\,\ud r\,\ud t$'ye böleriz. $\ud c$ ve $\ud r$ her iki tarafta çarpan
olarak göründüğünden hemen sadeleşir:

$$\frac{f(c + F\,\ud t,\, r + c\,\ud t,\, t + \ud t) - f(c, r, t)}{\ud t} = 0$$

Şimdi, sol tarafın $\ud t \to 0$ limitini değerlendirmemiz
gerekir. Bunu yapmak için kaydırılmış $f(c + F\,\ud t,\, r + c\,\ud
t,\, t + \ud t)$ fonksiyonunu açmamız gerekiyor. Çok değişkenli Taylor
açılımı ile bunu yapabiliriz. Taban koordinatlarından biraz uzakta
kaydırılmış bir fonksiyonu değerlendirmek için birinci mertebeden çok
değişkenli Taylor serisi açılımı lazım.

Bağımsız değişkenlere $(x_1, x_2, x_3, \ldots)$ bağlı genel bir $f$
fonksiyonu için, $(a_1, a_2, a_3, \ldots)$ taban noktası etrafındaki
resmi birinci mertebe Taylor açılımı açık koordinat farklarıyla şöyle
yazılır:

$$f(x_1, x_2, \ldots) \approx f(a_1, a_2, \ldots) + \frac{\partial f}{\partial x_1}(x_1 - a_1) + \frac{\partial f}{\partial x_2}(x_2 - a_2) + \ldots$$

Dağılım fonksiyonumuz $f$, yedi ayrı değişkene bağlı olduğundan — üç
konum koordinatı $(x, y, z)$, üç hız koordinatı $(u, v, w)$ ve zaman
$(t)$ — resmi ders kitabı açılımı şöyle görünür:

$$f(x,y,z,u,v,w,t) \approx f(a_x,a_y,a_z,a_u,a_v,a_w,a_t) + \frac{\partial f}{\partial x}(x-a_x) + \frac{\partial f}{\partial y}(y-a_y) + \frac{\partial f}{\partial z}(z-a_z) + \frac{\partial f}{\partial u}(u-a_u) + \frac{\partial f}{\partial v}(v-a_v) + \cdots$$

Formülü Türetmemizle İlişkilendirmek: Fiziksel sistemimizde, taban
noktamızı ve kaydırılmış değerlendirme noktamızı oluşturmak için
koordinatları eşleştiririz:

- Taban Noktası $(a_x, a_y, \ldots)$: Başlangıç durumu $(x, y, z, u, v, w, t)$
- Değerlendirme Noktası $(x, y, \ldots)$: $\ud t$ süresinden sonra kaydırılmış durum, yani $(x + u\,\ud t,\, y + v\,\ud t,\, z + w\,\ud t,\, u + F_x\,\ud t,\, v + F_y\,\ud t,\, w + F_z\,\ud t,\, t + \ud t)$

Şimdi zorunlu ders kitabı çıkarma terimlerinin $(x - a_x)$, $(y -
a_y)$ vb. fiziksel büyüklerimize nasıl dönüştüğüne bakın:

- Konum $x$ çıkarması: $(x + u\,\ud t) - x = u\,\ud t$
- Konum $y$ çıkarması: $(y + v\,\ud t) - y = v\,\ud t$
- Konum $z$ çıkarması: $(z + w\,\ud t) - z = w\,\ud t$
- Hız $u$ çıkarması: $(u + F_x\,\ud t) - u = F_x\,\ud t$
- Hız $v$ çıkarması: $(v + F_y\,\ud t) - v = F_y\,\ud t$
- Hız $w$ çıkarması: $(w + F_z\,\ud t) - w = F_z\,\ud t$
- Zaman $t$ çıkarması: $(t + \ud t) - t = \ud t$

Bu tam varyasyonları resmi Taylor açılımı formülüne geri koyarak şunu elde ederiz:

$$f(c+F\,\ud t,\, r+c\,\ud t,\, t+\ud t) \approx f(c,r,t) + \frac{\partial f}{\partial x}(u\,\ud t) + \frac{\partial f}{\partial y}(v\,\ud t) + \frac{\partial f}{\partial z}(w\,\ud t) + \frac{\partial f}{\partial u}(F_x\,\ud t) + \frac{\partial f}{\partial v}(F_y\,\ud t) + \frac{\partial f}{\partial w}(F_z\,\ud t) + \frac{\partial f}{\partial t}(\ud t) + O(\ud t^2)$$

Not: $O(\ud t^2)$, $\ud t^2$ veya daha yüksek mertebeden terimlerle
orantılı küçük yüksek mertebe matematiksel terimleri temsil eder;
limit alınırken bu terimler güvenle sıfıra gider.

Yerine Koyma ve Cebirsel Sadeleştirme: Şimdi yeni genişletilmiş
ifadeyi 1. Adımdaki denge denkleminin payına koyarız:

$$\frac{\left[f(c,r,t) + \frac{\partial f}{\partial t}\ud t + \left(u\frac{\partial f}{\partial x} + v\frac{\partial f}{\partial y} + w\frac{\partial f}{\partial z}\right)\ud t + \left(F_x\frac{\partial f}{\partial u} + F_y\frac{\partial f}{\partial v} + F_z\frac{\partial f}{\partial w}\right)\ud t + O(\ud t^2)\right] - f(c,r,t)}{\ud t} = 0$$

Payın başındaki özgün $f(c, r, t)$ dağılım fonksiyonu değerinin payın
sonundaki $-f(c, r, t)$ ile mükemmel biçimde sadeleştiğine dikkat
edin:

$$\frac{\frac{\partial f}{\partial t}\ud t + \left(u\frac{\partial f}{\partial x} + v\frac{\partial f}{\partial y} + w\frac{\partial f}{\partial z}\right)\ud t + \left(F_x\frac{\partial f}{\partial u} + F_y\frac{\partial f}{\partial v} + F_z\frac{\partial f}{\partial w}\right)\ud t + O(\ud t^2)}{\ud t} = 0$$

Paydaki $\ud t$'ye böleriz; paydaki kalan her terim en az bir $\ud t$
çarpanı taşımaktadır:

$$= \frac{\partial f}{\partial t} + u\frac{\partial f}{\partial x} + v\frac{\partial f}{\partial y} + w\frac{\partial f}{\partial z} + F_x\frac{\partial f}{\partial u} + F_y\frac{\partial f}{\partial v} + F_z\frac{\partial f}{\partial w} + O(\ud t)$$

Limiti Uygulamak: Son olarak $\ud t$'yi sıfıra götürürüz ($\ud t \to
0$).

$O(\ud t)$ içine hapsolmuş herhangi bir yüksek mertebe terimi hâlâ bir $\ud t$ çarpanı taşıdığından tam olarak $0$'a iner. Birinci mertebeden türevler ise $\ud t$ terimleri zaten temiz biçimde bölündüğünden hiç etkilenmez.

Bu, klasik genişletilmiş Boltzmann Denklemini geride bırakarak
cebirsel geçişi tamamlar:

$$= \frac{\partial f}{\partial t} + u\frac{\partial f}{\partial x} + v\frac{\partial f}{\partial y} + w\frac{\partial f}{\partial z} + F_x\frac{\partial f}{\partial u} + F_y\frac{\partial f}{\partial v} + F_z\frac{\partial f}{\partial w}$$

$$= \frac{\partial f}{\partial t} + c \cdot \frac{\partial f}{\partial r} + F \cdot \frac{\partial f}{\partial c}$$

Kaynaklar

[1] Sukop, Lattice Boltzmann Modeling

[2] Mohamad, Lattice Boltzmann Method with Computer Codes, 2. Baskı

[3] Chapman, The Mathematical Theory of Non-Uniform Gases

[4] Gibiansky, Lattice Boltzmann Method
















