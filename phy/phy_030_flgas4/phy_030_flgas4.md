# Sıvı ve Gaz Mekaniği - 4

Amacımız üç boyutlu Euler denklemlerini türetmek, bu formüller gaz
mekaniğini hesaplayabilir. Bu formülasyon için yapılan bazı
faraziyeler: gazın çalkantılı (unsteady), sıkıştırılabilir, ideal
akışkan / ağdasız (inviscid), sıcaklık iletmeyen, birörnek maddeden
oluşan, kimyasal reaksion içermeyen ve gövde kuvvetlerine maruz
olmayan (yerçekimi gibi) bir ortam olduğu. Bu faraziye bizi tam
tekmilli Navier-Stokes denklemlerinden daha basitleştirilmiş Euler
denklemlerine getiriyor.

Gövde kuvvetlerini yok saydık, eğer yerçekimi ya da dışarıdan başka
bir kuvvet mevcut olsaydı bu terimlerin formülün sağ tarafına eklenmesi
gerekirdi, fakat onları yok sayınca tek hesaba katılması gereken
basınç kuvvetidir. Bu önemli bir basitleştirmedir ve formülün nihai
yalın formuna ulaşılması için önemli bir tekniktir.

Formülasyonu gerçekleştirmek için kullanacağımız önemli bir kavram
ufak kontrol hacmi kavramı. Uzayda sabitleştirilmiş boyutları $\ud x,
\ud y, \ud z$ olan kenarları dikdörtgensel olan bir kutu (box) hayal
edelim, bu kutunun hacmi tabii ki $dV=\ud x \ud y \ud z$ olur. Dikkat,
burada anahtar kelime *sabitlenmiş*. Sıvı bu sabit kutunun içinden
akıyor, kutunun kendisi hareket etmiyor.

Sıvının (gazın) her noktasındaki büyüklükler şunlar,

* yoğunluk $\rho(x,y,z,t)$;
* basınç $p(x,y,z,t)$;
* hız $V=(u,v,w)$
* spesifik iç enerji $e$.

Ulaşmak istediğimiz bir muhafaza kanunu olacak, bu sebeple içinden
sıvının geçtiği sabitlenmiş bir hacim seçtik, ve bu hacim içinde
muhafazasını garanti edeceğimiz büyüklükler $\rho,\rho u,\rho v,\rho
w,\rho E$ olacak. Bunlar nedir,

* $\rho$: birim hacimdeki kütle
* $\rho u$: birim hacimdeki $x$-momentum
* $\rho v:$ birim hacimdeki $y$-momentum 
* $\rho u$: birim hacimdeki $z$-momentum 
* $\rho E$: birim hacimdeki toplam enerji

Dikkat $E$ büyüklüğü toplam spesifik enerji, birimi birim kütledeki
(mass) toplam enerji, $E=\frac{\text{energy}}{\text{mass}}$,

$$
E=\frac{J}{kg}
=\frac{kg \cdot m^2}{s^2 \cdot kg}
=\frac{m^2}{s^2}.
$$

Metre ve saniye ile enerji bağlantısı ne olabilir diye düşünülürse
kinetik enerji hatırlanabilir, birim kütledeki kinetik enerji
terimleri şöyle olurdu,

$$
\frac{1}{2}v^2 \implies \left(\frac{\text{m}}{\text{s}}\right)^2 =
\frac{\text{m}^2}{\text{s}^2}
$$

$\rho E$'ye dönersek, $\rho$ birim hacimdeki kütle olduğuna göre yani
$\rho=\frac{\mathrm{kg}}{\mathrm{m^3}}$, o zaman $\rho$ ve $E$
çarpılınca

$$
\rho E
=
\frac{\mathrm{kg}}{\mathrm{m^3}}
\frac{\mathrm{J}}{\mathrm{kg}}
=
\frac{\mathrm{J}}{\mathrm{m^3}}
$$

Kütle Muhafazası

Fiziksel prensibi hatırlayalım, evrende kütle yeniden yaratılmaz ve
kaybolmaz. Bu prensip sebebiyle ufak kutumuza giren çıkan kütleleri
hesaba katmamız gerekir (dikkat kutu *içindeki* kütlenin
muhafazasından bahsedilmiyor), kutu sınırlarından giren ve çıkan kütle
o kutudaki kütle miktarını artırır ya da azaltır. Yani artan, azalan,
giren, her miktarların muhasebesi yapılmalıdır.

$$
\text{kutu içindeki kütle artış oranı} = \text{kutuya giren kütle oranı} - \text{çıkan kütle oranı}.
$$

Ya da

$$
\text{kütle artış oranı} +  \text{net kütle çıkış oranı} = 0
$$

Ufak kutumuzun içindeki kütle,

$$
\ud m=\rho \ud x \ud y \ud z
$$

Bu kütlenin artış oranı için eşitliğin sağ tarafının zamana göre kısmi
türevini alabiliriz,

$$
\frac{\partial\rho}{\partial t} \ud x \ud y \ud z.
$$

Şimdi üsttekinin neye eşit olduğuna / onun tanımına gelelim. Bu tanım
daha önce belirttiğimiz gibi kutuya giren ve çıkan kütle oranlarının
farkını kullanacak. Hesabı şöyle yapabiliriz. Önce $x$ tarafına bakan
dikdörtgeni alalım, alanı $\ud y \ud z$ olsun, oradan akan kütle
oranını hesaplayabiliriz. Bu yüzeye dikgen / normal olan akış $u$ ile
gösteriliyor, çünkü akış hız vektörünün bileşenlerini daha önce
göstermiştik, $V=(u,v,w)$, işte $u$ buradaki $u$. Bu hızı temel
alalım, $\ud t$ zamanı sonunda $u \ud t$ kadar kütle akmıştır
diyebiliriz.

O akmış olan kütlenin hacmini de hesaplayabiliriz, $\ud V = u \ud t
\ud y \ud z$, kütlesi $\ud m = \rho \ud V$, yani $dm = \rho u \ud t
\ud y \ud z$. Simdi her seyi $\ud t$ ile bölersek, $\frac{\ud m}{\ud
t} = \rho u \ud y \ud z$ elde ederiz, işte bu $\ud y \ud z$ alanından
akan kütle akış oranıdır. Bu yuzdeki akisa, sola bakan yuz diyebiliriz,

$$
(\rho u)_x \ud y \ud z
\tag{2}
$$

ismi verelim. Şimdi kutunun diğer yüzeyinden olan akışa bakalım,
üstteki $x$ noktasındaydı, şimdi $x+\ud x$ noktasındaki yüzeye
bakalım, bu da sağdaki yüz olabilir, oradaki akış için $(\rho u)_x$
ifadesini kullanarak Taylor açılımı yapabiliriz,

$$
(\rho u)_{x+\ud x} = (\rho u)_x + \frac{\partial(\rho u)}{\partial x}\ud x.
$$

Yani $f(x+dx)$ gibi bir ifade kullanmak yerine $f(x)$ içeren ve onun
yersel (spatial) türevini içeren bir formül kullanmak daha iyi oldu,
cebiri basitleştirdi. Matematiksel olarak Taylor açılımı kullanmak
uydundur çünkü $\ud x \to 0$ olduğu bir ortamda işleyecek bir
diferansiyel denklem türetiyorum, ve bu sebeple Taylor açılımındaki
daha yüksek dereceli terimleri yok sayabiliyorum.

Devam edelim, o zaman sağ yüzde akış,

$$
\left[
(\rho u)_x + \frac{\partial(\rho u)}{\partial x}\ud x
\right]
\ud y \ud z
\tag{3}
$$

Demek ki $x$ yönündeki net akış (2) eksi (3) ile hesaplanabilir,

$$
\left[ (\rho u)_x\,dy\,dz +
\frac{\partial(\rho u)}{\partial x}\ud x \ud y \ud z \right] -
(\rho u)_x \ud y \ud z
$$

$$
= \frac{\partial(\rho u)}{\partial x} \ud x \ud y \ud z.
$$

Benzer mantığı $y$ yönü için kullanırsak,

$$
\frac{\partial(\rho v)}{\partial y}
\ud x \ud y \ud z
$$

$z$ yönü için de

$$
\frac{\partial(\rho w)}{\partial z}
\ud x \ud y \ud z
$$

Şimdi nihai formüle gelelim, eğer net pozitif bir akış var ise bu
birim hacimimizden kütle çıkışı var demektir [1, sf. 55], bu bir
azalmayı temsil eder. Formülü o zaman şu şekilde oluşturmalıyız,

$$
\frac{\partial\rho}{\partial t}\ud x \ud y \ud z
=
- \frac{\partial(\rho u)}{\partial x}\ud x \ud y \ud z
- \frac{\partial(\rho v)}{\partial y}\ud x \ud y \ud z
- \frac{\partial(\rho w)}{\partial z}\ud x \ud y \ud z
$$

$\ud x \ud y \ud z$ ile bölersek, ve tüm terimleri bir tarafa alırsak,

$$
\frac{\partial\rho}{\partial t}
+
\frac{\partial(\rho u)}{\partial x}
+
\frac{\partial(\rho v)}{\partial y}
+
\frac{\partial(\rho w)}{\partial z}
=0.
$$

Üstteki ifadeyi uzaklaşım (divergence) operatörü ile göstermek daha
yalın formül verebilir,

$$
\textrm{div} (\rho\mathbf V) =
\nabla\cdot(\rho\mathbf V) =
\frac{\partial(\rho u)}{\partial x}
+
\frac{\partial(\rho v)}{\partial y}
+
\frac{\partial(\rho w)}{\partial z},
$$

Yani

$$
\frac{\partial\rho}{\partial t}
+
\nabla\cdot(\rho\mathbf V)
=0.
$$




[devam edecek]

Kaynaklar

[1] Anderson, *Computational Fluid Dynamics, The Basics With Applications*

