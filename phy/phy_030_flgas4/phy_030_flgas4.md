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















[devam edecek]

Kaynaklar

[1] Anderson, Computational Fluid Dynamics, The Basics With Applications

