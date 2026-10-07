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













[devam edecek]

Kaynaklar

[1] Anderson, Computational Fluid Dynamics, The Basics With Applications

