# Felzenswalb Gruplama (Clustering) Algoritmasi

Imaji bolumlere ayirma (segmentation) baglaminda en iyi
algoritmalardan biri Felzenswalb algoritmasidir. Altta makalesi ve
isleyen C++ kodu paylasilmis,

http://cs.brown.edu/~pff/segment/ 

Scikit-image projesi ayni algoritmayi pakete dahil etmis, bu guzel
haber.

Ilginc bir durum: Koda yakinda bakilirsa, bir grafik bir de grafiksel
olmayan, pur kumeleme yapan ayri bolumleri oldugu
gorulebiliyor. Burada Felzenswalb'i kutlamak lazim, cok temiz bir kod
yazmis, neyin ne oldugu gayet bariz. Ilginc olan su, grafiksel olmayan
gruplama (clustering) kisminin islemek icin sadece noktalararasi
uzaklik bilgisine ihtiyaci var, bu verilmis ise pat diye (bazi basit
esik degerleri uzerinden) dogru kumeleri buluyor, gorsellik olsun
olmasin. Uzaklik bilgisi herhangi bir olcut olabilir, illa Oklitsel
olmasi gerekmez, eger veri tabaninda musteriler var ise, belki kosinus
uzakligi, ya da Jaccard, ya da bir baska sey.

Biz sunu farkettik ki, ufak bir takla ile gorsel olmayan ve cok iyi
isleyen kumeleme kismi cikartilabilir, ve ayri bir kumeleme
algoritmasi olarak kullanilabilir. Uzaklik bilgisini bir seyrek matris
(mtx) formatindan okuyacak bir ufak C++ eki yaptik, bu kod bilgiyi
alip Felzenswalb'in kodunu cagiriyor. Bu kodu ayri bir proje olarak
paylasiyoruz, 

https://github.com/burakbayramli/felzenszwalb

Felzenswalb imaj ayirma kodu da aslinda benzer bir isi yapiyor ama
bunu imajlar uzerinde yapiyor (cunku ana amaci bu), yani imaj renkleri
uzerinden bir uzaklik yaratiyor ve onu kodun geri kalanina
aktariyor. Biz sadece ic gruplama kismini genellestirdik.

Bu sayede O(E log E) cetrefilliginde isleyen ve optimal kumeleri bulan
(kume sayisini tanimlamaya gerek yok, mesela KMeans'de oldugu gibi)
bir algoritma elde etmis olduk. Acikcasi bu kadar basit bir kullanimin
niye baskalari tarafindan yapilmamis oldugunu anlayamadik. Herhalde ya
yapildi ama paylasilmiyor, ya da hakikaten gozden kacirilmis.

Altta uzaklik bilgisi uzerinden kumeleme yapan diger algoritmalarin
listesi goruluyor, mesela Affinity Propagation, ya da Spectral
Kumeleme. Onlara kiyasla Felzenswalb oldukca iyi durumda.

http://scikit-learn.org/stable/modules/clustering.html

Felzenswalb kumelemesi bu arada Minimum Kapsamli Agac (Minimum
Spanning Tree) kavramini kullanir ve aslinda Kruskal algoritmasinin
bir varyasyonudur.






