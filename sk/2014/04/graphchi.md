# GraphChi

Bu acik yazilim paketini ilk kez Buyuk Veri Berlin isimli bir meetup
toplantisinda duyduk. Prezentasyon Meetup.com uzerinden organize
edilen toplantilardan biriydi, bu tur ufak birliktelikler, ozellikle
Web uzerinden, cok rahat yapilabiliyor. Bir sirketten oda, digerinden
yiyecek, icecek destegi alininca is tamam - meetup.com kayitlari
hallediyor, ve toplanti basliyor. 

Buyuk Veri Berlin toplantisinda konuk Peter Wang'dir, Wang unlu Python
bilimsel hesap kutuphanesi Scipy, Numpy'i yazan Enthought'tan bilinen
bir kisilik, sonra oradan ayrilip ayri bir sirket Continuum'u
kuranlardan. Continuum ozellikle Anaconda surum kontrol sistemi ile
kendinden bahsettiriyor.

Wang'in prezentasyonu surada:

http://www.slideshare.net/kammeyer/big-data-berlin

Sayfa 16'da GraphChi'den bahsediyor, Twitter verisini analiz eden bir
algoritma (yavas) bir stilde kodlaninca 1000 makina ile 400 dakika
gerektirirken, GraphChi ayni islemi tek makina uzerinde 59 dakikada
isleyebilmektedir! Google'in unlu PageRank algoritmasi GraphChi
uzerinden 2 CPU uzerinde 790 saniyede, tam elli kat CPU (100 tane)
iceren Spark kumesi ise bu islem ancak iki kati kadar hizli
islenebilmektedir! Kissadan hisse, algoritmadan algoritmaya fark
vardir, ve akilli algoritma fark yaratir. GraphChi ozellikle ag yapisi
/ cizit / grafik isleme acisindan ilerlemeler iceriyor, azar azar
isleme ve bunu ozel bir sekilde yaparak hizli cozumler getiriyor.

Bizim bahsetmek istedigimiz GraphChi'nin tavsiye modulleri. Paket
icinde Netflix yarismasindan tanidik gelebilecek tum algoritmalar
var. Birkac turlu SVD kodlanmis, SVD'nin ek kullanici bilgilerini
kullanmasini saglayan SVD++ mesela (ki kazanan BellKor takimi bu
yaklasimi kullandi, ve Netflix'in de tavsiye sisteminde algoritmayi
kullandigini biliyoruz), ya da SVD'ye zaman bilincini ekleyen Timed
SVD++  eklenmis , RBM (restricted boltzman machines) mevcut, ve daha
az bilinen CLIMF yaklasimi. Bu algoritmalarin kullanimi oldukca basit,
indirilip

https://github.com/GraphChi

bash install.sh ve make ile derlenir, sonra

```
wget http://www.select.cs.cmu.edu/code/graphlab/datasets/smallnetflix_mm 
wget http://www.select.cs.cmu.edu/code/graphlab/datasets/smallnetflix_mme
```

ile kucuk veri seti indirilir, ki bu dosyalar text dosyalari, icinde her satirda

[kullanici] [film] [not]

formatinda veriler bulunabiliyor. Isletmek icin

```
[GRAPHCHI]/toolkits/collaborative_filtering/svd --training=smallnetflix_mm  --validation=smallnetflix_mme --nsv=3 --nv=10 --max_iter=10 --quiet=1 --tol=1e-1
```

Bu kod mevcut dizin icinde U,V matrislerini temsil eden iki dosya
yaratacak. SVD yontemini daha once isledik, bu iki matrisle artik
herhangi bir kullanici / film kombinasyonu icin bir tahmin hesabi
uretebilirsiniz.

Akla su soru gelebilir, GraphChi (ve onun buyuk abisi GraphLab) ag
isleme paketleridir, peki matris ayristirmasi ya da yapay sinir aglari
alanindan bilinen RBM yaklasiminin ag yapisi islemekle ne alakasi
olabilir? Ag yapisi analizi mesela Facebook'da arkadas gruplarini
belirleyen (community detection) gibi problemlerin alani degil midir?
Bu dogru, fakat bir acidan dusunursek tavsiye sistemlerinde A
matrisinin satirlari musteri, kolonlari filmler, musteri / film
kesistigi yerde -not degeri oldugu noktada- bunu bir "musteri
dugumunden", "film dugumune" bir baglanti olarak gorebiliriz ve
yapinin tamami bir ag gibi kabul edilip islenebilir.

Cikti Dosyalari

GraphChi algoritmalari girdi olarak alinan dosyalarin oldugu dizine ciktilari yazarlar, bu ciktilar mesela svdpp algoritmasi icin _U.mm, _V.mm, U_bias.mm gibi soneki tasiyan dosyalar olacaktir. Dosyalarin formati Market Matrix formati denen bir format, scipy.io.mmread bu dosyalari okuyabilir.

```
from scipy.io import mmread
x = mmread('...')
```

gibi bir ibare yeterli. Dikkat: tek bir puruz _U.mm gibi kordinat formatinda degil yogun (dense) formatta olan matrislerde. Aslinda bu formatta x,y boyutlari belirtildikten sonra tum veri tek bir kolonda verilir, GC projesindekiler veriyi tum kolonlara yaymislar. Tabii goze guzel gozukuyor, fakat mm formatina uygun degil. Bu sebeple mmread U,V matrislerini yukleyemiyor. Cozum olarak bana GC'de bir satir degistirip tekrar derlemem tavsiye edildi, biz de oyle yaptik. Dosya toolkits/collaborative_filtering/io.hpp icinde 208. satir,

```
if (j == actual_Size -1)
```

comment edin (yani iptal edin) ve tekrar derleyin. 

GraphChi forumu

http://forum.graphlab.com/

GC yazari D. Bickson forumu cok aktif sekilde takip ediyor bu arada,
ne sorulursa (tabii cevap vermek istediklerine) cat diye hemen cevap
alirsiniz.





