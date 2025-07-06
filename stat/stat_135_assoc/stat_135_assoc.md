# İlişkisel Madencilik (Association Mining)

İkisel Matris Ayrıştırması (Binary Matrix Factorization)

Veri madenciliği denince pek çok kişinin aklına gelen ilk örnek, aslında,
sık bulunan öğe kümeleri (frequent itemsets) örneğidir: "filanca ülkeden
sitemize gelen müşterilerin aynı zamanda vs özelliklerinin olduğunu da
keşfettik" gibi. 

Benzer bir örnek, ki bu alan öğe kümelerinin aslında en önemli çıkış
sebeplerinden birisidir, alışveriş sepeti analizidir. Müşterinin her
alışverişinde sepetinde belli mallar vardır, ve bu malların hangilerinin aynı
anda, aynı sepette olduğu analiz edilmeye uğraşılır. Eğer sürekli ekmek ve reçel
aynı anda alınıyorsa, bu bilgi kullanılarak belki malların daha iyi
konumlandırılması yapılacaktır, vs. Sık bulunan öğe kümeleri teknikleri bazen
değişik adlar altında da geçebiliyor, mesela ilişki madenciliği (association
mining) gibi. Algoritma olarak kullanılan pek çok teknik var, APriori iyi
bilinenlerden, FPGrowth ondan daha hızlı çalışan ve daha tercih edilen bir
teknik. İstatistiki bir teknik olan Çok Boyutlu Bernoulli Karışımları da bu
alanda kullanılan bir yaklaşım.

Bir diğer alternatif ikisel matris ayrıştırması (binary matrix
factorization -BMF-) kullanmaktır [3]. Aynen SVD'de olduğu gibi BMF de bir
matrisi ayrıştırır, fakat üç matris yerine iki matrise ayrıştırır ve hem
sonuç matrisi hem de ayrıştırılan matrisler sadece 0 ya da 1 değerini
taşıyabilirler. Yani bu ayrıştırma sonuç matrislerinin ikisel olmasını
mecbur tutar, negatif olmayan matris ayrıştırmasının (non-negative matrix
factorızation) sonuç matrisinin pozitif değerler taşımasını mecbur kılması
gibi. Bunlar birer kısıtlama (constraint) ve bu sonuç o kısıtlamalara göre
ortaya çıkıyor. *Dikkat*: BMF için toplama işlemi $1+0 = 1, 1+1=1, 0+0
= 0$ olarak tekrar tanımlanır, yani mantıksal OR işlemi haline gelir.

Ayrıştırma öncesi hangi kerte (rank) $k$ değerine geçmek istediğimizi biz
belirtiriz. BMF'nin öğe kümeleri madenciliği için faydası şurada: öğe
kümeleri ararken baktığımız öğeler kategorik şeylerdir, alışveriş sepeti
örneğinde mesela ekmek, reçel gibi. Kategorik öğeleri daha önce 1-hot
kodlaması (encoding) ile 1/0 değerleri taşıyan yeni kolonlara
geçirebildiğimizi görmüştük. Yani tamamen kategorik değerler taşıyan
veriler tamamen 1/0 taşıyacak şekilde tekrar kodlanabilir, yani ikisel
matris haline getirilebilir. Bu ikisel matrisi ayrıştırdığımız zaman ve
kendileri de ikisel olan iki yeni matris elde ettiğimizde ise bir anlamda
boyut indirgemesi yapmış oluruz, yani sanki ana matrisi "özetleriz''. İşte
bu özet, özellikle çarpılan "baz'' matris, öğe kümelerinin hangileri
olduğu hakkında ipuçları içeriyor olabilir.

Bir örnek üzerinde görelim, mesela altta Alice (A), Bob Marley (B) ve Prens
Charles (C) verileri var. Bu kişiler için saçı uzun mu (long-haired), ünlü
mü (well-known) ve bay mı (male) verileri var. 

![](abc.png)

Bu matris üzerinde ikisel ayrıştırma yaparsak, $k=2$

![](abc_res.png)

Eğer kontrol etmek istersek, matris çarpımı yapmamız gerekir, bunun için

```python
a = np.array([[1,  0],
               [1,  1],
               [0,  1]], dtype=bool)
b = np.array([[1,  1,  0],
               [0,  1,  1]], dtype=bool)

print (np.dot(a,b))
```

```
[[ True  True False]
 [ True  True  True]
 [False  True  True]]
```

0 ve 1 değerleri görmek için 1 ile çarpmak yeterli

```python
print (1*np.dot(a,b))
```

```
[[1 1 0]
 [1 1 1]
 [0 1 1]]
```

Sonuç başlangıç matrisi ile aynı, demek ki `bool` tipi matris
tanımlayınca Numpy çarpımı `dot`, çarpım sırasındaki toplama işlemi
için aritmetik toplama yerine VEYA (OR) kullanması gerektiğini anladı.

Şimdi ayrıştırmayı analiz edelim, özellikle sol taraftaki çarpılan "baz''
matrise bakalım.. [6] yazısından hareketle, bu yazıdaki kolon kombinasyon
bakışını kullanalım (tabii toplamanın BMF için OR olduğunu unutmadan), o
zaman soldaki baz matrisin dikey, kolon bazlı olarak, bir özet olduğunu
görebiliyoruz. Çünkü çarpan sağ taraf bu kolonları alıp onları belli
şekillerde "kombine ederek'' nihai (orijinal) matrisi ortaya
çıkartabilmeli. Bu sebeple soldaki çarpılan matris bir özet olmalı / baz
oluşturmalı, ve bunun yan etkisi olarak kolonlardaki değerlerde belli bir
kalıp / örüntü (pattern) olmalı. O zaman her baz kolonunda birbiriyle
alakalı olan ögeler aynı anda 1 değeri taşıyor olacaktır.

Sonuca göre uzun saçlı ve ünlü olmak (1. kolon) arasında bağlantı varmış ,
ayrıca erkek olmak ve ünlü olmak (2. kolon) arasında da bağlantı varmış :)
Veriye göre böyle en azından.. Bu sonucu orijinal matrise bakarak ta
kontrol edebiliriz.

Ayrıştırma Kodlaması 

BMF özel bir hesaptır ve Numpy / Scipy içinde mevcut değildir, ayrı bir
kütüphane kullanmak gereklidir, `nimfa` paketi içinde gerekli kodlar
var. Kurduktan sonra üstteki örneği şöyle çözebiliriz;

```python
import nimfa
import pandas as pd
import scipy.sparse as sp

threshold = 0.1

A = np.array([[1., 1., 0],
              [1., 1., 1.],
              [0, 1., 1.]])

nmf_model = nimfa.Bmf(A, rank=2, seed='nndsvd', max_iter=40, lambda_w=1.1, lambda_h=1.1)
nmf = nmf_model()
res1 = nmf.basis()
res2 = nmf.coef()
	      
print (res1)
res1 = np.abs(np.round(res1 - 0.5 + threshold))
res2 = np.abs(np.round(res2 - 0.5 + threshold))
res1 = pd.DataFrame(res1, index=['long-haired','well-known','male'])
res2 = pd.DataFrame(res2, columns=['A','B','C'])
print (res1)
print ('\n')
print (res2)
```

```
             0  1
long-haired  1  0
well-known   1  1
male         0  1


   A  B  C
0  1  0  0
1  0  1  1
```

Sonuç neredeyse tıpatıp aynı; sadece çarpan matriste [0,B] kordinatı 1
değil, fakat bize lazım olan baz matris aynı çıktı. 

BMF hakkında bazı ek bilgiler: [2]'ye göre en az hatalı BMF hesaplamak
NP-hard zorluğunda, yani 3SAT gibi, ya da Seyahat Eden Satış Elemanı
(Traveling Salesman) problemi gibi ki bu problemler kombinatoryel
(combinatorial) optimizasyon problemleridir; çözüm için tüm olasılıklar
denendiği ve kısayolun mevcut olmadığı çeşitten problemler. Fakat
yaklaşıksal BMF metotları oldukça hızlıdır, ayrıca seyreklik çok fark
yaratıyor (pozitif anlamda) ki kategorik veriler gerçek dünyada çoğunlukla
seyrek olarak görülüyor. Eldeki 2000 tane mal çeşidi içinden bir sepette
ancak 5-10 tane ürün oluyor mesela, tüm 2000 tane malı bir sepete koymak
mümkün değil.

FPGrowth


Öğe kümeleri bulmak için BMF haricinde bir yöntem FPGrowth yöntemidir
[1,2]. Bu yöntem önce her ögeden (tek başına) kaç tane olduğunu sayar,
belli bir eşik değeri `minsup` altında olanları atar, sonucu
sıralar. Bu liste bir yapısına işaret eden bir başlık yapısı haline
gelir. Ağacın kendisini oluşturmak için veri satırları teker teker işlenir,
her satırdaki her öge için başlık yapısındaki en fazla değeri taşıyan öğe
önce olmak üzere tepeden başlanıp alta doğru uzayan bir ağaç yapısı
oluşturulur. Ağaçtaki her düğüm altındaki düğümün sayısal toplamını
taşır. Madencilik için alttan başlanarak yukarı doğru çıkılır (amaç en üste
ulaşmak) ve bu sırada öğeler `minsup` altında ise, atılırlar. Sonuçta
ulaşılan ve atılmayan yollar bir öğe kümesini temsil ederler. 

Örnek verisi olarak alttakini kullanalım,

```python
data = [
['outlook=sunny', 'temparature=hot', 'humidity=high', 'windy=false', 'play=no'],
['outlook=sunny', 'temparature=hot', 'humidity=high', 'windy=true', 'play=no'],
['outlook=overcast', 'temparature=hot', 'humidity=high', 'windy=false', 'play=yes'],
['outlook=rainy', 'temparature=mild', 'humidity=high', 'windy=false', 'play=yes'],
['outlook=rainy', 'temparature=cool', 'humidity=normal', 'windy=false', 'play=yes'],
['outlook=rainy', 'temparature=cool', 'humidity=normal', 'windy=true', 'play=no'],
['outlook=overcast', 'temparature=cool', 'humidity=normal', 'windy=true', 'play=yes'],
['outlook=sunny', 'temparature=mild', 'humidity=high', 'windy=false', 'play=no'],
['outlook=sunny', 'temparature=cool', 'humidity=normal', 'windy=false', 'play=yes'],
['outlook=rainy', 'temparature=mild', 'humidity=normal', 'windy=false', 'play=yes'],
['outlook=sunny', 'temparature=mild', 'humidity=normal', 'windy=true', 'play=yes'],
['outlook=overcast', 'temparature=mild', 'humidity=high', 'windy=true', 'play=yes'],
['outlook=overcast', 'temparature=hot', 'humidity=normal', 'windy=false', 'play=yes'],
['outlook=rainy', 'temparature=mild', 'humidity=high', 'windy=true', 'play=no']
]
```

Hava ile alakalı bazı veriler [1] bunlar; bu veriler tahmin (outlook),
sıcaklık (temparature), nem (humidity), rüzgar (windy), dışarıda oyun
oynayan var mı (play). Mesela ilk satırda tahmin güneşli, ısı sıcak, nem
yüksek, rüzgar yok ve oyun oynayan yok. Bu şekilde bir sürü satır. Biz bu
veride bir kalıp olup olmadığına bakacağız. [2]'deki kodu [1]'den aldığımız
üstteki veriye uygularsak, sonuç şöyle:

```python
import fp
items = fp.fpgrowth(data, minsup=6)
for x in items:
    if len(x) > 1: print (x)
```

```
<fp.node instance at 0x5017ef0>
   Null Set   1
     play=yes   9
       humidity=high   1
         windy=true   1
           temparature=mild   1
       windy=false   6
         humidity=high   2
           temparature=mild   1
         humidity=normal   4
           temparature=mild   1
       humidity=normal   2
         windy=true   2
           temparature=mild   1
     humidity=high   2
       windy=true   2
         temparature=mild   1
     windy=false   2
       humidity=high   2
         temparature=mild   1
     humidity=normal   1
       windy=true   1
   Null Set   1
     play=yes   6
   Null Set   1
     play=yes   6
set(['play=yes', 'humidity=normal'])
set(['play=yes', 'windy=false'])
```

Bulunan sonuçlar iki tane (tek öğeli sonuçlar da var ama onları
eledik). Bunlar hakikaten veri içindeki kalıpları temsil ediyorlar. Fena
değil. 

Kıyas için BMF üzerinden madencilik yapalım. Önce 1-hot kodlaması yapalım,
ve örnek için bir veri satırını ekrana basalım,

```python
from sklearn.feature_extraction import DictVectorizer
import pandas as pd, re

def one_hot_dataframe(data, cols, replace=False):
    vec = DictVectorizer()
    mkdict = lambda row: dict((col, row[col]) for col in cols)
    tmp = data[cols].apply(mkdict, axis=1)
    vecData = pd.DataFrame(vec.fit_transform(tmp).toarray())
    vecData.columns = vec.get_feature_names_out()
    vecData.index = data.index
    if replace is True:
        data = data.drop(cols, axis=1)
        data = data.join(vecData)
    return (data, vecData, vec)

cols = ['outlook','temparature','humidity','windy','play']
df = pd.DataFrame(data,columns=cols)
# kolon ismini veriden cikart, cunku tekrar geri koyulacak
# fpgrowth icin veri icinde olmasi lazim
df = df.map(lambda x: re.sub('.*?=','',x))
df2, _, _ = one_hot_dataframe(df, cols, replace=True)
# tek ornek ekrana bas
print (df2.iloc[0])
```

```
humidity=high       1
humidity=normal     0
outlook=overcast    0
outlook=rainy       0
outlook=sunny       1
play=no             1
play=yes            0
temparature=cool    0
temparature=hot     1
temparature=mild    0
windy=false         1
windy=true          0
Name: 0, dtype: float64
```

Şimdi BMF işletelim, $k=4$

```python
import nimfa
import scipy.sparse as sp

nmf_model = nimfa.Bmf(np.array(df2).T, rank=4, seed='nndsvd', max_iter=40, lambda_w=1.1, lambda_h=1.1)
nmf = nmf_model()
res1 = nmf.basis()
res2 = nmf.coef()
res1 = np.abs(np.round(res1 - 0.5 + threshold))
res2=  np.abs(np.round(res2 - 0.5 + threshold))
res1 = pd.DataFrame(res1,index=df2.columns)
print (res1)
```

```
                  0  1  2  3
humidity=high     1  0  0  1
humidity=normal   0  1  0  0
outlook=overcast  0  0  1  0
outlook=rainy     1  0  0  0
outlook=sunny     0  0  0  1
play=no           0  0  0  1
play=yes          0  1  1  0
temparature=cool  0  0  0  0
temparature=hot   0  0  0  0
temparature=mild  1  0  0  0
windy=false       0  0  1  0
windy=true        1  0  0  0
```

Bu sonuçları kategoriksel hale çevirip tekrar ekrana basalım,

```python
for i in range(4):
    print (np.array(df2.columns)[res1.iloc[:,i] == 1])
```

```
['humidity=high' 'outlook=rainy' 'temparature=mild' 'windy=true']
['humidity=normal' 'play=yes']
['outlook=overcast' 'play=yes' 'windy=false']
['humidity=high' 'outlook=sunny' 'play=no']
```

1. sonuç atlanabilir, buradaki "kalabalık'' orada bir kalıp olmadığına
dair bir işaret. Ayrıştırma sonucu bu tür kolonlar ortaya çıkabilir, diğer
kolonlardaki kalıplar bütünü temsil etmeye *tam* yetmemişse, arta kalan
her türlü gereklilik bir yerlere tıkılabiliyor, bu normal. 2. sonuç
FPGrowth sonucunda var, güzel. 3. sonuç ta neredeyse aynı, sadece ek olarak
`outlook=overcast` var. Fakat, 3. sonuç aslında önemli bir kalıp
içeriyor olabilir, yani kalması daha iyi olur.

4. sonuç ise çok önemli bir kalıp ve FPGrowth bunu tamamen kaçırmış!

Sebep FPGrowth'un çözüme lokal olarak erişmeye çalışıyor olması, kıyasla
BMF bütüne (global) bakıyor [3]. Bu ne demektir? Bir ayrıştırmanın ne
olduğunu düşünürsek, bir matrisi oluşturan çarpımı ayrıştırıyoruz ve bu
ayrıştırma olduktan sonra iki matris elde ediyoruz. Bu iki matris özgündür 
(unique). Yani belli bir ikisel matrisi oluşturan çarpım sadece tek bir
şekilde olabilir. Buradan hareketle diyebiliriz ki bu ayrıştırma bütünü
göze alarak yapılmalıdır, sağı, solu tutan ama köşesi tutmayan bir
ayrıştırma olmaz. Bu sebeptendir ki ayrıştırma çözümünden belli bir
kapsayıcılık bekleyebiliriz.

FPGrowth ise olaya yerel bakıyor; ağaç oluştururken değişik bir sıra takip
edilirse mesela değişik ağaçlar ortaya çıkabilir. Ayrıca her önemli ilişki
muhakkak özgün bir dal yapısında olmayabilir. Madencilik algoritması alt
dallardan başlar ve yukarıya doğru çıkar, fakat bu her zaman iyi bir yöntem
midir?

Kodlama Notları

Şu kod `np.round(num - 0.5 + threshold)` kullanımı yuvarlama
(rounding) yapıyor, çünkü Nimfa 1 değeri yerine 0.9, 0.8 gibi değerler
üretebiliyor, ayrıca 0.1 gibi değerler de oluyor. Biz bildiğimiz yuvarlama
`.5`  sonrası üzerini 1 yapmak yerine belli bir eşik değeri
(threshold) üzerinden yuvarlama yaptık. Yani eşik=0.2 ise 0.7 alta
yuvarlanır ve 0 olur, 0.9 eşik üstünde olduğu için üste yuvarlanır 1 olur.

BMF için kerte $k$ kullanıcı tarafından seçilmeli, ama bu durum SVD, ya da
GMM ile kümeleme gibi diğer yapay öğrenim metotlarından farklı değildir. Bu
oynanması gereken, keşfedilmesi gereken bir değer.

İlginçlik - İstatistiki Ölçüt

Kümeleri uydurduktan sonra bile bu kümelerin içinde hangisinin "daha iyi''
olduğunu bulmak için istatistiki ölçüt kullanmak faydalı olabilir. Hatta
birazdan bahsedeceğimiz teknik aslında her türlü ilişki madenciliği
yaklaşımı için faydalı, çünkü hangi teknik olursa olsun bize verinin belli
bir grubunu "önemli'' olarak gösterecek. Ardından biz bu grubu alıp onun
ne kadar önemli olduğunun ölçütünü hesaplayabileceğiz.

Teknik şöyle: İstatistiki testlerden [9] yazı bölümünü hatırlarsak, bir
ideal dağılım vardı, ve eldeki verinin bu ideale olan yakınlığını
ölçüyorduk. Chi Kare testi ayrıksal bazda işliyordu, eğer eldeki sürekli
fonksiyon bazlı bir dağılım ise onun ideal hesaplarını kutucuklara
bölüştürüyorduk.

İlişkisel madencilikte elde ettiğimiz kural bir vektör içinde 0/1 değerleri
olacak. Yaklaşım şöyle; önce verideki her kolonun tek başına oranını
buluruz. Bu oranlar her kolon "dağılımının'' birbirinden bağımsız farz
edildiği "idealize'' ortamın ölçütleri olacaklar. Veri mesela şöyle,

```python
data = [[1,1,0,0,1],
        [1,0,0,0,0],
        [1,0,0,1,1],
        [1,1,0,1,1],
        [1,1,1,0,1],
        [0,0,1,1,0],
        [0,1,1,0,0]
        ]
data = np.array(data)
sums = data.sum(axis=0)
means = data.mean(axis=0)
print ('toplam', sums)
print ('ortalama', means)
```

```
toplam [5 4 3 3 4]
ortalama [ 0.71428571  0.57142857  0.42857143  0.42857143  0.57142857]
```

Şimdi bulunan kurallardan birini, diyelim `[1,1,0,0,1]`, ana veride en
fazla 1 sayısına tekabül eden kolonunu seçeriz, ve bu kolonun 1 olduğu tüm
satırları bir alt küme olarak toparlarız. Bu alt kümede diyelim 5 tane
satır var, ve kuralın diğer ögeleri 1. haricinde 2. ve 5.  kolonun da '1'
değerinde olması. O zaman, toplam 5 satır için 2. ve sonuncu satırda 5*0.57
ve 5*0.57 tane satır olmalı. Sıfır hipotezi bağımsızlık olduğu için bu
"beklenen (expected)'' sayı. Diğer yandan gerçek rakamlar var, bu rakamlar
alt kümedeki '1' değerlerinin toplamı, ki bu da "görünen (observed)''
sayı. Bu iki vektör üzerinden chi kare değerini hesaplıyoruz [5, sf. 391],

$$ \chi^2 = \sum_i \frac{(O_i-E_i)^2}{E_i} $$

$\chi^2$'nin serbestlik derecesi 3-1=2 (çünkü kuralda 3 tane kolon var,
1. kolonu alt kümeyi bulmak için kullandık). p-değeri ne kadar yüksek ise
kural o kadar ilginç diyebiliriz.

```python
from scipy.stats.distributions import chi2

def interesting(rule): 
     idx = (sums*rule).argmax()
     subset = data[data[:,idx] == 1]
     print (subset)
     print (subset[:,rule==1])
     obs = subset[:,rule==1].sum(axis=0)
     exp = len(subset)*means[rule==1]
     print ('gorunen (observed)', obs)
     print ('beklenen (expected)', exp)
     chi = np.sum((obs-exp)**2 / exp)
     dof = rule.sum()-1
     print (1-chi2.cdf(chi,dof))

rule = np.array([1,1,0,0,1])
interesting(rule)
```

```
[[1 1 0 0 1]
 [1 0 0 0 0]
 [1 0 0 1 1]
 [1 1 0 1 1]
 [1 1 1 0 1]]
[[1 1 1]
 [1 0 0]
 [1 0 1]
 [1 1 1]
 [1 1 1]]
gorunen (observed) [5 3 4]
beklenen (expected) [ 3.57142857  2.85714286  2.85714286]
0.595795886519
```

Bir başka kural deneyelim, 

```python
rule = np.array([1,0,0,0,1])
interesting(rule)
```

```
[[1 1 0 0 1]
 [1 0 0 0 0]
 [1 0 0 1 1]
 [1 1 0 1 1]
 [1 1 1 0 1]]
[[1 1]
 [1 0]
 [1 1]
 [1 1]
 [1 1]]
gorunen (observed) [5 4]
beklenen (expected) [ 3.57142857  2.85714286]
0.310494434317
```

Bu daha az ilginçmiş. Hakikaten de ilk kuralın veriye bakarak daha ilginç
olduğunu söyleyebiliriz. 

Gösterdiğimiz tekniği film sonuçlarında kullanmadık, bunu ödev olarak
okuyucuya bırakıyoruz.

Kaynaklar

[1] Ian H. Witten, Eibe Frank, Mark A. Hall, *Data Mining Practical Machine Learning Tools and Techniques*

[2] Harrington, P., *Machine Learning in Action*

[3] Miettinen, *Boolean Matrix Factorizations*, [http://www.mpi-inf.mpg.de/~pmiettin/slides/BooleanMatrixFactorizationsForDataMining_Antwerp_slides.pdf](http://www.mpi-inf.mpg.de/~pmiettin/slides/BooleanMatrixFactorizationsForDataMining_Antwerp_slides.pdf)

[4] Zip boundary, *ZIP Code FAQs*, [http://www.zipboundary.com/zipcode_faqs.html](http://www.zipboundary.com/zipcode_faqs.html)

[5] Rao, *Linear Statistical Inference and Its Applications*

[6] Bayramlı, Lineer Cebir, *Matris Çarpımı, Ders 1*

[7] Bayramlı, Istatistik, *Pivotlama*

[8] Bayramlı, Istatistik, *Çok Değişkenli Bernoulli Karışımı*

[9] Bayramlı, Istatistik, *Pearson Chi Kare Uyum Derecesi Testi*




