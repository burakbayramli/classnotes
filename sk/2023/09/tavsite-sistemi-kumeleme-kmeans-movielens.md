# Kümeleme (Clustering) ile Tavsiye Sistemi

Tavsiye sistemlerini kodlamanın klasik yolu kullanıcı-ürün matrisinde
(ki matris ögeleri beğeni notunu taşır) arama yapmaktır. Kendi
beğenilerim bu matris dışında ona benzer ama ayrı bir satır olarak
düşünülebilir, bu satırın tüm kullanıcılara olan mesafesini [2]
hesaplarım, bana en yakın kullanıcıları bulurum, ve onların en
beğendiği ürünleri tavsiye alırım.

Bu teknik tüm matrisi tarar, onun tüm satırlarına bakar. Beğeni
matrisi seyrektir tabii ki bu aramayı hızlandırır fakat yine de
tavsiye sistemi tüm veriye sahip olmalıdır. 

Bir diğer yöntem kullanıcı satırlarını kümelere ayırarak beğenileri
birbirine benzeyen kişileri aynı grup altında toplamak. Böylece her
küme için onu temsil eden bir küme merkezi vektörü elde edebiliriz, ve
yeni kullanıcının bu merkezlere olan mesafesini hesaplarız sadece,
10-20 tane küme için, ve sonra kümedeki kullanıcıları listeleyip
onların beğenilerini tavsiye olarak veririz.

Örnek veri [1] sitesinden, oradaki `ml-latest-small.zip` verisini
alacağız.  Daha büyük bir veri seti `ml-25m.zip` içinde. Veride bazı
önişlemler hala lazım, veri setinde `ratings.csv` dosyası mesela şu
formatta,

```
userId,movieId,rating,timestamp
1,296,5.0,1147880044
1,306,3.5,1147868817
1,307,5.0,1147868828
1,665,5.0,1147878820
```

Aynı kullanıcı kimliği birden fazla farklı satıra dağılmış
halde. Satır satır işleme bazlı yapan yaklaşımlar için bir
kullanıcının tüm verisini aynı satırda almak daha iyidir, böylece azar
azar (incremental) işlem birimi kullanıcı olur, tek satır okuyunca tek
bir kullanıcı işlemiş oluruz. Ayrıca parallel işlem gerektiğinde aynı
kullanıcı veisinin farklı süreçlere dağıtılması gerekmez, bunun
kordinasyonu zor olurdu. O zaman üstteki veriyi kullanıcı bazlı hale
getirelim, ayrıca verinin seyrekliğini gözönünde bulundurursak, not
verilen filmleri sözlük öğesi yapalım. Sözlük, metin formatta JSON
olarak tutulabilir, dosyaya böyle yazılır.  Yeni veride iki kolon
olacak, birincisi `userId`, ikincisi ise bir JSON metni. Değişimi
yapalım,


```python
import json, csv, subprocess, os

indir = "/opt/Downloads/ml-latest-small"
outdir = "/tmp/movie"
infile = indir + "/ratings.csv"
outfile = outdir + "/ratings-json.csv"
curruser = 0
row_dict = {}

if os.path.exists(outdir) == False: os.mkdir(outdir)

fout = open(outfile, "w")
with open(infile) as csvfile:   
    rd = csv.reader(csvfile,delimiter=',')
    headers = {k: v for v, k in enumerate(next(rd))}
    for row in rd:
        if row[headers['userId']] != curruser:
            fout.write(str(curruser) + "|")
            fout.write(json.dumps(row_dict))
            fout.write("\n")
            fout.flush()
            curruser = row[headers['userId']]
            row_dict = {}       
        row_dict[int(row[headers['movieId']])] = float(row[headers['rating']])
fout.close()

def show_out(n,m):
    fin = open(outfile)
    for i,line in enumerate(fin.readlines()):
        if i>=n and i<= m: print (line[:30],"..")
        if i>m: break

def ls(dirname):
   cmd = "/bin/ls -s1 " + dirname
   process = subprocess.Popen(cmd.split(" "), stdout=subprocess.PIPE, stdin=subprocess.PIPE, stderr=subprocess.STDOUT, shell=False)
   output, err = process.communicate()
   res = str(output).split("\\n")
   for x in res: print (x)   
```

```python
show_out(2,6)
```

```text
2|{"318": 3.0, "333": 4.0, "17 ..
3|{"31": 0.5, "527": 0.5, "647 ..
4|{"21": 3.0, "32": 2.0, "45": ..
5|{"1": 4.0, "21": 4.0, "34":  ..
6|{"2": 4.0, "3": 5.0, "4": 3. ..
```

Kullanıcı `4` film `21` için 3 notunu vermiş.. böyle gidiyor.

### Kümeleme

K-Means algoritmasının ana mantığı, paralel versiyonu [3,4]'te
işlendi.

Algoritmaya geçmeden önce bazı hazırlık işlemleri yapalım. Filmlerin tabandaki
kimlik no'ları matris üzerinde kullanıma hazır değil, onlara 0'dan başlayan bir
artarak giden kimliği biz atayacağız, ve aradaki eşlemeyi bir sözlük ile
yapacağız, sözlükler `movie_id_int.json` ve `movie_title_int.json` içinde
olacak. 

Ayrıca K-Means'te bir ilk fitilin yakılması, önyükleme (boostrap)
gerekir, çünkü algoritma verili etiketler üzerinden küme merkezleri,
verili küme merkezleri üzerinden etiket eşlemesi hesaplar, özyineli
bir algoritmadir, fakat ilk başlangıçta bu iki bilgide mevcut
değildir, bu durumda bir yerden başlanılması gerekir, çoğu yaklaşım
mesela küme merkezlerini rasgele atar. Bu örnekte küme merkezi yerine
etiket ataması (hangi kullanıcı hangi kümeye ait) rasgele yapılırsa
daha iyi, çünkü seyrek veri durumunda reel sayılar olan küme
merkezlerini iyi seçememek mümkün. Belli bir aralıktaki tam sayı küme
ataması daha basit, `cluster_ass` değişkeninde bu atama olacak.

```python
import os, numpy as np, json, pandas as pd

K = 5 # kume sayisi
M = 9742 # film sayisi
U = 610 # kullanici sayisi

np.random.seed(0)

def prepare():

    df = pd.read_csv(indir + "/movies.csv")
    d = df.reset_index().set_index('movieId')['index'].to_dict()
    fout = open(outdir + "/movie_id_int.json","w")
    fout.write(json.dumps(d))
    fout.close()

    df = pd.read_csv(indir + "/movies.csv")
    d = df.reset_index().set_index('title')['index'].to_dict()
    fout = open(outdir + "/movie_title_int.json","w")
    fout.write(json.dumps(d))
    fout.close()
    
    cluster_ass = np.random.randint(K,size=U)
    np.savez(outdir + '/cluster-assignments-0',cluster_ass)

prepare()
```

Bu kodu işlettikten sonra gerekli başlangıç dosyaları `outdir` içinde
olmalı.  Şimdi işletimi sağlayacak yardımcı kodu paylaşalım, detaylar
[4] içinde bulunabilir, ileride paralel işletimi de sağlayacak,


```python
import os

def process(file_name,N,hookobj):
    file_size = os.path.getsize(file_name)
    beg = 0
    chunks = []
    for i in range(N):
        with open(file_name, 'r') as f:
            s = int((file_size / N)*(i+1))
            f.seek(s)
            f.readline()
            end_chunk = f.tell()-1
            chunks.append([beg,end_chunk])
            f.close()
        beg = end_chunk+1
    c = chunks[hookobj.ci]
    with open(file_name, 'r') as f:
        f.seek(c[0])
        i = 0
        while True:
            i += 1
            line = f.readline()
            hookobj.exec(line)
            if f.tell() > c[1]: break
        f.close()
        hookobj.post()        
```

Artık işleyici kodları yazabiliriz. Bize iki tane ayrı kod bloğu
gerekiyor, bunlardan ilki daha önce söylediğimiz gibi verili küme
atamalarını kullanarak küme merkezlerini hesaplar. İkincisi küme
merkezlerini kullanarak kullanıcı küme atamalarını yapar. Atama verisi
basit tek bir vektör içinde, `U` tane kullanıcı (seyirci), `K` tane
küme var, o zaman `(1,M)` boyutunda, içeriği 0 ile `K` arasında bir
değer, ilk atamaların rasgele yapıldığı vektöre bakalım,

```python
ca = np.load(outdir + "/cluster-assignments-0.npz")['arr_0']
print ('atamalar',ca.shape)
print (ca[:10], '...')
```

```text
atamalar (610,)
[4 0 3 3 3 1 3 2 4 0] ...
```

İkinci kod parçası atamaları kullanıp küme merkezlerini yaratır, bu
merkez bir küme içindeki seyircilerin tüm filmlere verdiği notların
ortalamasıdır, bu tüm kümeler için yapılır o zaman sonuç matrisinin
boyutu `K` küme `M` film için K x M olmalı. Her iki kod parçası
alttadır,


```python
import numpy.linalg as lin
fin1  = outdir + "/ratings-json.csv"

np.random.seed(0)

class KMeans1Job:
    def __init__(self,ci,iter_no):
        self.ci = ci
        self.iter_no = iter_no
        self.movie_id_int = json.loads(open(outdir + "/movie_id_int.json").read())
        self.cluster_ass = np.load(outdir + "/cluster-assignments-%d.npz" % int(self.iter_no-1))['arr_0']
        self.s = np.zeros((K,M))
        self.N = np.zeros((K,M))
                
    def exec(self,line):
        id,jsval = line.split("|")
        ratings = json.loads(jsval)
        my_k = int(self.cluster_ass[int(id)])
        for mov,rating in ratings.items():
            self.s[my_k, self.movie_id_int[mov]] += rating
            self.N[my_k, self.movie_id_int[mov]] += 1.0
                
    def post(self):
        means = np.divide(self.s, self.N, out=np.zeros_like(self.s), where=self.N>0)
        np.savez(outdir + '/means-%d' % self.iter_no, means)
        print ('KMeans1Job tamam')

class KMeans2Job:
    def __init__(self,ci,iter_no):
        self.ci = ci
        self.iter_no = iter_no
        self.means = np.load(outdir + "/means-%d.npz" % int(self.iter_no))['arr_0']
        self.movie_id_int = json.loads(open(outdir + "/movie_id_int.json").read())
        self.cluster_ass = np.zeros((U,))

        
    def exec(self,line):
        id,jsval = line.split("|")
        ratings = json.loads(jsval)
        vec = np.zeros(M)
        for mov,rating in ratings.items():
            vec[self.movie_id_int[str(mov)]] = rating
        nearest = np.argmin(lin.norm(vec[vec>0] - self.means[:,vec>0],axis=1))
        self.cluster_ass[int(id)] = nearest
        
    def post(self):
        np.savez(outdir + '/cluster-assignments-%d' % self.iter_no,self.cluster_ass)
        print ('KMeans2Job tamam')
        
```

Ortalamalar

`KMeans1Job` kodunda ortalamalar için her küme için o küme altındaki
kullanıcıların notları, yani vektörler toplanır, aynı sırada kaç not
verilmiş olduğu takip edilir, ve toplam not sayısına bölünür. Sıfır
ile bölüm tehlikesi var muhakkak, bu durumda özel bir bölme çağrısı
kullanacağız, `numpy.divide`, bir örnek altta,


```python
s = np.zeros((3,3,))
N = np.zeros((3,3,))

s[0,2] = 4; s[1,1] = 4; s[2,1] = 7

N[0,2] = 2; N[1,1] = 4; N[2,1] = 7

print (s)
print (N)
print ('\n')
print (s/N)
print (np.divide(s, N, out=np.zeros_like(N), where=N>0))
```

```text
[[0. 0. 4.]
 [0. 4. 0.]
 [0. 7. 0.]]
[[0. 0. 2.]
 [0. 4. 0.]
 [0. 7. 0.]]


[[nan nan  2.]
 [nan  1. nan]
 [nan  1. nan]]
[[0. 0. 2.]
 [0. 1. 0.]
 [0. 1. 0.]]
```

Mesafe

Devam etmeden önce önemli bir konuya değinelim, bu da mesafe
kavramı. K-Means kümelemesi Öklit (Euclidian) mesafe kullanır, bu
mesafenin sıfır değerlerini nasıl kullandığına dikkat etmek
gerekir. Sıfır değeri tabii ki bizim seçimlerimiz bağlamında
"seyredilmemiş" demektir. Fakat Öklitsel mesafe hesaplıyorsam, mesela
benim üç boyutlu vektörüm x1,y1,z1 ile başka bir x2,y2,z2 vektör
arasında bu hesap (x1-x2) karesi artı (y1-y2) karesi vs diye devam
ediyor. Sonra bu toplamın karekökü alınıyor. Fakat eğer bende x filmi
seyredilmemişse, yoğun matris bağlamında orada sıfır vardır, ama küme
merkezine uzaklık hesaplıyorsam bu merkezin x2 (ve diğer
y2,z2).. öğesinde muhakkak bir değer vardır. Bu mevcut değere uzaklık
hesaplarsam bendeki sıfır değeri mevcut değeri beni uzakmışım gibi
gösterecektir. Fakat belki o filmi seyretsem belli bir kümenin
değerine yakın not verirdim, o zaman ona yakın gözükürdüm. Demek ki
bendeki boş değerler mesafesel olarak problem çıkartabilir. İşte bu
sebeple `KMeans2Job` kodunda mesafe hesaplarken `vec[vec>0] -
self.means[:,vec>0]` kodunu kullandık, yani `vec` üzerinde sıfır
*olmayan* değerlerin `self.means` ile mesafesine bakılıyor. Diğerleri
Öklit mesafesine dahil edilmiyor.

Kodu işletelim, şimdilik iki kod parçasını tek bir döngü içinde çağıracağız,

```python
process(file_name = outdir + '/ratings-json.csv', N=1, hookobj = KMeans1Job(0,1))
process(file_name = outdir + '/ratings-json.csv', N=1, hookobj = KMeans2Job(0,1))
```

```text
KMeans1Job tamam
KMeans2Job tamam
```

```python
ls (outdir)
```

```text
b'total 2156
   8 cluster-assignments-0.npz
   8 cluster-assignments-1.npz
 384 means-1.npz
 140 movie_id_int.json
 344 movie_title_int.json
1272 ratings-json.csv

```

Görüldüğü gibi `-1.npz` ile biten dosyalar yaratıldı, bu dosyaların
1'inci döngünün sonuçları. Boyutlara bakıyoruz,

```python
ca = np.load(outdir + "/cluster-assignments-1.npz")['arr_0']
means = np.load(outdir + "/means-1.npz")['arr_0']
print ('atamalar',ca.shape)
print ('kume merkezleri',means.shape)
```

```text
atamalar (610,)
kume merkezleri (5, 9742)
```

### Kullanım

Döngüyü birkaç kez işletelim,

```python
for iter_no in range(1,10):
    process(file_name=fin1, N=1, hookobj = KMeans1Job(0,iter_no))
    process(file_name=fin1, N=1, hookobj = KMeans2Job(0,iter_no))
```

Bunu yapmak sonuçları iyileştirir. Kendi seçtiğimiz bazı filmlere
notlar verelim, ve bu seçimlere yakın olan kümeyi bulalım,

```python
picks = """
movie,rating
Swordfish (2001),5
"Rock, The (1996)",5
Dunkirk (2017),2
"""

import io

dt = json.loads(open(outdir + "/movie_title_int.json").read())
means = np.load(outdir + "/means-9.npz")['arr_0']
picks = pd.read_csv(io.StringIO(picks),index_col=0).to_dict('index')
vec = np.zeros(M)
for mov,rating in picks.items():
    if str(mov) in dt:
        vec[dt[str(mov)]] = rating['rating']
nearest = np.argmin(lin.norm(vec[vec>0] - means[:,vec>0],axis=1))
print ('en yakin', nearest)
```

```text
en yakin 3
```

En yakın kümeyi bulduk. Şimdi not verdiğimiz her filmin tüm kümelerde
ortalama nasıl notlanmış olduğuna bakalım, acaba bizim notlarla uyumlu
mu?

```python
means[:,dt['Swordfish (2001)']]
```

```text
Out[1]: array([2.25      , 3.5       , 3.33333333, 3.2       , 2.875     ])
```

```python
means[:,dt['Rock, The (1996)']]
```

```text
Out[1]: array([3.8125    , 3.3125    , 3.58064516, 3.85714286, 3.5       ])
```

```python
means[:,dt['Dunkirk (2017)']]
```

```text
Out[1]: array([3.375     , 3.75      , 5.        , 3.16666667, 3.        ])
```

Uyumlu gözüküyor.

Paralel işletme kısmını ödev bırakıyoruz, burada eklenecek kodların
genel yaklaşımından bahsedelim. Ortalama için her paralel işletici
kullanıcıların bir kısmını işler, ve o kısımla ilgili küme
ortalamalarını hesaplar. İki tane süreç iki tane K x M ortalama
yaratır, o zaman her döngüde paralel süreç ortalamaları bitince seri
şekilde (paralel değil) bu ortalamaların ortalamaları alınır, o
döngünün nihai ortalaması bu olur.

Küme ataması daha bariz, çünkü zaten kullanıcı bazlı hesaplanan ve
atanan bir değer, eh bizim paralel yaklaşım da kullanıcı bazlı
işbölümü yaptığına göre burada tek yapılması gereken paralel atama
bitince her süreçten gelen sonuçları birleştirmektir, yani ucu uca
getirip yapıştırmak (concatanate), bu kadar.

Not: K-Means algoritmasi nereden başlanırsa başlansın yakınsama
yapabilen (convergent) bir algoritmadır, bir optimal noktaya muhakkak
ulaşır. Fakat bu optimal nokta yerel (local) optima olabilir, genel,
kullanışlı optima olmayabilir. Bu durumda farklı yerlerden (birkaç
farklı rasgele noktadan) algoritmaya birkaç kere işletip nereye
ulaştığına bakmak bir ek yöntem olabilir.

Kaynaklar

[1] https://grouplens.org/datasets/movielens/

[2] <a href="../../../stat/stat_137_collab/toplu_tavsiye__collaborative_filtering__filmler_svd_ile_boyut_indirgeme.html">Toplu Tavsiye (Collaborative Filtering), Filmler, SVD ile Boyut İndirgeme</a>

[3] <a href="../../../algs/algs_080_kmeans/kmeans_kumeleme_metodu.html">K-Means Kümeleme Metodu</a> 

[4] <a href="../../2022/11/paralel-veri-analizi-istatistik.html">Paralel Veri Analizi, İstatistik</a>



