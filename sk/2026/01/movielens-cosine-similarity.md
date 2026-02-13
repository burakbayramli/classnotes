# Movielens Filmleri, Kosinüs Benzerliği, Tavsiyeler

Kosinüs benzerliği konusunu [1]'de işledik, ölçüt iki vektörün
birbirine çok boyutlu açısal yakınlığını hesaplar. Bunun için tek
gereken bu iki vektörün arasındaki noktasal çarpım, ve her iki
vektörün büyülüklerinin (norm) çarpımı. Aynı belgede bu hesabın hızlı
yapılabilmesi için seyrek matris kavramından bahsedildi. Fakat aslında
biz python kütüphanelerinden gelen seyrek matris formatları yerine
kendi seyrek matris / vektör yapımızı da oluşturabiliriz.

İlerlemeden önce noktasal çarpımın ne olduğunu hatırlayalım, aynı
boyuttaki iki vektörün birbirine tekabül eden her değeri çarpılır ve
bu çarpımlar birbirine toplanır, lineer cebirde $a \cdot b$ ile
gösterilen islem, ve hesapsal olarak şöyle yapılabilir,

```python
a = np.array([2,4,6,7,4,9,4,3,2,5,6,3,1,2])
b = np.array([1,3,6,8,1,1,4,3,3,4,1,5,4,9])

float (np.sum([x*y for x,y in zip(a,b)]))
```

```text
Out[1]: 213.0
```

Üstteki kodda `zip` ile her iki vektörün öğeleri gezildi, ve tarif
edilen işlemler yapıldı.

Şimdi Movielens film not veri tabanını düşünelim. Kullanıcılar
filmlere not verirler, eğer bir matriste kullanıcılar satırda filmler
kolonda ise bir kullanıcının tüm notlarını bir vektör, diğer bir
kullanıcının notlarını diğer bir vektör olarak elde edebilirdik, ve
yakınlık için noktasal çarpımı bu iki vektör üzerinde
gerçekleştirirdik. Eğer 2000 kullanıcı 1000 tane film var ise bir
kullanıcını vektörü 1 x 1000 boyutunda olurdu.

Tabii kayıtlarda binlerce film olacaktır, ve herhangi bir kullanıcının
tüm filmleri seyredip not vermiş olması imkansız, hatta seyretmiş olsa
bile not vermemiş olabilir. Çoğunlukla bir kullanıcı 10-15 filme not
vermiştir, belki yüzlerce. Her durumda da not sayısı azdır. Yani
üstteki örnek veri aslında şuna benzeyebilir,

```python
a = np.array([2,0,6,0,0,0,0,0,0,0,0,0,0,0,3,1,2])
b = np.array([0,3,1,0,0,0,0,0,0,0,3,3,4,1,1,2,0])

float (np.sum([x*y for x,y in zip(a,b)]))
```

```text
Out[1]: 11.0
```

Görüldüğü gibi her iki vektörde de bir sürü sıfır var. Noktasal
çarpımda sıfır çarpı herhangi bir değer sıfır olduğu için, herhangi
bir hücrede sıfır varsa o noktadaki hesap sıfır olur, toplama etkisi
olmaz. Ve dikkat üstteki örnekte sıfır olmayan çoğu değer çakışmıyor
(ancak o zaman toplama katkı olabilir), mesela `a` 1'inci öğe 2 ama
`b` aynı öğe sıfır, `b` 2'inci öğe 3 ama onun eşi `a` uzerinde sıfır,
demek ki ilk iki öğenin toplama hiçbir katkısı olmayacak. Keşke sıfır
olan hücreleri direk atlayabilseydik, değil mi? Seyrek matris
formatları aslında bunu yapmamızı sağlar. Kütüphane `scipy.sparse`
içinde bu amaç için pek çok kodlar vardır. Fakat biz kendi
pişirdiğimiz kodlar ile de aynı sonucu elde edebiliriz.

Bir vektör yerine bir sözlük (dictionary) yapısı kullanarak bunu
yapabilirdik. Sözlük anahtarı üstteki vektörde sıfır olmayan değerin
indisi olabilir. Yeni "vektörlerimiz" alttaki gibi olur,

```python
da = dict({0: 2, 2:6, 14:3, 15:1, 16:2})
db = dict({1: 3, 2:1, 10:3, 11:3, 12:4, 13:1, 14:1, 15:2})
```

Görüldüğü gibi, mesela `da` içinde `1`, `3`, .. gibi anahtar değerleri
yok, bu anahtarlar atlandı çünkü değerleri sıfır. Şimdi noktasal
çarpım mantığı şöyle kodlanabilir: sözlüklerden birini al, tüm
öğelerini gez, gezerken bu anahtar değeri diğer sözlükte var mı diye
kontrol et, varsa çarpımı yap, toplama ekle, yoksa sıfır değerini
ekle. Dikkat edilirse *gezilen* sözlük zaten sıfır olan değerleri
bastan içermediği için onları dolaylı olarak zaten atlamış oluyoruz.


```python
res = sum(da[key]*db.get(key, 0) for key in da)
print (res)
```

```text
11
```

Aynı sonucu aldık. Bir kodlama püf noktası, diğer sözlükte anahtar
erişimi `.get` ile yaptık, ve bu `get` bir şey bulamadıysa sıfır
döndürsün diye bir "yokluk olağan değerini" bu çağrıya bildirdik,
çünkü


```python
print (da.get(5))
```

```text
None
```

olurdu, ama alttaki kullanımla,

```python
print (da.get(5, 0))
```

```text
0
```

elde edilir, böylece olmama durumunu sıfır değerine çevirip nihai
çarpımı sıfırlamış oluyoruz.

Kodun optimal olduğunu görebiliriz. Gezilen ana sözlükte olmayan
değerler (verilmemiş notlar) hiç gezilmiyor, çünkü onlar sözlük içinde
bile değiller. Eğer çakışma yoksa `get` sıfır döndürüyor, toplama
etkisi olmuyor.  Hesapsal yük açısından `get` çağrısı çok hızlıdır,
sabit zamanda işler. Gezilen değerlerin okunması aynı şekilde.

Üstteki algoritmaya farklı şekilerde de yardım edebiliriz. Mesela biz
örnek veride `da` sözlüğünü gezdik (onun tüm öğelerini okuduk), bunu
yapmamızın sebebi `da` sözlüğünün `db`'den daha küçük olmasıdır. Eğer
verimizde bir tarafın her zaman diğer taraftan daha ufak vektörler
vereceğini biliyorsak, bu bilgiyi koda gömebiliriz. Film tavsiyesi
bağlamında benim kayıtlarımda not verdiğim 1000 üzerinde film var.  Bu
demektir ki benim beğeni vektörüm çoğu zaman karşılaştırma yaptığım
Movielens tabanındaki diğer kullanıcıların beğeni vektörlerinden büyük
olacaktır. Bu durumda kendim için bir noktasal çarpım kodluyorsam,
algoritmayi her zaman diğer kullanıcının vektörünü gezecek şekilde
kodlamalıyım.

Altta tarif edilen algoritmanın kodlaması var, bu algoritma büyük
Movielens tabanı [2] üzerinden film tavsiyeleri üretiyor. Benim şahsi
seçimlerim `movpicks.csv` içinde. Kod benim seçimlerimi kullanarak
bana en yakın kullanıcıları bulur ve o en yakın kullanıcıların 4 ve
daha üzeri not verdiği filmleri toparlayarak benim için bir tavsiye
listesi oluşturur.

```python
content = open("simrecom.py").read()
print(content.replace('\n', '\r\n'))
```

```text
#
# Recommend movies based on Grouplens ratings file
#
# https://grouplens.org/datasets/movielens/latest/
#
# Download the full file, and unzip in a known location set in d
#
from scipy.sparse import csr_matrix
import scipy.sparse.linalg, json
import pandas as pd, numpy as np
import os, sys, re, csv

csv.field_size_limit(sys.maxsize)

d = "/opt/Downloads/ml-32m"

def sim_recommend():
    fin = d + "/user_movie.txt"
    picks = pd.read_csv(os.environ['HOME'] + '/Documents/kod/movpicks.csv',index_col=0).to_dict('index')
    skips = pd.read_csv(os.environ['HOME'] + '/Documents/kod/movskips.csv',index_col=0).to_dict('index')
    mov = pd.read_csv(d + "/movies.csv",index_col="title")['movieId'].to_dict()
    genre = pd.read_csv(d + "/movies.csv",index_col="movieId")['genres'].to_dict()
    mov_id_title = pd.read_csv(d + "/movies.csv",index_col="movieId")['title'].to_dict()
    picks_json = dict((mov[p],float(picks[p]['rating'])) for p in picks if p in mov)
    picks_norm = np.sqrt(sum(v**2 for v in picks_json.values()))
    res = []
    with open(fin) as csvfile:   
        rd = csv.reader(csvfile,delimiter='|')
        for i,row in enumerate(rd):
            jrow = json.loads(row[1])
            jrow_norm = np.sqrt(sum(v**2 for v in jrow.values()))
            dp = sum(jrow[key]*picks_json.get(int(key), 0) for key in jrow)
            dp = dp / ((picks_norm*jrow_norm)+1e-10)
            res.append([row[0],dp])
            if i % 1e4 == 0: print (i,dp)
          

    df = pd.DataFrame(res).set_index(0)
    df = df.sort_values(by=1,ascending=False).head(400)
    df = df.to_dict()[1] # the final list of close users

    recoms = []
    with open(fin) as csvfile:   
        rd = csv.reader(csvfile,delimiter='|')
        for i,row in enumerate(rd):
            jrow = json.loads(row[1])
            # if the user exists in the closest users list
            if str(row[0]) in df:
                # get this person's movie ratings
                for movid,rating in jrow.items():
                    if int(movid) not in mov_id_title: continue 
                    fres = re.findall('\((\d\d\d\d)\)', mov_id_title[int(movid)])
                    if rating >= 4 and \
                       mov_id_title[int(movid)] not in picks and \
                       mov_id_title[int(movid)] not in skips and \
                       'Animation' not in genre[int(movid)] and \
                       'Documentary' not in genre[int(movid)] and \
                       len(fres)>0 and int(fres[0]) > 2010: \
                       # add the picks of this user multiplied by his closeness
                       recoms.append([mov_id_title[int(movid)],rating*df[row[0]]])

    df = pd.DataFrame(recoms)
    df = df.sort_values(1,ascending=False)
    df = df.drop_duplicates(0)
    df.to_csv("/opt/Downloads/movierecom3.csv",index=None,header=False)
        
if __name__ == "__main__":      
    sim_recommend()
                

```

Üstteki kod Movielens 32M verisinin `rating.csv` dosyasından bir
`user_movie.txt` dosyası üretilmiş olduğunu farz ediyor. Bu yeni dosya
içeriği daha önce tarif ettiğimiz sözlük yapısının dosyalaşmış
halidir, her satırda bir kullanıcı vardır ve her kullanıcının verdiği
notlar o satırda bir JSON formatında paylaşılır, `{'film1': not,
"film10': not}` şeklinde. Bildiğimiz gibi JSON formatı bir sözlüğün
diske yazılması için kullanılabilmektedir. Değişim kodu alttaki
bağlantıda paylaşılıyor.

Kodlar

[movprep.py](movprep.py)


Kaynaklar

[1] Bayramli, <a href="../../../stat/stat_137_collab/stat_137_collab.html">Toplu Tavsiye (Collaborative Filtering), Filmler, SVD ile Boyut İndirgeme</a>

[2] Netflix, <a href="https://grouplens.org/datasets/movielens/32m/">MovieLens 32M, (ml-32m)</a>
