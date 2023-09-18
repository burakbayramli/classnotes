# Kümeleme (Clustering) ile Tavsiye Sistemi

Tavsiye sistemlerini kodlamanın klasik yolu kullanıcı-ürün matrisinde
(ki matris ögeleri beğeni notunu taşır) arama yapmaktır. Kendi
beğenilerim bu matris sidinda ayrı bir satır olarak düşünülebilir,
satırın tüm kullanıcılara olan mesafeisini [2] hesaplarım, bana en
yakın kullanıcıların en iyi not verdiği ürünleri tavsiye alırım.

Bu teknik tüm matrisi tarar, onun tüm satırlarına bakar. Beğeni
matrisi seyrektir tabii ki bu aramayı hızlandırır fakat yine de
tavsiye sistemi tüm veriye sahip olmalıdır. 

Bir diğer yöntem kullanıcı satırlarını kümelere ayırarak beğenileri
birbirine benzeyen kişileri aynı grup altında toplamak. Böylece her
küme için onu temsil eden bir küme merkezi vektörü elde edebiliriz, ve
yeni kullanıcının bu merkezlere olan mesafesini hesaplarız sadece,
10-20 tane küme için, ve sonra kümedeki kullanıcıları listeleyip
onların beğenilerini tavsiye olarak veririz.

Ornek veri [1] sitesinden, oradaki `ml-latest-small.zip` verisini alacagiz.
Daha buyuk bir veri seti `ml-25m.zip` icinde.

Fakat veri setinde ratings.csv dosyasi su formatta,

```
userId,movieId,rating,timestamp
1,296,5.0,1147880044
1,306,3.5,1147868817
1,307,5.0,1147868828
1,665,5.0,1147878820
```

Aynı kullanıcı kimliği birden fazla farklı satırda çıkıyor. Satır
satır işleme bazlı yapan yaklaşımlar için bir kullanıcının tüm
verisini aynı satırda almak daha iyi olabilir, böylece azar azar
(incremental) işlem birimi kullanıcı haline gelir, ayrıca parallel
işlem gerektiğinde aynı kullanıcı veisinin farklı işlemlere
dağıtılması gerekmez, bunun kordinasyonu zor olurdu. O zaman üstteki
veriyi kullanıcı satırı haline getirelim, ayrıca verinin seyrekliğini
gözönünde bulundurursak, not verilen filmlerin bir sözlük öğesi olarak
görmek uygun olabilir. Sözlük metin formatta JSON olarak tutulabilir.
Yani yeni veride iki kolon olacak, birinci `userİd` ikinci bir JSON
metni. Değişimi yapalım,


```python
import json, csv

d = "/opt/Downloads/ml-latest-small"
outdir = "/tmp"
infile = d + "/ratings.csv"
outfile = outdir + "/ratings-json.csv"
curruser = 0
row_dict = {}

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









```python
s = np.zeros((3,3,))
N = np.zeros((3,3,))

s[0,2] = 4; s[1,1] = 4; s[2,1] = 7

N[0,2] = 2; N[1,1] = 4; N[2,1] = 7

print (s)
print (N)

print (s/N)
print (np.divide(s, N, out=np.zeros_like(N), where=N>0))
```

```text
[[nan nan  2.]
 [nan  1. nan]
 [nan  1. nan]]
[[0. 0. 2.]
 [0. 1. 0.]
 [0. 1. 0.]]
```





```
movie,rating
Swordfish (2001),5
"Rock, The (1996)",5
Dunkirk (2017),2
```


[devam edecek]

Kaynaklar

[1] https://grouplens.org/datasets/movielens/

[2] <a href="../../../stat/stat_137_collab/toplu_tavsiye__collaborative_filtering__filmler_svd_ile_boyut_indirgeme.html">Toplu Tavsiye (Collaborative Filtering), Filmler, SVD ile Boyut İndirgeme</a>

