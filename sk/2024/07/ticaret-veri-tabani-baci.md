# Ticaret Veri Tabanı, Önişlemler, Raporlar

200 ülke için ikili ticaret ilişkilerinin verisi B.A.C.I. tabanında
paylaşılıyor [1].  Bu yazıda tabanın 2022 yılı için olan verisini
işleyeceğiz, veri 5000 ürün kategorisini içermeke, kategori sistemi
"harmonize edilmiş sistem" adı verilen 6 sayılık bir kod
kullanıyor. BACI kayıtları alan / satan bilgini içerir, yani her ülke
ikilisi için ihraç edici / ithal edici kayıtlanmıştır, ve bu ilişkide
her ürünün ihraç/ithal miktarı ve değeri de tabanda vardır. Taban tek
bir csv dosyası, kolonlar,

```
t: yıl
i: ihraççı
j: ithalcı
k: ürün
v: değer
q: miktar
```

Para deger birimi 1000 Amerikan doları, miktar ise metrik ton.

Girdi verisine bir göz atalım,

```python
baci_dir = "/opt/Downloads/baci"

!head -10 $baci_dir/BACI_HS22_Y2022_V202401b.csv
```

```text
t,i,j,k,v,q
2022,4,20,210610,        0.357,        0.002
2022,4,20,210690,        0.061,        0.001
2022,4,20,271000,        5.807,        8.103
2022,4,20,843131,        0.313,        0.022
2022,4,31,080211,        2.027,        0.500
2022,4,31,081310,        0.912,        0.240
2022,4,31,252620,        5.642,       47.550
2022,4,31,340600,        0.167,        0.002
2022,4,31,680221,        5.034,       21.000
```

Kayıtları işlemek için [2]'deki paralel, satırsal işleyici altyapısını
kullanacağız.  CSV dosyasında her satırda ihraç eden ülke / ithal eden
ülke / ürün üçlüsü var. Alttaki kod her satırı okurken iki ülke
arasındaki değeri en yüksek ürünü takip ediyor. Bu takip basit bir
Python sözlüğü ile yapılıyor, sözlükteki anahtar iki kimliğin
birleşimi, ihraççı - ithalci ile bir özgün, birleşik kimlik
yaratılıyor, bu kimliğin değeri o iki ülke arasındaki o ana kadar
kayıtlarda görülen en yüksek değeri taşıyor, eğer yeni satırda daha
yüksek değer bulunursa sözlük güncelleniyor. Diğer bir sözlükte bu
ikili için o değerin tekabul ettiği ürünün anahtarı kayıt ediliyor.


```python
from timeit import default_timer as timer
from multiprocessing import Process
from datetime import timedelta
import util, json, os

class BaciJob:
    def __init__(self):
        self.infile = "" 
        self.chunk = -1 
        self.P = {}
        self.V = {}
        self.header = {'t':0,'i':1,'j':2,'k':3,'v':4,'q':5}
    def exec(self,line):        
        tok = line.strip().replace(' ','').split(',')
        i,j = tok[self.header['i']], tok[self.header['j']]
        i,j = int(i),int(j)
        v = float(tok[self.header['v']])
        q = tok[self.header['q']]
        prod = tok[self.header['k']]
        key = "%d-%d" % (i,j)
        if key not in self.V or v > self.V[key]:
            self.V[key] = v
            self.P[key] = prod
            
    def post(self):
        print (self.infile)
        fout = open(baci_dir + "/out-p.json","w")
        fout.write(json.dumps(self.P))
        fout.close()
        
        fout = open(baci_dir + "/out-v.json","w")
        fout.write(json.dumps(self.V))
        fout.close()

def process_baci_top_products():
    file_name = baci_dir + "/BACI_HS22_Y2022_V202401b.csv"
    start = timer()
    N = 1 
    ps = [Process(target=util.process,args=(file_name, i, N, BaciJob(),1)) for i in range(N)]
    for p in ps: p.start()
    for p in ps: p.join()
    end = timer()
    print('elapsed time', timedelta(seconds=end-start))
```

```python
process_baci_top_products()
```

```text
elapsed time 0:01:41.059329
```

İşlem bitince sonuçlar `baci_dir` dizini altında yazılmış olmalı. Referans edilen
`process` kodu [util.py](util.py) içinde.

Çıktıları raporlamak için yeni iki fonksiyon yazalım, bu
fonksiyonlardan `baci_top_product()` verili iki ülke için, ihraccı /
ithalci olarak, en değerli ürün kategorisini ve o kategorinin tanmını
geri getirsin. Bu bilgilerin raporlanabilmesi için bazı referans csv
dosyaları okunmalı, ardı ardı yapılacak fonksiyon çağrılarında sürekli
okunmasın diye onları otomatik olarak önbellege gönderiyoruz,
`@lru_cache` kullanımına dikkat, `init()` ilk çağrıldığında dört tane
DataFrame'i diskten okuyup geri döndürür, sonraki çağrılarda
önbellekteki objeler diske gitmeden geri verilir.

```python
from functools import lru_cache
import pandas as pd, json, textwrap

@lru_cache(maxsize=1) # returned types are cached
def init():
   baci_cc = pd.read_csv(baci_dir + '/country_codes_V202401b.csv',index_col='country_name')
   baci_pc = pd.read_csv(baci_dir + '/product_codes_HS22_V202401b.csv',index_col='code')
   baci_p = json.loads(open(baci_dir + "/out-p.json").read())
   baci_v = json.loads(open(baci_dir + "/out-v.json").read())
   return baci_cc, baci_pc, baci_p, baci_v

def baci_top_product(frc, toc):
    baci_cc, baci_pc, baci_p, baci_v = init()
    key = "%d-%d" % (baci_cc.loc[frc].country_code, baci_cc.loc[toc].country_code)
    print('$', f"{baci_v[key]*1000:,}")
    s = baci_pc.loc[int(baci_p[key])].description
    for x in textwrap.wrap(s, width=70):
    	print (x)
    
```

Çin'in Fransa'ya ihraç ettiği en çok para getiren ürünü nedir?

```python
baci_top_product("China","France")
```

```text
$ 979,185,844.0
Communication apparatus (excluding telephone sets or base stations):
machines for the reception, conversion and transmission or
regeneration of voice, images or other data, including switching and
routing apparatus
```

Rusya ve Hindistan,

```python
baci_top_product("Russian Federation","India")
```

```text
$ 25,534,776,517.0
Oils: petroleum oils and oils obtained from bituminous minerals, crude
```

Kaynaklar

[1] <a href="http://www.cepii.fr/cepii/en/bdd_modele/bdd_modele_item.asp?id=37)">BACI</a>

[2] Bayramlı, [Paralel, Satır Bazlı Dosya İşlemek](../../2016/02/toptan-islemler-paralelizasyon.html)

