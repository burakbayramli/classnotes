# MrJob

Python bazli esle/indirge sureclerini hic Hadoop komut satiriyla ve
Streaming ile ugrasmadan isletmenin bir diger yolu mrjob (Amerikalilar
bunu "mister job" diye okuyor). Kurmak icin

```
sudo pip install mrjob
```

/home/hduser altinda .mrjob.conf adinda bir dosya yaratin ve icine sunu yazin

```
runners:  hadoop:
    hadoop_home: [HADOOP DIZINI] 
```

Mrjob ile esleme / indirgeme komutlari ayri script olarak degil, tek
bir class'ta ayri metotlar olarak verilir. Mesela kelime sayma

from mrjob.job import MRJob
import re
WORD_RE = re.compile(r"[\w']+")
class MRWordFreqCount(MRJob):
    def mapper(self, _, line):
        for word in WORD_RE.findall(line):
            yield (word.lower(), 1)
    def reducer(self, word, counts):
        yield (word, sum(counts))
if __name__ == '__main__':
    MRWordFreqCount.run()
```

Test amacli tek basina isletmek icin (Hadoop olmadan)

```
python words.py words.csv
```

Hadoop ile (once dosyayi kopyalayalim)

```
hadoop dfs -copyFromLocal words.csv  /user/
```

```
python /home/hduser/words.py hdfs:///user/words.csv -r hadoop --step-num=1
```

Ustteki komutun islemesi icin hduser olarak login etmeniz
gerekir. Eger baska bir kullanicidan isletmek isterseniz,

```
ssh localhost -l hduser [ustteki ayni komut]
```

isleyecektir. Kac tane esle / indirge'nin paralel isleyecegini
tanimlayabiliriz, bunun icin (mesela 2)

```
... --jobconf mapred.map.tasks =2 --jobconf mapred.reduce.tasks=2
```

kullanilir. 

Nasil Isliyor?

Peki motor kapaginin altinda neler oluyor? Mrjob, Hadoop Streaming'i
sarmalayarak bize daha rahat kullanimi olan bir arayuz
sunar. Streaming, bilindigi gibi HDFS'teki bir dosyayi okuyup
script'lerimize satir satir gecmekle sorumludur. Mrjob ise bunun
uzerine bir katman daha ekler, ve mesela dosyamizda tab, bosluk
(space) iyi bilinen ayraclar kullaniliyorsa daha bastan anahtar ve
degeri ayirip map cagrisina bunlari tek obje, obje listeleri olarak
gecebilir.

Dahasi da var: map icinden uretilen anahtar ve deger ikilileri
cetrefil tipler olabilir (vektor, sozluk gibi) ve bu ciktilar
Python'un yerli (native) tipleri olarak indirgeyiciye
gonderilebilirler. Direk Hadoop'ta bu is oldukca zor olurdu. Bu
iletisim icin

```
from mrjob.protocol import PickleProtocol
..
class BizimMR(MRJob):
    INTERNAL_PROTOCOL = PickleProtocol
```

secimi yeterli. 

Simdi anlamsal (semantic), islem sirasi gibi konulara gelelim. Hem
esle hem indirgeme icin tek bir sinif tanimlanmasi bizi aldatmasin,
esleyici metot ile indirgeyici metot arasinda aslinda sinif ic
degiskenlerini kullanarak iletisim olamiyor. Yani sinifimizdan bir
obje yaratilacak, o obje'nin esleme kismi bitince obje atilacak,
indirgeme icin baska bir (ayni siniftan) obje yaratilacak. "Peki o
zaman niye ayni sinif kullanilmis?" diye bir soru akla gelebilir,
yani, kod idaresi acisindan her seyin tek bir yerde olmasi daha temiz
tabii.

Ayrica indirgeme evresinde her ayri anahtar icin ayri bir objenin (ve
reducer metotunun) kullanildigini / cagrildigini kabul
edebilirsiniz. Yani obje eger "aklinda bir sey tutacaksa" bu
degerlerin baska anahtarlara ait olmamasi gerekir. Yuk dagitimi
baglaminda ayni obje belki birkac kez ayni anahtar icin kullanilir,
ama bunun nasil olacaginin garantisi yoktur, o sebeple tek anahtar tek
indirgeyici kabul etmek en iyisi. Zaten indirgeyici metotunun
parametre listesini dusunursek, def reducer(self, key, tokens) olarak
gosterilmis, yani tek bir anahtar gelmis ve onunla alakali degerler
listesi gelmis.

Bu arada mapper her satir icin bir kere cagrilir. Her anahtar icin o
anahtara ait reducer bir kere cagrilir.

Bu cagrilardaki mesela tokens tipine dikkat, bir iterator bu,
Hadoop'un mantigini biliyoruz, satir satir isleme uzerinde ve hafizada
cok az sey tutmak uzerine kurulu. Python'un iteratorleri de bu mantiga
cuk diye oturuyor, sadece iteratorun bize o anda gosterdigi seyle
ilgileniyoruz. Benzer bir sebeple Python icinden yield kullaniyoruz
[link], cunku kendimiz de basamagimiz icinde "bir sonraki basamagin
okuyabilecegi" bir iterator yaratiyoruz.

Bazi Cengel (hook) Noktalari ve Numaralar:

Bir makina / surecteki esleyicinin isi tamamen bitince, su metotu
cagir gibi bir tanim yapilabiliyor. Bunun icin map_final metotu tekrar
tanimlanir.

Bir diger numara: Metot steps ile kendi EI isimlerimiz tanimlayabilir, hatta EI basamaklarina ek basamaklar ekleyebiliriz. Isim degisikligi:

```
def steps(self):
    return [self.mr(mapper=self.xxx, reducer=self.yyy)]
```

Ek basamaklar

```
def steps(self):
    return [self.mr(mapper=self.mapper,reducer=self.reducer),
            self.mr(reducer=self.reduce_final)]
```

Bu durumda reducer'in urettigi anahtarlar bir sonraki reduce_final'a
yine anahtar bazinda gidecek. Ayni indirgeyici mantigi, sadece bir
tane daha. Biz bu asamayi bir "nihai toparlama" noktasi olarak
kullaniyoruz genelde, o yuzden cogunlukla tum reduce metotlarina ayni
anahtari urettiriyoruz, ve tum yollar tek bir reduce_final'e cikiyor.

Eger reduce tanimlamazsaniz Hadoop'taki IdentityReducer tanimlamak ile
ayni kapiya cikiyor, yani mapper'in ciktisi indirgenmeden direk sonuca
gidiyor.

Eger anahtar degeri icermeyen sadece degerleri bir cikti dosyasina
yazmak istiyorsak,

```
OUTPUT_PROTOCOL = RawValueProtocol
```

tanimlamak yeterli, bu durumda yield(None, my_list) gibi bir cagrinin
sadece my_list kismi yazilir (eger o tanimi yapmasaydik, None
ibaresini ciktida gorecektiniz). Biraz formatlama yaparsak, mesela
virgullerle ayrili bir cikti

```
yield (None,",".join(map(str,my_list)))
```

diyebiliriz. Bu durumda mesela tek makina ortaminda python myjob.py
girdi.csv > cikti.csv isletikten sonra virgulle ayrilmis anahtarsiz
guzel bir cikti buluruz.





