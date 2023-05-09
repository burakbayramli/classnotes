# MrJob

Python bazlı eşle/indirge süreçlerini hiç Hadoop komut satırıyla ve
Streaming ile uğraşmadan işletmenin bir diğer yolu mrjob (Amerikalılar
bunu "mister job" diye okuyor). Kurmak için

```
sudo pip install mrjob
```

`/home/hduser` altında `.mrjob.conf` adında bir dosya yaratın ve içine sunu yazın

```
runners:  hadoop:
    hadoop_home: [HADOOP DIZINI] 
```

Mrjob ile eşleme / indirgeme komutları ayrı script olarak değil, tek
bir class'ta ayrı metotlar olarak verilir. Mesela kelime sayma

```
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

Test amaçlı tek başına işletmek için (Hadoop olmadan)

```
python words.py words.csv
```

Hadoop ile (once dosyayi kopyalayalim)

```
hadoop dfs -copyFromLocal words.csv  /user/
```

```
python /home/hduser/words.py hdfs:///user/words.csv -r hadoop --step-num=1
```

Üstteki komutun işlemesi için hdüşer olarak login etmeniz
gerekir. Eğer başka bir kullanıcıdan işletmek isterseniz,

```
ssh localhost -l hduser [ustteki ayni komut]
```

işleyecektir. Kaç tane eşle / indirge'nin paralel işleyeceğini
tanımlayabiliriz, bunun için (mesela 2)

```
... --jobconf mapred.map.tasks =2 --jobconf mapred.reduce.tasks=2
```

kullanilir. 

Nasıl İşliyor?

Peki motor kapağının altında neler oluyor? Mrjob, Hadoop Streaming'i
sarmalayarak bize daha rahat kullanımı olan bir arayüz
sunar. Streaming, bilindiği gibi HDFS'teki bir dosyayı okuyup
script'lerimize satır satır geçmekle sorumludur. Mrjob ise bunun
üzerine bir katman daha ekler, ve mesela dosyamızda tab, boşluk
(space) iyi bilinen ayraçlar kullanılıyorsa daha baştan anahtar ve
değeri ayırıp map çağrısına bunları tek obje, obje listeleri olarak
geçebilir.

Dahası da var: map içinden üretilen anahtar ve değer ikilileri
çetrefil tipler olabilir (vektör, sözlük gibi) ve bu çıktılar
Python'un yerli (natıve) tipleri olarak indirgeyiciye
gönderilebilirler. Direk Hadoop'ta bu iş oldukca zor olurdu. Bu
iletişim için

```
from mrjob.protocol import PickleProtocol
..
class BizimMR(MRJob):
    INTERNAL_PROTOCOL = PickleProtocol
```

seçimi yeterli. 

Şimdi anlamsal (semantic), işlem sırası gibi konulara gelelim. Hem
eşle hem indirgeme için tek bir sınıf tanımlanması bizi aldatmasın,
eşleyici metot ile indirgeyici metot arasında aslında sınıf iç
değişkenlerini kullanarak iletişim olamıyor. Yani sınıfımızdan bir
obje yaratılacak, o obje'nin eşleme kısmi bitince obje atılacak,
indirgeme için başka bir (aynı sınıftan) obje yaratılacak. "Peki o
zaman niye aynı sınıf kullanılmış?" diye bir soru akla gelebilir,
yani, kod idaresi açısından her şeyin tek bir yerde olması daha temiz
tabii.

Ayrıca indirgeme evresinde her ayrı anahtar için ayrı bir objenin (ve
reducer metotunun) kullanıldığını / çağrıldığını kabul
edebilirsiniz. Yani obje eğer "aklında bir şey tutacaksa" bu
değerlerin başka anahtarlara ait olmaması gerekir. Yük dağıtımı
bağlamında aynı obje belki birkaç kez aynı anahtar için kullanılır,
ama bunun nasıl olacağının garantisi yoktur, o sebeple tek anahtar tek
indirgeyici kabul etmek en iyisi. Zaten indirgeyici metotunun
parametre listesini düşünürsek, def reducer(self, key, tokens) olarak
gösterilmiş, yani tek bir anahtar gelmiş ve onunla alakalı değerler
listesi gelmiş.

Bu arada mapper her satır için bir kere çağrılır. Her anahtar için o
anahtara ait reducer bir kere çağrılır.

Bu çağrılardaki mesela tokens tıpine dikkat, bir iteratör bu,
Hadoop'un mantığını biliyoruz, satır satır işleme üzerinde ve hafızada
çok az şey tutmak üzerine kurulu. Python'un iteratörleri de bu mantığa
cuk diye oturuyor, sadece iteratörün bize o anda gösterdiği şeyle
ilgileniyoruz. Benzer bir sebeple Python içinden yield kullanıyoruz
[link], çünkü kendimiz de basamağımız içinde "bir sonraki basamağın
okuyabileceği" bir iteratör yaratıyoruz.

Bazı Çengel (hook) Noktaları ve Numaralar:

Bir makina / süreçteki eşleyicinin işi tamamen bitince, şu metotu
çağır gibi bir tanım yapılabiliyor. Bunun için map_final metotu tekrar
tanımlanır.

Bir diğer numara: Metot steps ile kendi Eİ isimlerimiz tanımlayabilir,
hatta Eİ basamaklarına ek basamaklar ekleyebiliriz. İsim değişikliği:

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

Bu durumda reducer'in ürettiği anahtarlar bir sonraki reduce_final'a
yine anahtar bazında gidecek. Aynı indirgeyici mantığı, sadece bir
tane daha. Biz bu aşamayı bir "nihai toparlama" noktası olarak
kullanıyoruz genelde, o yüzden çoğunlukla tüm reduce metotlarına aynı
anahtarı ürettiriyoruz, ve tüm yollar tek bir reduce_final'e çıkıyor.

Eğer reduce tanımlamazsanız Hadoop'taki İdentityReducer tanımlamak ile
aynı kapıya çıkıyor, yani mapper'ın çıktısı indirgenmeden direk sonuca
gidiyor.

Eğer anahtar değeri içermeyen sadece değerleri bir çıktı dosyasına
yazmak istiyorsak,

```
OUTPUT_PROTOCOL = RawValueProtocol
```

tanımlamak yeterli, bu durumda yield(Nöne, my_list) gibi bir çağrının
sadece my_list kısmı yazılır (eğer o tanımı yapmasaydık, Nöne
ibaresini çıktıda görecektiniz). Biraz formatlama yaparsak, mesela
virgüllerle ayrılı bir çıktı

```
yield (None,",".join(map(str,my_list)))
```

diyebiliriz. Bu durumda mesela tek makina ortamında `python myjob.py
girdi.csv > cikti.csv` işletikten sonra virgülle ayrılmış anahtarsız
güzel bir çıktı buluruz.



