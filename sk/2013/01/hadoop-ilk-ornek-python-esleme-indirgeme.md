# Hadoop - Ilk Ornek (Python), Esleme-Indirgeme

Şimdi bir önceki yazıda Java ile yapılanı Python ile yapalım. Önce
mapper.py kodunu `/home/hduser` altında yaratalım,

```python
#!/usr/bin/env python
import sys
# input comes from STDIN (standard input)
for line in sys.stdin:
    # remove leading and trailing whitespace
    line = line.strip()
    # split the line into words
    words = line.split()
    # increase counters
    for word in words:
        # write the results to STDOUT (standard output);
        # what we output here will be the input for the
        # Reduce step, i.e. the input for reducer.py
        #
        # tab-delimited; the trivial word count is 1
        print '%s\t%s' % (word, 1)
```

Sonra aynı dizinde reducer.py

```python
#!/usr/bin/env python
from operator import itemgetter
import syscurrent_word = None
current_count = 0word = None
# input comes from STDINfor line in sys.stdin:
    # remove leading and trailing whitespace
    line = line.strip()
    # parse the input we got from mapper.py
    word, count = line.split('\t', 1)
    # convert count (currently a string) to int
    try:
        count = int(count)
    except ValueError:
        # count was not a number, so silently
        # ignore/discard this line
        continue
    # this IF-switch only works because Hadoop sorts map output
    # by key (here: word) before it is passed to the reducer
    if current_word == word:
        current_count += count
    else:
        if current_word:
            # write result to STDOUT
            print '%s\t%s' % (current_word, current_count)
        current_count = count
        current_word = word# do not forget to output the last word if needed!
if current_word == word:
    print '%s\t%s' % (current_word, current_count)
```

İşe başlamadan önce `mapper.py` işliyor mu hemen kontrol edelim,

```
echo "foo foo quux labs foo bar quux" | /home/hduser/mapper.py
```

Sonuc

```
foo
    1foo
    1quux
    1labs
    1foo
    1bar
    1quux
    1
```

olmalı. İndirgeyiciyi test edelim

```
echo "foo foo quux labs foo bar quux" | /home/hduser/mapper.py | /home/hduser/reducer.py
```

Sonuc 

```
foo
  
 2quux
  
 
1
labs
  
 1foo  
 1bar
  
 1quux
  
 1
```

Eğer bir önceki örnek yüzünden mevcut dizine yazılamayacağı gibi bir
hata gelmesin istersek (ya da gelirse) bu dizini silebiliriz,

```
bin/hadoop dfs -rmr /user/hduser/gutenberg-output
```

Şimdi Python süreçleri başlatabiliriz, Python kodunun kullanılabilmesi
için Hadoop Streaming kullanacağız, HS sayesinde işleyici süreçlerin
verilerini standart girdi (standard ınput, STDİN) üzerinde alırlar, ve
çıktıları standart çıktı (standard output, STDOUT) üzerine
yazarlar. `[HADOOP DIZIN]/contrib/streaming/hadoop-*streaming*.jar`
dosyası bu işi yapmaktadır. Ayrıca HS kullanılınca STDIN, STDOUT ile
iletişim yapabilen (neredeyse her programlama dili) her türlü stil /
dil işleyici yazmak için kullanılabilir.

```
bin/hadoop jar contrib/streaming/hadoop-*streaming*.jar -mapper /home/hduser/mapper.py -reducer /home/hduser/reducer.py -input /user/hduser/gutenberg/* -output /user/hduser/gutenberg-output
```

Sonuçları daha önce yaptığımız gibi geri alırız,

```
bin/hadoop dfs -getmerge /user/hduser/gutenberg-output /tmp/gutenberg-output
```

Artık üstteki dosya tüm sonuçları içeren sonuç dosyasıdır.

Daha detaylı eşle/indirge anlatımı için İstatistik notlarımıza bakılabilir.

Not: Kelime sayma probleminde anahtarlara ayırma işlemi basit gibi
duruyor, "kelimenin kendisi anahtar zaten" vs. Fakat bu evrede bazı
veri temizleme işlemleri olabilir, ki bu işlemler kendi iç
mantıklarını taşıyor olabilirler; ya da anlamsal (semantic) farklı
işlemler devreye sokabiliriz. Kelimelerde Ankara, ankara, annkara
kelimelerinin aynı anahtara eşleneceğinin kararını bu aşamada veriyor
olabilirdiik mesela. Belki elimizde yazma hatalarını algılayabilen bir
algoritmamiz vardır, onu bu aşamada kullanmaya karar verdik
vs. Geometrik transformasyonlardan tutun, coğrafi, kelimesel her türlü
eşleştirme bu safhada kullanılabilir.

Kaynaklar

[1] http://developer.yahoo.com/hadoop/tutorial/module4.html

[2] http://www.michael-noll.com/tutorials/running-hadoop-on-ubuntu-linux-single-node-cluster/
