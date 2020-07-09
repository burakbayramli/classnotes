# Hadoop - Ilk Ornek (Python), Esleme-Indirgeme


Hadoop - Ilk Ornek (Python), Esleme-Indirgeme





Simdi bir onceki yazida Java ile yapilani Python ile yapalim. Once mapper.py kodunu /home/hduser altinda yaratalim,

#!/usr/bin/env pythonimport sys# input comes from STDIN (standard input)for line in sys.stdin:    # remove leading and trailing whitespace    line = line.strip()    # split the line into words    words = line.split()    # increase counters    for word in words:        # write the results to STDOUT (standard output);        # what we output here will be the input for the        # Reduce step, i.e. the input for reducer.py        #        # tab-delimited; the trivial word count is 1        print '%s\t%s' % (word, 1)

Sonra ayni dizinde reducer.py

#!/usr/bin/env pythonfrom operator import itemgetterimport syscurrent_word = Nonecurrent_count = 0word = None# input comes from STDINfor line in sys.stdin:    # remove leading and trailing whitespace    line = line.strip()    # parse the input we got from mapper.py    word, count = line.split('\t', 1)    # convert count (currently a string) to int    try:        count = int(count)    except ValueError:        # count was not a number, so silently        # ignore/discard this line        continue    # this IF-switch only works because Hadoop sorts map output    # by key (here: word) before it is passed to the reducer    if current_word == word:        current_count += count    else:        if current_word:            # write result to STDOUT            print '%s\t%s' % (current_word, current_count)        current_count = count        current_word = word# do not forget to output the last word if needed!if current_word == word:    print '%s\t%s' % (current_word, current_count)


Ise baslamadan once mapper.py isliyor mu hemen kontrol edelim,

echo "foo foo quux labs foo bar quux" | /home/hduser/mapper.py

Sonuc

foo    1foo    1quux    1labs    1foo    1bar    1quux    1

olmali. Indirgeyiciyi test edelim

echo "foo foo quux labs foo bar quux" | /home/hduser/mapper.py | /home/hduser/reducer.py

Sonuc 

foo    2quux    1labs    1foo    1bar    1quux    1

Eger bir onceki ornek yuzunden mevcut dizine yazilamayacagi gibi bir hata gelmesin istersek (ya da gelirse) bu dizini silebiliriz,

bin/hadoop dfs -rmr /user/hduser/gutenberg-output

Simdi Python surecleri baslatabiliriz, Python kodunun kullanilabilmesi icin Hadoop Streaming kullanacagiz, HS sayesinde isleyici sureclerin verilerini standart girdi (standard input, STDIN) uzerinde alirlar, ve ciktilari standart cikti (standard output, STDOUT) uzerine yazarlar. [HADOOP DIZIN]/contrib/streaming/hadoop-*streaming*.jar dosyasi bu isi yapmaktadir. Ayrica HS kullanilinca STDIN, STDOUT ile iletisim yapabilen (neredeyse her programlama dili) her turlu stil / dil isleyici yazmak icin kullanilabilir.

bin/hadoop jar contrib/streaming/hadoop-*streaming*.jar -mapper /home/hduser/mapper.py -reducer /home/hduser/reducer.py -input /user/hduser/gutenberg/* -output /user/hduser/gutenberg-output

Sonuclari daha once yaptigimiz gibi geri aliriz,

bin/hadoop dfs -getmerge /user/hduser/gutenberg-output /tmp/gutenberg-output

Artik ustteki dosya tum sonuclari iceren sonuc dosyasidir.

Daha detayli esle/indirge anlatimi icin Istatistik notlarimiza bakilabilir.

Mrjob icin suraya bakiniz.

Kaynaklar

http://developer.yahoo.com/hadoop/tutorial/module4.html

http://www.michael-noll.com/tutorials/running-hadoop-on-ubuntu-linux-single-node-cluster/

---

[1] Kelime sayma probleminde anahtarlara ayirma islemi basit gibi duruyor, "kelimenin kendisi anahtar zaten" vs. Fakat bu evrede bazi veri temizleme islemleri olabilir, ki bu islemler kendi ic mantiklarini tasiyor olabilirler; ya da anlamsal (semantic) farkli islemler devreye sokabiliriz. Kelimelerde Ankara, ankara, annkara kelimelerinin ayni anahtara esleneceginin kararini bu asamada veriyor olabilirdiik mesela. Belki elimizde yazma hatalarini algilayabilen bir algoritmamiz vardir, onu bu asamada kullanmaya karar verdik vs. Geometrik transformasyonlardan tutun, cografi, kelimesel her turlu eslestirme bu safhada kullanilabilir.





