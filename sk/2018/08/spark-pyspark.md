# Spark, PySpark

Hadoop gibi yüksek ölçekte ve pür düz dosya bazlı veri işleyebilen bir
mimari - Spark. Python dilinde PySpark ile kullanılabilir, en basit
kurulum için pip install pyspark çağrısı yeterli. Bu çağrı arka plan
kodlarının hepsini indirip kuruyor.

Spark Hadoop aksine map / reduce mimarisi kullanmaz, daha çetrefil bir
yönlü çevrimsiz çiziti (directed acyclic graph -DAG-) baz alan bir
dağıtım mimarisi kullanır. Bu açıdan TensorFlow ile daha yakından
bağlantıları var.

Bu yazıda çok makinalı kurulumdan bahsetmeyeceğiz, kodlama odaklı
olacağız fakat Spark ölçekleme işini gözden uzakta kendiliğinden
hallettiği için altta anlatılacak programlama derslerinin hepsi çok
makinalı ortamlar için geçerli.

Peki ama çok makinalı paralelizasyon yapmayacaksak, niye Spark
kullanalım? En basit haliyle bile Spark en azından makinanızın tüm
çekirdeklerini kullanabilecek yapıda hazırdır. Yani diyelim ki 100
GB'lik bir dosyamız ve 30 çekirdekli makinamiz var. Spark ile tüm 30
çekirdeği paralel şekilde kullanabilecek bir analiz yapabiliriz. Diğer
yandan pür Python ile bu paralelizasyon yapmak hala kolay değil.

Spark ile (tek makina ortamında bile) tek büyük dosyayı paralel, ya da
bir sürü büyük dosyayı paralel şekilde işleyebiliriz.

Spark Hadoop dosya sistemi HDFS ile konusabilir.

Spark'ın önemli bir ilerlemesi paketten çıktığı hali ile SQL'vari bir
dille veriye erişimi mümkün kılması. Metin dosyası üzerinde SQL! Ne
güzel. SQL tabii ki arka planda DAG üzerinden parçalanıp pek çok
makina üzerinde işleyebilir hale gelir.

Bu yazıda referans verilen Hadoop yazısında ile işlediğimiz patent
verisini baz alacağız (bağlantı en altta). Bakalım Spark ile kodlama
ne kadar daha rahat. İlk önce hazırlama kodlarını çağıralım, dosya
isimlerini verelim,

```
from pyspark import SparkContext

from pyspark.sql import SparkSession, SQLContext

f1 = 'apat63_99.txt.gz'
f2 = 'cite75_99.txt.gz'
f3 = 'citesmall.txt'

sc = SparkContext()
sql = SQLContext(sc)
```

Şimdi Spark işlemleri yapmaya hazırız. Görüldüğü gibi gzip dosyaları
verdik (dosyalar Hadoop yazısının altındaki bağlantılardan
indirilebilir), bu dosyalar içinde metin dosyaları var (gunzip ile
kontrol edebiliriz), Spark metin dosyalarını direk gz içinden
okuyabiliyor. Bu bir rahatlık.

Dikkat: her türlü sıkıştırılmış format okunmayabilir, mesela tar cvzf
ile oluşturulan tar.gz işlemiyor, işlermiş gibi gözüküyor fakat
verinin bazı kısımları bozuk çıkacaktır).

Şimdi dosyayı okuyalım,

```
df = sql.read.format("csv").\
         option("header","true").\
         option("delimiter", ",").\
         option("inferSchema","true").\
         load(f1)

df.printSchema()

root
 |-- PATENT: integer (nullable = true)
 |-- GYEAR: integer (nullable = true)
 |-- GDATE: integer (nullable = true)
 |-- APPYEAR: integer (nullable = true)
 |-- COUNTRY: string (nullable = true)
 |-- POSTATE: string (nullable = true)
 |-- ASSIGNEE: integer (nullable = true)
 |-- ASSCODE: integer (nullable = true)
 |-- CLAIMS: integer (nullable = true)
 |-- NCLASS: integer (nullable = true)
 |-- CAT: integer (nullable = true)
 |-- SUBCAT: integer (nullable = true)
 |-- CMADE: integer (nullable = true)
 |-- CRECEIVE: integer (nullable = true)
 |-- RATIOCIT: double (nullable = true)
 |-- GENERAL: double (nullable = true)
 |-- ORIGINAL: double (nullable = true)
 |-- FWDAPLAG: double (nullable = true)
 |-- BCKGTLAG: double (nullable = true)
 |-- SELFCTUB: double (nullable = true)
 |-- SELFCTLB: double (nullable = true)
 |-- SECDUPBD: double (nullable = true)
 |-- SECDLWBD: double (nullable = true)
```

sonucu gelecek. Gayet rahat okundu, noktalarla ayrılmış bölümler
farklı kolon değerleri kolon kabul edildi, şu anda elimizde bir
DataFrame objesi var.

```
type(df)

pyspark.sql.dataframe.DataFrame
```

Bu dosya her patent için tarifsel, meta bilgi içeriyor. Patent ne
zaman alınmış, hangi ülkeden alınmış vs. Eğer ülke bazlı bir sayım
yapmak istersek,

```
df.groupBy('COUNTRY').count().show()

+-------+-----+
|COUNTRY|count|
+-------+-----+
|     DZ|   10|
|     MM|    4|
|     LT|    7|
|     CI|    4|
|     TC|    2|
|     FI| 6984|
|     AZ|    3|
|     UA|   77|
|     RO|  361|
|     ZM|   11|
|     NL|26687|
|     BS|  164|
|     PL|  686|
|     AM|    2|
|     MX| 1832|
|     PF|    3|
|     GL|    1|
|     EE|    4|
|     VG|    8|
|     SM|    2|
+-------+-----+
only showing top 20 rows
```

Sonuçlar üst 20 satırı olacak şekilde gösterildi. Bu arada Spark
tembel (lazy) işlemleri her kademesine dahil etmiş, yani bir işlemi
mecbur olmadıkça katiyen yapmıyor. Mesela üstteki çağrıda show
öncesinde olan

```
res = df.groupBy('COUNTRY').count()
```

komutunu işletsek, say (count) emri vermiş olsak bile o işlem
hesaplanmiyor. Ne zamanki gösterme emri geldi, show ile, Spark o zaman
işlem yapıyor.

Sonuçlara dönersek, veriye göre MX (Meksika herhalde) ülkesi 1832 patent almış.

Sıralama yaparak göstersek,

```
df.groupBy('COUNTRY').count().sort('count',ascending=False).show()

+-------+-------+
|COUNTRY|  count|
+-------+-------+
|     US|1784989|
|     JP| 421441|
|     DE| 221095|
|     GB|  98012|
|     FR|  85398|
|     CA|  53872|
|     CH|  43313|
|     IT|  32433|
|     SE|  28286|
|     NL|  26687|
|     TW|  19979|
|     KR|  14855|
|     AU|  11386|
|     BE|  10972|
|     AT|  10260|
|     IL|   7378|
|     SU|   6992|
|     FI|   6984|
|     DK|   6479|
|     ES|   3601|
+-------+-------+
only showing top 20 rows
```

Sayarak pek çok farklı sonuca varmak mümkün. Mesela her patent kaç
tane referans almış? Bunun için ikinci dosyayı yükleyelim, bu dosyada
her patentin referans ettiği kayıtlı (her referans bir satır)

```
df = sql.read.format("csv").\
         option("header","true").\
         option("delimiter", ",").\
         option("inferSchema","true").\
         load(f2)

df.printSchema()

root
 |-- CITING: integer (nullable = true)
 |-- CITED: integer (nullable = true)

CITING referans eden kimlik (id) CITED referans edilen.
```

5 tane ilk satırı ekrana basalım,

```
df.take(5)

[Row(CITING=3858241, CITED=956203),
 Row(CITING=3858241, CITED=1324234),
 Row(CITING=3858241, CITED=3398406),
 Row(CITING=3858241, CITED=3557384),
 Row(CITING=3858241, CITED=3634889)]
```

 patent kaç tane referans almış hesabı için

```
df.groupBy('CITED').count().show()

+-------+-----+
|  CITED|count|
+-------+-----+
|3060453|    3|
|3390168|    6|
|3626542|   18|
|3611507|    5|
|3000113|    4|
|3273281|    1|
|3550403|    2|
| 228233|    1|
|3827399|    1|
|2622583|    1|
|3381752|   10|
|1035529|    1|
|2664273|    6|
|3289868|   16|
|3400790|    7|
|3618714|    8|
|3297130|    7|
|1715276|    4|
|  50223|    1|
| 685851|    1|
+-------+-----+

only showing top 20 rows
```

Bir sürü sayı var, ama sonuçlar patent bazlı, bunu daha özetli olarak
göremez miyiz? Sayımların sayımı belki yardımcı olur, yani bir
histogram. Belli kutucuklara göre o kutucuların temsil ettiği sayı
penceresine kaç tane referans sayısı düşüyor, bunu frekans olarak
gösterebiliriz.

```
df.groupBy('CITED').count().rdd.values().histogram([0,1,2,3,4,5,10,200])

([0, 1, 2, 3, 4, 5, 10, 200],
 [0, 921127, 552246, 380319, 278438, 686185, 440581])
```

Bu sonuç aslında liste içinde liste, üst seviyede iki liste var,
1. liste aralık sınırlarını tanımlayan liste, onu biz tanımlamıştık
zaten, 0,1,2 diye gidiyor, yani 0 ile 1 arası değerler ilk kutucuğa, 1
ile 2 arası değerler ikinci kutucuğa, vs. Böyle devam ediyor. Üst
seviyede 2. listede bu kutucukların içine kaç tane bulgu düştüğünün
hesabı tutuluyor, yani frekans değerleri. 1 ile 2 arası 921,000 kusur
referans var, 2 referans daha az, 3 daha da böyle gidiyor. Not: sıfır
referans gösterilmemiş çünkü count hesabı doğası itibariyle olan bir
sayımı gösteriyor, yani sıfır tane olan sayım hiç yok.

Devam edelim, Şimdi Hadoop yazısının referans aldığı kitaptaki
"tersine çevirme" işlemini yapalım. Yani eğer id 1,2,3 patentleri id
5'ike referans vermişse, tersine çevirince 5 için 1,2,3 listesini
görmemiz lazım. Bu işlem için Hadoop + Java ile bir sürü çetrefil
işlemler görülüyor. Fakat bu işlem de bir tür Spark SQL'i ile
halledilebilir. Referans verilen kimlik üzerinde groupBy yaparız, ve
CITED gruplaması üzerinden CITING listesi elde ederiz, bu listeyi
tabii ki özetlemek lazım, çünkü groupBy sonuçları muhakkak bir şekilde
özetlenmelidir, ya sayılarak, ya ortalaması alınarak (bu örnekte
ortalama anlamsız tabii), gibi. Bu özetleme direk string yanyana
koyulacak şekilde birleştirme ile olabilir, önce collect_list sonra
concat_ws ile bir ayraç belirtilerek birleştirme yapılır.

Daha ufak bir veride görelim,

```
"CITING","CITED"
1,2
2,3
3,4
4,5
6,7
7,2
8,3
9,5
10,5
11,5

df = sql.read.format("csv").\
         option("header","true").\
         option("delimiter", ",").\
         option("inferSchema","true").\
         load(f3)

from pyspark.sql.functions import concat_ws, collect_list

df.groupBy('CITED').agg(concat_ws(":", collect_list(df.CITING))).collect()

[Row(CITED=3, concat_ws(:, collect_list(CITING))=u'2:8'),
 Row(CITED=5, concat_ws(:, collect_list(CITING))=u'4:9:10:11'),
 Row(CITED=4, concat_ws(:, collect_list(CITING))=u'3'),
 Row(CITED=7, concat_ws(:, collect_list(CITING))=u'6'),
 Row(CITED=2, concat_ws(:, collect_list(CITING))=u'1:7')]
```

sonuç geldi. Sonuç doğru.

Peki bu sonuçları bir dosyaya yazmak istesek,

```
tmp = df.groupBy('CITED').agg(concat_ws(":", collect_list(df.CITING) ).alias("REFBACK")  )

tmp.coalesce(1).write.csv("/tmp/out")
```

işletiriz. Şimdi /tmp/out altına bakarsak, orada bir part-..csv
dosyası göreceğiz. Bu dosya içinde istenen sonuç var.

Bu arada o tek dosyayı elde etmek için coalesce(1) kullandık,
kullanmasaydık /tmp/out altında bir sürü part- dosyaları olacaktı,
bunlar sonucun parçaları olacaktı bir bakıma. Aslında çok makinalı
ortamda elde edilen (ve yeterli olan) genelde budur, coalesce(1)
çağrısı sonuçları tek bir kontrolör makinaya yönlendirecekti. Genelde
sonuçların ayrı makinalarda parçalar olarak kalması yeterlidir, bunlar
sonra başka şekillerde birleştirilebilir.

PySpark sadece CSV ile sınırlı değil, çok rahat şekilde json dosyaları
da okuyup yazabilir.

RDD

Biraz önce gördüğümüz yapısı belli (structured) veri idi. Kolon
isimleri belli, virgülle ayrılmış veri var, bunlar DataFrame ile
işlenebilir ve üzerlerinde Spark SQL işletilebilir. Fakat Spark yapısı
belli olmayan veri ile de çalışabilir (hatta ilk çıkış noktası
burası), RDD veri yapısı burada devreye giriyor. RDD paralelize
edilebilen bir birim. Biraz önceki patent dosyasını

```
rdd = sc.textFile("apat63_99.txt.gz", 4)
```

ile okuyabilirdik. İkinci parametre, 4, kaç parça yaratılması istendiğini kontrol ediyor. Eğer verilmezse her parçada 128 MB olacak şekilde Spark bölümü yapıyor.

Şimdi sayalım,

```
rdd.count()

2923923

row = rdd.take(3)
```

dersek

```
[u'"PATENT","GYEAR","GDATE","APPYEAR","COUNTRY","POSTATE","ASSIGNEE","ASSCODE","CLAIMS","NCLASS","CAT","SUBCAT","CMADE","CRECEIVE","RATIOCIT","GENERAL","ORIGINAL","FWDAPLAG","BCKGTLAG","SELFCTUB","SELFCTLB","SECDUPBD","SECDLWBD"',
 u'3070801,1963,1096,,"BE","",,1,,269,6,69,,1,,0,,,,,,,',
 u'3070802,1963,1096,,"US","TX",,1,,2,6,63,,0,,,,,,,,,']
```

Gördüğümüz gibi bir liste sadece, içine ne olduğu RDD bağlamında
Spark'in umrunda değil.

```
row[2]

u'3070802,1963,1096,,"US","TX",,1,,2,6,63,,0,,,,,,,,,'

row[2][35]

u'6'
```

RDD üzerinde Python işlemleri yaptırabiliriz,

```
def f(x): return x[35]

rdd.map(f).take(5)

[u'"', u'6', u'6', u'6', u'6']
```

RDD içinde envai türden veri olabilir, mesela bazı şirketlerde ara
veri ürünü olarak Python pickle dosyaları bile part- dosyalarında
tutulabiliyor, sonra textFile yerine pickleFile ile bunlar okunur,
üzerlerinde ek işlemler yapılır vs.

SQL Sorgulari, Birleştirmek (Join)

Daha çetrefil bir örnek görelim; Amerika'dan alınmış kaç tane patent
İngiltere'den alınmış patenti referans ediyor?

Bunun için üçlü birleştirme yapmamız lazım. Önce patent tanımından
başlayacağız, onun PATENT kimliğini referans tablosunun CITING
kolonuna eşleyeceğiz, oradan CITED kolonunu tekrar geriye patent tanım
tablosuna bağlanacağız. Bu birleştirme üzerinde ilk tanımdaki ülke US
ikinci tanımda GB kodu olma şartını arayacağız.

```
descr = sql.read.format("csv").\
                 option("header","true").\
                 option("delimiter", ",").\
                 option("inferSchema","true").\
                 load(f1)

cite = sql.read.format("csv").\
                option("header","true").\
   option("delimiter", ",").\
     option("inferSchema","true").\
     load(f2)

descr.createOrReplaceTempView("descr")
cite.createOrReplaceTempView("cite")

sqlc = "select t1.PATENT from descr t1 " + \
       "join cite t2 on t1.PATENT = t2.CITING  " + \
       "join descr t3 on t2.CITED = t3.PATENT " + \
       "where t1.COUNTRY= 'US' and t3.COUNTRY ='GB'"

sql.sql(sqlc).count()

253018
```

Tüm referansların sayısı kaç


```
cite.count()

16522438
```


Düz veri dosyaları üzerinde SQL işletebilmek çok güzel bir şey.

Yapay Öğrenme

Spark'ın yapay öğrenme (machine learning) için ek bir kütüphanesi var,
MLLib. Bu kütüphane içinde kümeleme, lojistik regresyon gibi YO'da
kullanılan pek çok algoritmanın yüksek ölçekte çalışan versiyonları
mevcuttur.

Hafıza Arttırmak

Bazen Spark yeterince hafıza olmadığından yakınabilir (başıma geldi,
60 GB hafıza ile hata mesajı geldi),  arttırmak için

```
conf = SparkConf().setAppName("App")
conf = (conf.setMaster('local[*]')
        .set('spark.executor.memory', '180G')
        .set('spark.driver.memory', '180G')
        .set('spark.driver.maxResultSize', '180G'))

sc = SparkContext(conf=conf)
```

Ölçüler biraz abartılı olabilir, önemli değil, hafıza maksimuma
gelinceye kadar işlesin, test ortamında farketmez.

Aynı seçenekler [SPARK DİZİNİ]/conf/spark-defaults.conf dizininde de ayarlanabilir.

Kelime Saymak (Wordcount)

Hadoop'un demirbaş örneği kelime saymayı yapmazsak olmaz, bu örnek
yüksek ölçekli veri işlemenin "hello world" örneği haline geldi.

```
f = '/home/burak/Documents/Dropbox/Public/data/nietzsche.txt'

text_file = sc.textFile(f)

counts = text_file.flatMap(lambda line: line.split(" ")) \
             .map(lambda word: (word, 1)) \
             .reduceByKey(lambda a, b: a + b)

counts.saveAsTextFile("/tmp/wordcount")
```

Sayım sonuçları /tmp/wordcount dizininde olacak.

Bağlantılar ve veri altta

http://sayilarvekuramlar.blogspot.com/2014/09/esle-indirge-mimarisi-mapreduce-mr.html

https://dzone.com/articles/apache-spark-introduction-and-its-comparison-to-ma

https://s3.amazonaws.com/text-datasets/nietzsche.txt





