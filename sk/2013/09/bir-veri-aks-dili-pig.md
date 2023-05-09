# Bir Veri Akış Dili: Pig

Hadoop icin yazilan bazi veri analizleri oldukca cetrefil hale
gelebilir. Ilginctir ki SQL bazli veri tabanlari ortaminda tablolar,
dosyalar ile ugrasirken, Buyuk Veri / Hadoop devreye girince duz
dosyalar (flat file) ortamina geri donduk. Aslinda bu bir bakima iyi
oldu, daha daginik, yapisiz (unstructured) veriler ile ugrasiyoruz, ve
onlari sonradan analiz etmek icin daha yaratici cozumler bulmamiz
gerekiyor. Verinin uzerinde onceden bir sablon koymuyoruz, analiz
gerektiginde veriyi o ihtiyaca yonelik olarak okuyoruz. Veri parcali
parcali, bir takim duz dosyalar, bir tar.gz dosyasi icindeki birkac,
yuzlerce, binlerce bir halde karsimiza cikabiliyor.

Daha once MRJob'tan bahsettik. Bu ortamda bir islemden (job) digerine
veri aktarimi kolaydir, her basamakta direk Python icinde kalarak kod
yazabilirsiniz, map() icinden yayinladiginiz anahtar degerler mesela,
sonraki reduce() basamagina direk Python tipleri olarak aktarilir.

Fakat MRJob'in hala bazi eksikleri var, tum veri akisinizi ayarlamak
icin bir arac degil. Diyelim ki elinizde girdi olarak iki dosya var,
bu dosyalari birlestireceksiniz (join) sonra sonuc olarak iki dosya
ureteceksiniz, bunlarin biri, bir sonraki basamak icin referans
olacak, vs. Bu tur isleri idare etmek icin bir veri analiz diline
ihtiyac var.

En son surumu Pig 0.12 bu ihtiyaclara cevap veriyor, ayrica, bizim
icin en onemli ozelligi Python icin Jython uzerinden degil, artik
direk baglanti kurabiliyor olmasi, yani numpy, scipy gibi
kutuphaneleri Pig uzerinden kullanmak mumkun olacak. Bu surum
hakikaten yeni, duyurusu surada.

Konusma

Apache Pig

Pig musteri tarafinda (client side) isleyen bir programdir. Yani bir
Pig script yazdigimizda bu script'i tum Hadoop makinalarina ayri ayri
gondermeye gerek yoktur mesela. Pig script'iniz Hadoop kumenize
baglanir, ve o kumeye, veri akis kodunuza gore arka planda bazi
esle/indirge islemleri islettirir. Pig'i bir nevi derleyici gibi
gorebiliriz, Pig kodu yazariz, "derleyince" arka planda EI islemleri
uretilir (ve kumeye gonderilir). Demek ki tek bir makinada Pig
kurulmus olmasi yeterlidir.

Once baglantidan Pig 0.12 indirin, unzip yapin. Kurulusu hduser altina
yapsaniz iyi olur. Sonra su - hduser ile hduser olun ve .bashrc icine
PATH sonuna [PIG DIZINI]/bin ekleyin

Kullanmak icin soyle bir veriyi

```
1950    0    11950    22    11950    -11    11949    111    11949    78    1
```

`/tmp/sample.txt` icine yazin. Su kodu /tmp/sample.pig icine yazin

```
records = LOAD '/tmp/sample.txt' AS (year:chararray, temperature:int, quality:int);filtered_records = FILTER records BY temperature != 9999 AND (quality == 0 OR quality == 1 OR quality == 4 OR quality == 5 OR quality == 9);grouped_records = GROUP filtered_records BY year;max_temp = FOREACH grouped_records GENERATE group, MAX(filtered_records.temperature);DUMP max_temp;
```

Simdi hduser olun, ve komut satirindan pig -x local /tmp/sample.pig isletin. 

```
(1949,111)
(1950,22)
```

gibi bir sonuc ekrana basilmali. Ilk Pig script'imiz boyle. 

Bir birlestirme yapalim, /tmp/A icine

```
Joe    2Hank    4Ali    0Eve    3Hank    2
```

/tmp/B icine

```
2    Tie4    Coat3    Hat1    Scarf
```

Simdi /tmp/join.pig icine

```
A = LOAD '/tmp/A';B = LOAD '/tmp/B';C = JOIN A BY $0, B BY $1;DUMP C;
```

Isletelim

```
pig -x local /tmp/join.pig
```

Sonuc

```
(2,Tie,Joe,2)(2,Tie,Hank,2)(3,Hat,Eve,3)(4,Coat,Hank,4)
```

Kurulus Notlari

Kurulus acisindan en rahati Pig'i hduser altinda kurmak, sonra diger
gunluk kullanilan kullanici icindeyken ssh hduser@localhost araciligi
ile pig cagrisini hduser'a yaptirmak. Bu sekilde local durumunda
ciktilar /home/hduser altina gidecektir. Bu ssh kullanimina bir ornek
soyle

```
$ ssh hduser@localhost "\export JAVA_HOME=/usr/lib/jvm/java-6-openjdk-amd64; \/home/hduser/pig-0.12.0/bin/pig -x local \/home/burak/dizin/filanca.pig \
```

Tabii ustte pig script'in oldugu dizinin hduser tarafindan
erisilebiliyor olmasi lazim, bunu chmod +x ile script'in icinde oldugu
dizinleri disariya acarak basarabilirsiniz.

Dizinler

Pig ile store ... into 'filanca' komutlari into'dan sonra gelen isim
altinda bir dizin yaratir, ve sonuclar tanidik Hadoop formatinda
part-r-.. gibi dosyalardir. Fakat ayni script tekrar isletilirse bu
dizinin ustune yazilmaya ugrasilir ve Pig hata verir. Dizinin uzerine
yazmak problem degilse, script basinda

```
fs -rmr filanca 
```

ile bu dizin silinebilir, sonra script'in geri kalani o dizini yeniden
yaratacaktir.

Bu yaziya ekler olacak.






