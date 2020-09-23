# Hadoop - Ilk Ornek (Java)


Hadoop - Ilk Ornek (Java) 





Oncelikle tum Hadoop yazilarinin kullandigi ornek kelime sayma ornegine bakalim. Suradaki adreslerden 3 tane veri dosyasini indirin, ve diyelim ki /tmp/gutenberg altina koyun

http://www.gutenberg.org/etext/20417

http://www.gutenberg.org/etext/5000

http://www.gutenberg.org/etext/4300

Simdi bu dosyalari HDFS uzerine kopyalacagiz. Dikkat, HDFS ozel bir dosya sistemi [3], bizim dosya sistemimiz "uzerinde" yasiyor ama ic yapisi tamamen baska bir alem. O "aleme" erismek icin normal Unix cp, mv komutlarini degil, Hadoop'un kendi verdigi komutlari kullaniyoruz. Yani o dosya sistemiyle konusurken bir tercumana ihtiyacimiz var. Not: Alttaki tum komutlari hduser ve [HADOOP_DIZINI] icinde islettigimizi varsayiyoruz [1],

bin/hadoop dfs -copyFromLocal /tmp/gutenberg /user/hduser/gutenberg

Disk uzerindeki listeye bakalim (goruldugu gibi, ls bile kullanamiyoruz, ozel ls lazim)

bin/hadoop dfs -ls /user/hduser

Kelime sayma (WordCount) Java programi Hadoop ana dizini icindeki bir jar icinde zaten var, onu kullanalim, alip islettirelim,

bin/hadoop jar hadoop-examples-*.jar wordcount /user/hduser/gutenberg /user/hduser/gutenberg-output

Is bitince sonuc gutenberg-output icinde, ekrana basalim

bin/hadoop dfs -cat /user/hduser/gutenberg-output/part-r-00000

Istersek kendi dizinimize geri alabiliriz,

mkdir /tmp/gutenberg-output
bin/hadoop dfs -getmerge /user/hduser/gutenberg-output /tmp/gutenberg-output
head /tmp/gutenberg-output/gutenberg-output

Ekranda soyle seyler gozukecek, kelime sayimi yapilmis, tabii ciktinin "basina" baktigimiz icin 1 tane bulunmus kelimeler goruyoruz bir suru, mesela AS-IS, vs.

"(Lo)cra"    1"1490    1"1498,"    1"35"    1"40,"    1"A    2"AS-IS".    1"A_    1"Absoluti    1"Alack!    1

Ilk Hadoop surecimizi boylece isletmis olduk.

---

[1] Eger dfs komutlarini hduser degil kendi kullanicimiz uzerinden isletmek istersek, ssh uzerinden bunu yapabiliriz. Nasil olsa ssh artik localhost uzerinde de calisacak sekilde ayarlandi, eger kullanicimizin .ssh/id_rsa.pub anahtarini /home/hduser/.ssh/authorized_keys uzerine eklersek, "ssh localhost -l hduser /home/hduser/Downloads/hadoop*/bin/hadoop dfs ..." gibi komutlari artik kendi kullanicimiz uzerinden isletebiliriz. 

[2] Kaynak

[3] HDFS dagitik (distributed) calisabilen bir dosya sistemi, cok buyuk tek bir dosyayi birden fazla makina uzerinde parca parca tutabiliyor yani (bunu arka planda kullaniciyi gostermeden kendisi hallediyor), hatta o parcalari tutan makinalarin cokme durumunda yedegine yonlendirme (failover) gibi ozellikleri de var. Buyuk veri islemesi gereken Hadoop ortami icin bicilmis kaftan, cunku tipik olarak cok buyuk bir veya birkac veri dosyasi elimize geciyor, ve bunlari "bir yerlere koymamiz" gerekiyor. Ya da esle-indirge surecinden yine buyuk veri uretiliyor, bu ciktilarin da HDFS uzerine koyulmasi mumkun.





