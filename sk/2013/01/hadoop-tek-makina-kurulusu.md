# Hadoop (Tek Makina Kurulusu)

Hadoop eszamanli, paralel veri isleme tekniklerinden olan Esleme /
Indirgeme (Map-Reduce) kavraminin kodlanmasini / isletilmesini
saglar. Hadoop servisleri ozel bir sekilde yazilmis EI kodlarini alir,
onlari birden fazla makinaya dagitir, onlar icin gereken verileri
parcalayarak / bolerek parca parca bu sureclere verir, analiz
sonuclarini birlestirir (yine kullanicinin verdigi programa bakarak)
ve sonuclari toparlayarak tek sonuca indirger. Hadoop Java ile
yazilmistir, fakat surecleri (jobs) pek cok dilde yazilabilirler. Once
Hadoop kuralim, ve bu ornek icin tek makina uzerinde birden fazla
Hadoop servisi isletebilecek sekilde bu kurulumu yapalim.

Once Hadoop icin ayri bir kullanici gerekli

```
$ sudo addgroup hadoop
$ sudo adduser --ingroup hadoop hduser
```

Bu kullanici ssh cagrisi yapacak, aslinda ssh ile makinamiz yine kendisine baglanacak ama, sistem boyle isliyor, tek makina kurulusunda bile bunlari yapmak lazim, 

```
$ ssh-keygen -t rsa -P ""
$ cat $HOME/.ssh/id_rsa.pub >> $HOME/.ssh/authorized_keys
```

Dikkat: Ustteki komutlari hduser icin ayni sekilde isletmek lazim.  

Eger ssh localhost ile baglanti olmazsa, belki SSH servisi
kurulmamistir, onu kurariz,

```
$ sudo apt-get install openssh-server
```

Simdi alttaki adresten hadoop indirelim ve lzop kuralim

http://www.apache.org/dyn/closer.cgi/hadoop/core

```
sudo apt-get install lzop
```

Alttakileri hem hduser hem de kendi kullanicimiz icin .bashrc icine ekleyelim 

```
export HADOOP_HOME=[HADOOP DIZINI]
export JAVA_HOME=[JAVA JDK DIZINI]
unalias fs &> /dev/null
alias fs="hadoop fs"
unalias hls &> /dev/null
alias hls="fs -ls"lzohead () {
    hadoop fs -cat $1 | lzop -dc | head -1000 | less}
export PATH=$PATH:$HADOOP_HOME/bin
```

Simdi [HADOOP DIZINI]/conf/hadoop-env.sh icinde

```
# The java implementation to use.
  Required.
export JAVA_HOME=[JAVA HOME]
```

yapin. JAVA_HOME ne ise ustte onu veririz, which java ile bu dizini
bulabilirsiniz, bazen bu komut bir sembolik baglanti (symlink) verir,
ls -al ile bu baglantilari izleyerek gercek adrese
ulasabilirsiniz. Eger Java JDK kurulmamissa, 

```
sudo apt-get install openjdk-6-jdk
```

Simdi Hadoop icin gereken islem dizinlerini kuralim, ve onlari hduser'a verelim,

```
sudo mkdir /app
sudo mkdir /app/hadoop
sudo mkdir /app/hadoop/tmp 
sudo chown hduser:hadoop /app/hadoop/tmp
conf/core-site.xml icinde

```
  <property>
    <name>hadoop.tmp.dir</name>
    <value>/app/hadoop/tmp</value>
    <description>A base for other temporary directories.</description>
  </property>
  <property>
    <name>fs.default.name</name>
    <value>hdfs://localhost:54310</value>
    <description>The name of the default file system.
  A URI whose
    scheme and authority determine the FileSystem implementation.
  The
    uri's scheme determines the config property (fs.SCHEME.impl) naming
    the FileSystem implementation class.
  The uri's authority is used to
    determine the host, port, etc. for a filesystem.</description>
  </property>
```

conf/mapred-site.xml icinde

```
<property>  <name>mapred.job.tracker</name>
  <value>localhost:54311</value>
  <description>The host and port that the MapReduce job tracker runs  at.
  If "local", then jobs are run in-process as a single map  and reduce task.
  </description>
</property>
```

conf/hdf-site.xml icinde

```
<property>
  <name>dfs.replication</name>
  <value>1</value>
  <description>Default block replication.
  The actual number of replications can be specified when the file is created.
  The default is used if replication is not specified in create time.
  </description></property>
```

Simdi hduser olarak sunu isletin

```
[HADOOP DIZINI]/bin/hadoop namenode -format
```

Simdi kendi kullaniciniz icinden Hadoop kurulus dizinine gidin, ve
chmod a+w ile bu dizine herkes icin okuma hakki verin, boylece hduser
oraya yazabilsin. 

Bu komut daha once tanimlanan /app//hadoop altinda gereken dizinleri formatlayacak.

```
[HADOOP DIZINI]/bin/start-all.sh
```

ile tum servisleri baslatabilirsiniz.

```
jps
```

komutu

```
25612 TaskTracker
25870 Jps
25290 SecondaryNameNode
25053 DataNode
25380 JobTracker
24819 NameNode
```

listesini dondurmeli.

Eger tek makina uzerinde birden fazla cekirdek (core) uzerinde islem
yapmak istiyorsak mapred-site.xml dosyasi icindeki  mapred.map.tasks
ayari kullanilabilir, buraya 4-5 gibi bir sayi gerekiyor. Ayrica
mapred.tasktracker.map.tasks.maximum ayari var, ki bu da eszamanli
olarak isleyen islemlerin (task) en fazla ne kadar olabilecegini
belirtleyen bir ust sinir, bir onceki ayar ile kismen
ilintili. Birinci ayardaki sayi bu ust sinir sayisindan fazla
olamiyor.
