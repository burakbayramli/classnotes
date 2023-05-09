# Starcluster

Cok makinali, ya da cok cekirdekli (core) ortaminda paralel islem
yapilmasini saglayan ve Amazon EC2 ortamini baz alinarak yazilmis acik
kodlu yeni bir cozum - Starcluster.

http://star.mit.edu/cluster/

Amazon EC2 bilindigi gibi Amazon'un sanal makina kullanimini /
kiralanmasini saglayan bir sistemdir. Belli bir kira karsiliginda Unix
komut satirina kavusmanin bir yolu! Sistem cok populer ve sanal
makinalarin uzerinde hangi yazilimi tasiyacagi AMI imajlari ile
tanimlanabiliyor ve insanlar su anda populer AMI'leri birbiriyle
paylasiyor.

Starcluster da bu AMI imajlarindan biri aslinda. Bu AMI'yi alip her
makinasi Starcluster imaji tasiyan Amazon EC2 kumesini baslatmak
mumkun, imaj uzerinde numpy, scipy, blas gibi numerik kutuphaneler
bastan hazir. Ek olarak bu proje bazi araclar, disaridan
baglanilmasinin saglayan eklentiler (plugin) de sagliyor.

Starcluster'in en onemli ozelligi iPython Parallel ile direk
calisabiliyor olmasidir. Yani bir ipython not defteri acip, bu defteri
uzaktaki bir Starcluster kumesine bagliyabiliyorsunuz, ve hucrelere
girdiginiz kod onlarca, yuzlerce makina uzerinde paralel sekilde
isletmek mumkun oluyor. Konu hakkinda bilgilendirici bir prezentasyon
surada, uzaktaki kumeye baglanma, kod isletmeyi gostermis.

http://twiecki.github.com/zipline_in_the_cloud_talk/#/

Bu proje Hadoop'a alternatif midir? Bir anlamda oyle. Starcluster EDS
adli bir dizin sistemi uzerinden veriye erisiyor, Hadoop bu isi HDFS
ile yapiyor. Fark surada, HDFS buyuk bir dosyayi kumesindeki her
makinaya paylastirir, ve esle-indirge komutu geldigi zaman, bu
islemler veriye yakin makinaya "gonderilirler". Diger yandan mesela
Amazon Elastic Map Reduce uzerinden Hadoop kullananlar veriyi
cogunlukla S3 uzerinde tutuyorlar, kume kurulup, kullanilip hemen
yokediliyor, yani S3'ten erisimin EDS'ten erisime benzedigi iddia
edilebilir, bu EC2 baglaminda tabii.

Not: Cok cekirdekli bir mikroislemci kullanan tek makina uzerinde bile
Starcluster servisi isletilebilir, bunun ornekleri de ustteki
baglantida var.






