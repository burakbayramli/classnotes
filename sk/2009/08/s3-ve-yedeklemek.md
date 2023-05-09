# S3 ve Yedeklemek

Bir EC2 makinasi kapandiginda bir imaja yakilmamis her verisini
kaybeder. O zaman bir MySql tabanini EC2 uzerinde isletiyorsak,
dinamik olarak surekli guncellenen verisini kaybetmemenin bir yolunu
bulmamiz lazim. S3 servisi buyuk dosyalarinizi Amazon sistemi icinde
depolamanizi sagliyor. O zaman EC2 makinasindan rutin olarak (cron
uzerinden mesela) MySql'in yedegini alip, bu yedegi S3'e gondermek bir
cozum olabilir.S3'e script ortamindan erisebilmek icin Ubuntu EC2
makinanizdasudo apt-get install s3cmdkomutunu isletin. Bu komut s3cmd
adli bir yardimci programi kuracak. Bu programi sc3cmd --configure ile
isletin; sorulan sorular EC2 acik ve gizli anahtarlariniz.

Bu anahtarlar girildikten sonra s3cmd onlari hep hatirlayacak ve
bundan sonra isletilen tum s3cmd komutlari o anahtarlar uzerinden
gerceklestirilecek, yani tum islemler ayni hesabin isaret ettigi
kullanici uzerinden yapilacak. Bu kullanicinin kullanmasi icin yeni
bir dizin yaratmak;

s3cmd mb s3://[BUCKET ISMI]

Herhangi bir dosyayi oraya kopyalamak icins

3cmd put /tmp/test s3://[BUCKET ISMI]

[BUCKET ISMI] isminin "global" bir isim oldugunu belirtmistik, yani
yeterince tekil olan bir isim secelim.Bu kadar; artikmysqldump -u root
--quick [DB] | gzip > /vs/vs/db-backup.gzgibi bir komutla rutin olarak
yaratilan yedek dosyasini (db-backup.gz yani) s3cmd ile S3 hesabimiza
gonderebiliriz.Kaynak





