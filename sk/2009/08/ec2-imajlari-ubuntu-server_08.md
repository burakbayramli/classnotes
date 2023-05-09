# EC2 Imajlari, Ubuntu Server

Uygulamamizi sonuc ortamina (production) gondermeye karar verdik;
diyelim ki barindirma servisi olarak Amazon EC2 servisini sectik. Daha
onceki su yazida genel EC2 komutlarini tanidik.Simdi bir Ubuntu bazli
servis makinasini nasil yaratacagimiza gelelim:Bildigimiz gibi, EC2
"makina imajlari (machine image)" kavramiyla isliyor. Bir makinayi
sifirdan yaratmak icin gerekecek tum dosyalar (bunlar oldukca buyuk
yer tutuyor dogal olarak) yine Amazon'un buyuk olcekli dosyalari
tutmak icin sundugu S3 servisi uzerinden herkese sunuluyor. Bu
servisteki tum Ubuntu AMI (makina imajlarini) gormek
icin

ec2-describe-instances -a | grep ubuntuisletmek yeterli.

Soyle bir liste geri gelecek:...IMAGE ami-5059be39
canonical-cloud-us/ubuntu-intrepid-20090422-i386.manifest.xml099720109477
available public i386machine aki-714daa18 ari-6a5bbc03 I...Ustteki
imaj bizim sectigimiz imajlardan biri. Canonical Ubuntu sistemini
yaratan sirket zaten, bu sebeple "kaynaktan" gelecek imajin saglam
olacagi dusuncesinden hareketle onu seciyoruz. Bu konuyu onceki yazida
isledigimiz icin daha fazla detaya girmiyoruz. Simdi, ek olarak yine
diyelim ki, "sudo apt-get install" ile bu imajdan yaratilan makinaya
bir suru yeni program ekledik. Bu programlari kaybetmek istemiyoruz,
cunku ec2-terminate-instances ile biraz onceki yarattigimiz makinayi
kapatinca imaja dahil olmayan her turlu yeni dosya kaybolacaktir. O
zaman bu makinanin son halini yeni bir imaj olarak "yakmamiz"
gerekecek. Bunu yapmak icin EC2 makinasinda sunlari isletebiliriz.sudo
ec2-bundle-vol -d /mnt -k /mnt/[ACIK SSH ID DOSYASI] -c /mnt/[CERT
DOSYASI] -u [KULLANICI ISMI] -r i386 -p [IMAJ ISMI][] icindeki
dosyalari EC2 hesabinizda anahtarlarinizi gosteren yerden ya
indirebilir, ya da kopyalayarak yapistirabilirsiniz.

Acik ssh id dosyasi kisisel bilgisayarinizda EC2 sistemlerine erismek
icin surekli kullandigimiz dosya aslinda. Tum bu dosyalari alip EC2
makinanizdaki /mnt/ altina kopyalayin. Niye /mnt? Cunku yakma islemi
sirasinda /mnt altindaki dosyalar imaja yazilmiyor ve bu "gizli"
dosyalarin imaj icinde olmamasi daha iyi.Imaji yarattiktan sonra onu
alip S3 depolama sisteminize yazmaniz lazim, kaybolmamasi icin. EC2
hesabi olan herkes bir S3 hesabi acabilir. Bunu yaptiktan
sonra;ec2-upload-bundle -b [BUCKET ISMI] -m /mnt/[IMAJ
ISMI].manifest.xml -a [ACCESS KEY] -s [SECRET KEY]Bucket ismi, sizin
sececeginiz bir "dizin" ismi olacak. S3 bu isimleri "global" isimler
olarak aliyor, yani "mybundle" gibi isimler cok genel isimler
secilirse cakisma olur, projenize ozel bir isim olmasi daha iyi
olur.S3 dosyalarinizi gormek icin S3 Organizer iyi bir Firefox
eklentisi.Bunun ertesinde, artik EC2 makinanizi kapatsaniz bile,
yarattiginiz imaj dosyalarini baz alarak hemen ayni makinayi sifirdan
yaratabilirsiniz. Bunu yapmadan once imaji EC2 hesabinizdan "kayit
etmis olmaniz (register image)" lazim. [IMAJ ISMI].manifest.xml ile
bunu hemen yapabilirsiniz, ardindan komut satirinde
ec2-describe-images komutunu isletirseniz, kayit ettigimiz imajin "i-"
ile baslayan kimligini alabilirsiniz, bundan sonra ec2-run-instances
komutu ile bildigimiz islemleri gerceklestirebiliriz.Bir kaynak:





