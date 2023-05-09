# Amazon EC2 ile Barindirma

Amazon.com şirketi, kitap vb. şeyler satmanın yaninda, bir süredir
barındırma (hoşting) servisi veriyor. Amazon.com İnternet dünyasının
artık en eski e-ticaret sitelerinden, ve kendi e-ticaret işlerini
sürdürebilmek için satış yapan sitelerinin de devasa boyutlarda olması
gerekliydi. Bu ölçekte bir siteyi bu kadar uzun süre idare ederken,
kendilerine sağladıkları servisi dışarıdaki son kullanılara da sağlama
gibi bir fikre erişmiş olmalılar.  Bu bizim gibi İT programcıları için
iyi haber.

Amazon EC2 barındırma servisi, komut satırından yazabileceğiniz birkaç
satır sayesinde Amazon altyapısı içinde istediğiniz sayıda ve
donanımda "sanal makinayı" yaratabilmenizi sağlıyor. Bu makinalar
bildiğimiz araçlar ssh ve scp kullanarak erişilebiliyor, ve bu
makinalara giriş yapınca yine alışageldiğimiz Linux ya da Windows
işlemlerini yapabiliyoruz. Bu makinalara yeni programlar kurabiliyor,
dosya kopyalayıp, silebiliyor, program işletebiliyoruz. Makinaların
her birine root erişimimiz oluyor.

EC2 servisinde bir makinayı yaratmak için öncelikle bir "imaj"
gerekli. EC2 portföyünde belli imajlar var, bu imajlar önceden
başkaları tarafından hazırlanmış (yazılım bağlamında) makina kurulum
"tipleri". Mesela bir imaj Redhat Fedora Linux işletim sistemi
üzerinde MySQL ve Apaçhe programlarını içeren bir "paket"
olabiliyor. Biz komut satırından bir makinayı yaratırken her zaman bir
imaj ismi veriyoruz, ve makina yaratılınca üzerinde imajın parçası
olan programlar hazır halde bizi bekliyor oluyor.

Bir makinayı sıfırdan alıp (scp ile dışarıdan istediğimiz programları
aktararak kendi imajımızı kendimiz de yaratabiliriz, fakat bu uzun
süreli bir işlem olurdu tabii. Ayrıca hazır bir imajdan başlayarak,
kendi eklerimizi yaparak bize özel bambaşka bir imajı da
yaratabiliyoruz. Bu imajı başkaları ile paylaşmak ta mümkün, ve
başkaları bu imaj üzerinden kendi sanal makinaları yarattıklarında
aynı programları görüyor oluyorlar.

Tahmin edilebileceği üzere, imajlar oldukca büyük boyutlarda olacak,
bu yüzden Amazon bu imajları kendi dosyalama sistemi içinde muhafaza
ediyor. İmajları lokal bilgıyarınıza indirmenize gerek
kalmıyor. Amazon dosyalama servisinin ismi

S3.  Odeme nasil yapiliyor?

EC2 üzerinde yarattığınız bilgisayarlar ne kadar süre kullanımda ise o
süre karşılığında bir "kira" ödüyorsunuz. Mesela ufak boyutlarda bir
sanal bilgisayar her saat için $0.10. Ayrıca bu makinadan gelen/giden
veriye bakarak bir takım bedeller var. Şu sayfadan tahmini kullanım
verilerini girerek aylık ödeyeceğiniz bedeli hesaplayan bir araçı
bulabilirsiniz.  Amazon EC2'yi kullanmaya başlamak için kredi kart
bilginizi girmeniz gerekiyor; kullanımınızı baz alarak Amazon otomatik
olarak bedeli kartınızdan kesiyor.

Bazı komutlar (komutları nereden indirilip kullanmak için çevre
değişkenlerini nasıl set etmeniz gerektiğini şuradan bulabilirsiniz):
Yarattığınız makinalara bir anahtar çifti (key pair) yaratmanız lazım;
bu ec2-add-keypair [isim] ile. Buradan gelen verileri bir dosyaya
kaydedin, ve artık bir makinayı yaratıp, erişirken hep bu dosyayı
referans edeceksiniz. Mesela bundan sonra id_rsa-gsg-keypair isimli
dosyayı referans alalım.  Bir makinayı ami-4ea34727 imajını baz alarak
yaratmak ve başlatmak için

```
ec2-run-instances ami-4ea34727 -k id_rsa-gsg-keypair
```

Baslatmis oldugunuz tum makinalari gormek icin

```
ec2-describe-instances
```

Ustteki komuttan makinanizin host adresi ve instance kimligi gibi
bilgileri alabilirsiniz. Instance bilgisi "i-" ile baslayan bir kimlik
olacak. Bu kimligi bu makinaya referans etmek icin
kullanabilirsiniz. Mesela makinayi durdurmak icin;

```
ec2-terminate-instances [kimlik]
```

Makinaniza ssh ile baglanmak icin

```
ssh -i id_rsa-gsg-keypair root@[makina ismi].
```

Makina ismini yukarıda bahsedilen listeden alın.  Dosya kopyalamak
için

```
scp -i id_rsa-gsg-keypair [dosya]
root@[makina]
```

Ben şu şekilde bir Ünix alias yarattım; işleri rahatlaştırıyor.

```
alia swinst='ec2-describe-instances'
alias escp='scp -i id_rsa-gsg-keypair'
alias essh='ssh -i id_rsa-gsg-keypair '
```

Daha fazla bilgiyi baslangic (getting started) dokumanlarindan
alabilirsiniz. Bu dokumandaki ilk ornegi harfi harfine takip edin. Iyi
bir baslangic saglayacaktir.





