# Amazon EC2 ile Barindirma

Amazon.com sirketi, kitap vb. seyler satmanin yaninda, bir suredir
barindirma (hosting) servisi veriyor. Amazon.com Internet dunyasinin
artik en eski e-ticaret sitelerinden, ve kendi e-ticaret islerini
surdurebilmek icin satis yapan sitelerinin de devasa boyutlarda olmasi
gerekliydi. Bu olcekte bir siteyi bu kadar uzun sure idare ederken,
kendilerine sagladiklari servisi disaridaki son kullanilara da saglama
gibi bir fikre erismis olmalilar.  Bu bizim gibi IT programcilari icin
iyi haber. Amazon EC2 barindirma servisi, komut satirindan
yazabileceginiz birkac satir sayesinde Amazon altyapisi icinde
istediginiz sayida ve donanimda "sanal makinayi" yaratabilmenizi
sagliyor. Bu makinalar bildigimiz araclar ssh ve scp kullanarak
erisilebiliyor, ve bu makinalara giris yapinca yine alisageldigimiz
Linux ya da Windows islemlerini yapabiliyoruz. Bu makinalara yeni
programlar kurabiliyor, dosya kopyalayip, silebiliyor, program
isletebiliyoruz. Makinalarin her birine root erisimimiz oluyor.  EC2
servisinde bir makinayi yaratmak icin oncelikle bir "imaj"
gerekli. EC2 portfoyunde belli imajlar var, bu imajlar onceden
baskalari tarafindan hazirlanmis (yazilim baglaminda) makina kurulum
"tipleri". Mesela bir imaj Redhat Fedora Linux isletim sistemi
uzerinde MySQL ve Apache programlarini iceren bir "paket"
olabiliyor. Biz komut satirindan bir makinayi yaratirken her zaman bir
imaj ismi veriyoruz, ve makina yaratilinca uzerinde imajin parcasi
olan programlar hazir halde bizi bekliyor oluyor. Bir makinayi
sifirdan alip (scp ile disaridan istedigimiz programlari aktararak
kendi imajimizi kendimiz de yaratabiliriz, fakat bu uzun sureli bir
islem olurdu tabii. Ayrica hazir bir imajdan baslayarak, kendi
eklerimizi yaparak bize ozel bambaska bir imaji da yaratabiliyoruz. Bu
imaji baskalari ile paylasmak ta mumkun, ve baskalari bu imaj
uzerinden kendi sanal makinalari yarattiklarinda ayni programlari
goruyor oluyorlar.  Tahmin edilebilecegi uzere, imajlar oldukca buyuk
boyutlarda olacak, bu yuzden Amazon bu imajlari kendi dosyalama
sistemi icinde muhafaza ediyor. Imajlari lokal bilgiyariniza
indirmenize gerek kalmiyor. Amazon dosyalama servisinin ismi S3.
Odeme nasil yapiliyor? EC2 uzerinde yarattiginiz bilgisayarlar ne
kadar sure kullanimda ise o sure karsiliginda bir "kira"
oduyorsunuz. Mesela ufak boyutlarda bir sanal bilgisayar her saat icin
$0.10. Ayrica bu makinadan gelen/giden veriye bakarak bir takim
bedeller var. Su sayfadan tahmini kullanim verilerini girerek aylik
odeyeceginiz bedeli hesaplayan bir araci bulabilirsiniz.  Amazon
EC2'yi kullanmaya baslamak icin kredi kart bilginizi girmeniz
gerekiyor; kullaniminizi baz alarak Amazon otomatik olarak bedeli
kartinizdan kesiyor.  Bazi komutlar (komutlari nereden indirilip
kullanmak icin cevre degiskenlerini nasil set etmeniz gerektigini
suradan bulabilirsiniz): Yarattiginiz makinalara bir anahtar cifti
(key pair) yaratmaniz lazim; bu ec2-add-keypair [isim] ile. Buradan
gelen verileri bir dosyaya kaydedin, ve artik bir makinayi yaratip,
erisirken hep bu dosyayi referans edeceksiniz. Mesela bundan sonra
id_rsa-gsg-keypair isimli dosyayi referans alalim.  Bir makinayi
ami-4ea34727 imajini baz alarak yaratmak ve baslatmak icin
ec2-run-instances ami-4ea34727 -k id_rsa-gsg-keypair Baslatmis
oldugunuz tum makinalari gormek icin ec2-describe-instances Ustteki
komuttan makinanizin host adresi ve instance kimligi gibi bilgileri
alabilirsiniz. Instance bilgisi "i-" ile baslayan bir kimlik
olacak. Bu kimligi bu makinaya referans etmek icin
kullanabilirsiniz. Mesela makinayi durdurmak icin;
ec2-terminate-instances [kimlik] Makinaniza ssh ile baglanmak icin ssh
-i id_rsa-gsg-keypair root@[makina ismi]. Makina ismini yukarida
bahsedilen listeden alin.  Dosya kopyalamak icin scp -i
id_rsa-gsg-keypair [dosya] root@[makina] Ben su sekilde bir Unix alias
yarattim; isleri rahatlastiriyor.  alias
inst='ec2-describe-instances'alias escp='scp -i id_rsa-gsg-keypair
'alias essh='ssh -i id_rsa-gsg-keypair ' Daha fazla bilgiyi baslangic
(getting started) dokumanlarindan alabilirsiniz. Bu dokumandaki ilk
ornegi harfi harfine takip edin. Iyi bir baslangic saglayacaktir.




