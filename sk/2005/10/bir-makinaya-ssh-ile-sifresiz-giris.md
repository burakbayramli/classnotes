# Bir Makinaya SSH ile Şifresiz Giriş

Bir makinadan diğerine hem şifresiz hem de güvenli bir şekilde girip,
orada komut işletmek, ya da oraya dosya kopyalamak için iki program
vardır: ssh ve scp. Kullanmak için komut satırından (`ssh` için)

```
ssh host1 -l remoteuser
```

komutunu kullandığınızda `host1` adlı makinaya `remoteuser`
kullanıcısı üzerinden login etmiş olurzzzz. Ya da uzak makinada bir
komut işletip sonucunu kendi makinanıza almak isterseniz (meselâ
ikinci makinada listeleme komutu olan ls işletelim), şunu yaparız

```
ssh host1 -l remoteuser ls
```

Eğer uzak makinaya bir dosya kopyalamak istersek

```
scp file.txt remoteuser@host1:/tmp
```

Bu komut ile uzaktaki makinaya sanki yerel dizinlerimiz arasında dosya
kopyalıyormuş kadar rahat bir şekilde bir dosya
kopyalayabiliyoruz. Girişte bahsettiğimiz ba- sit teknik işte
budur. ssh ve scp bir kez kurulduktan sonra, uzaktaki makina, yerel
makinanızın bir uzantısı hâline gelir. Bir makinayı uzaktan idare
etmek demek, ya bir dosya değişimi, ya da bir komut işletmek demek
olduğu için, bu iki programı kullanarak uzaktaki bir makinada
yapamayacağımız şey yoktur.

Şifresiz Kullanımı Kurmak

Eğer ssh’i hiçbir ek ayar yapmadan kullanırsanız, (ilk kurduğumuz haliyle)
her kullanışınızda size bir şifre sorulacaktır. Aynı şekilde scp komutu da böyle
davranır. Fakat, biz meselâ yüz tane makina için arka arkaya scp ya da ssh
kullanmamız gerekeceği için, şifre isteme işlemini iptal edip, güvenlik kon-
trolünü kullanıcıya sorulmayan başka bir şekilde yapmamız gerekiyor. Bu yöntem
de, açık / gizli (public / private) anahtarlar kullanarak yapılan güvenlik kon-
trolüdür.

Windows üzerinde ssh ve scp’nin işler kodlarını kurmayı, A.8 bölümünde
bulabilirsiniz. Linux üzerinde ssh ve scp genellikle otomatik olarak kurulur,
eğer kurulmamışsa, Linux kurulum disklerinizde bu programı bulabilirsiniz, ya
da admin’inize bu programları kurdurabilirsiniz.

Açık / gizli anahtar kurulumunu yapmak için şunları yapın: İlk önce kod
gönderimi ya da uzaktan idareyi yapan yerel makinamızı tanıtan bir gizli
anahtar, bir de açık anahtar üretmemiz gerekiyor.

```
$ ssh-keygen -t rsa
```

Sorulan sorular için hiç cevap girmeden ENTER’e basarak geçin. Bu
bittikten sonra, `$HOME/.ssh/` dizininiz icinde 2 dosya
göreceksiniz. Bu dosyalar id_¬ rsa.pub ve id_rsa dosyaları
olacaktır. HOME değişkeninin nerede olduğunu ko- mut satırından
Unix/Cygwin’de echo `$HOME` ile öğrenebilirsiniz. Windows’da
dosyaların nereye yazıldığı OpenSSH tarafından zaten ssh-keygen
sonunda size bildirilecektir.

Biraz önce üretilen dosyalardan `id_rsa`, gizli anahtarınızdır. Dosya
`id_rsa.pub` ise açık anahtarınızdır. Şimdi, `id_rsa.pub` kayıdındaki
açık anahtarı, uzaktan erişeceğiniz servis bilgisayarına FTP ya da scp
ile gönderin (scp kullanırsanız, -şimdilik- şifre girmeniz gerekecek
tabii ki). Sonra, uzaktaki bilgisa- yardaki kullanacağınız kullanıcı
hesabına girin, hesabın üst seviyesinde `$HOME/.ssh/` dizini altına
id_rsa.pub kayıdını bırakın. Sonra, servis sisteminde

```
cat > $HOME/.ssh/id_rsa.pub >> authorized_keys
```

komutunu calıştırın. Kuruluş işlemi bundan ibarettir. Bu son işlemden
sonra artık uzaktan işlettiğiniz ssh ve scp işlemleri şifresiz bir
şekilde işinizi yapmanıza izin verecektir.

Açık anahtarımızı servis makinasına eklemek için, Unix cat komutu ile
`>>` işlecini kullandığımıza dikkat edelim. Bu demektir ki, birden
fazla .pub dosyasına tek bir authorized_keys dosyasına ekleyebiliriz
(ve birden fazla kullanıcıyı desteklebilmek için bunu yapmamız
gerekir). Böylece aynı makinaya erişen birden fazla erişen kişi, aynı
servis makinasına ve aynı kullanıcıya değişik açık anahtarlar ile
erişebilir. Bkz [2] 

Uzak Dosya Erişimi

Uzaktaki makinanın herhangi bir dizin hiyerarşisini belli bir
noktasından alıp, `ssh` üzerinden bizim yerel makinadaki bir boş
dizine "montelemek (mount)" mümkün, `sshfs` ile. Kurmak için

```
sudo apt-get install sshfs
```

Şimdi mesela ben yerel `user1` kullanıcısı olayım,
`/home/user1/Downloads/uzak` boş dizinine (yoksa yaratırım)
192.168.1.1 makinasındaki `/home/ahmet/Documents` dizinini monteleyelim,

```
sudo sshfs -o allow_other,default_permissions burak@192.168.1.1:/home/ahmet/Documents /home/user1/Downloads/uzak
```

Önce kendi (root kabüllü) şifreniz, sonra uzaktaki makinanın şifresi
sorulacak. Girince işlem tamamlanır, `mount` komutuyla montelenen
dizinler listesinde uzaktaki makina artık görülmelidir

Artık kendi `/home/user1/Downloads/uzak` dizinime gittiğimde, uzaktaki
`/home/ahmet/Documents` dizinini görürüm, hangi programı kullanırsam
kullanayım bu işler.

Eski Anlatım

SSH gizli/açık anahtar kavramına göre çalışır (public/private key
encryption). Uzak bir makinada şifresiz giriş yapmak, komutlar
işletmek, ve scp ile şifresiz kopyalama yapmak için, şunları yapmamız
lâzım.

Kendimiz için bir gizli/açık anahtar çifti yaratmak için şu komutu
çalıştırın.

```
$ ssh-keygen -t rsa
```

Sorulan sorular için hiç cevap girmeden ENTER'e basarak geçin.

Bu bittikten sonra, `$HOME/.ssh` dizini icinde 2 dosya
göreceksiniz. id_rsa.pub ve id_rsa. (`$HOME`'un nerede olduğunu
Cygwin'den ya da Linux komut satırından echo `$HOME` ile
öğrenebilirsiniz). Bu kayıtlardan id_rsa isimli olan sizin gizli
anahtarınızdır, id_rsa.pub ise açık anahtarınızdır. Şimdi, `id_rsa.pub`
kayıdını, erişeceğiniz bilgisayara (meselâ CVS havuzunu tutan
bilgisayara) `scp` ya da `ftp` ile gönderin (scp kullanilinca simdilik
sifre sorulacak). O bilgisayardaki kullanıcı adınız altında girin, ve
`$HOME/.ssh` dizini altına `id_rsa.pub` kayıdını bırakın.

Sonra, sunucu sisteminde sunu calıştırın:

```
cat $HOME/.ssh/id_rsa.pub >> authorized_keys
```

Kutlarım. Artık ssh ya da CVS komutları kullanırken şifreye
ihtiyacınız olmayacak. Artık `ssh kullanıcı@makina` ile şifresiz
yapabiliriz, hatta `ssh kullanıcı@makina komut` ile uzaktaki
makinadaki bir programı başlatıp sonuçlarını direk kendi makinamızda
görebiliriz.

Unix `cat` komutu ile `>>` işlecini kullandığımıza dikkat edin. Bu
demektir ki, birden fazla .pub dosyasına tek bir authorized_keys
dosyasına ekleyebiliriz. Yâni, birden fazla erişen kişi, aynı servis
makinasına ve aynı kullanıcıya değişik açık anahtarlar ile erişebilir.

Önemli bir ek detay, servis tarafındaki authorized_keys dosyasının
güvenliğinin çok fazla açık olmamasıdır. Eğer dosyanın Unix bazında
güvenliği çok açıksa, sshd bağlanmaya çalışan ssh/scp komutunu bir
şifre girmeye zorlayacaktır. authorized_keys dosyasının yeterince
kapalı hale getirmek için

```
chmod 600 authorized_keys
```

Çift Anahtar Bazlı Şifresiz Giriş Ne Kadar Güvenli?

Bu kullanım güvenli, çünkü bir korsan, servis bilgisayarına sadece ve
sadece id_rsa dosyasına sahipse başarabilir. Bu dosya da, sizin
bâğlanan bilgisayarınızda duruyor olacaktır, ve o dosyayı almak için
bâğlanan bilgisayara, (yâni ssh-keygen komutunu işlettiğiniz
bilgisayara) sızılması gerekecektir. Demek ki akılda tutulması gereken
kıstas şudur: Çift anahtar yaklaşımını kullanarak iki makina arasında
şifresiz giriş kurduğunuzda, bağlanılan makina, bağlanan makinanın
güvenli olduğu kadar güvenli olacaktır.

Sorun Çıkarsa

Bazı tipik kuruluş problemleri şunlar oluyor. SSH bağlanamama
problemlerinde, sunucu bilgisayarında sshd programınının calıştığını
kontrol edin. "ps -eaf | grep sshd" ile bunu kontrol edebilirsiniz.

Bağlanan bilgisayarlarda Cygwin'e özel bir problem de Windows'da
çıkabiliyor. Bunun tarifi ve tamiri de şöyle:

Cygwin ssh programı (OpenSSH), gizli anahtarın üzerindeki erişim
haklarının "kullanıcıya özel" olmasını istiyor. Bu normal tabii çünkü
bu anahtar gizli, ve her kullanıcı tarafından okunamaması lâzım.

Hata şöyle:

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@@         WARNING: UNPROTECTED PRIVATE KEY FILE!          @
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Permissions 0644 for '/cygdrive/h/.ssh/id_rsa' are too open.

It is recommended that your private key files are NOT accessible by
others.

This private key will be ignored.

Hiç problem degil (diye düşünüyoruz) chmod, vs. iş biter. Bir tek
problem, Windows ve Unix erişim hakları yöntemleri uyuşmaması. Chmod'u
çalıştırdığınızda, komut bir sey yapmış gibi geri geliyor, fakat
dosyada bir değişiklik olmuyor. Unix dosya haklarının Cygwin'de
"simule" edilmesi için, cygwin.bat içine şunu eklemek lazım.

```
set CYGWIN=tty ntea
```

Bundan sonra

```
$ chmod 0600 id_rsa
```

.. ve bundan sonra ssh ve scp komutlarınız düzgün çalışacak.


Kaynaklar

[1] https://www.digitalocean.com/community/tutorials/how-to-use-sshfs-to-mount-remote-file-systems-over-ssh

[2] <a href="bir-makinaya-ssh-ile-sifresiz-giris-video.html">Video ile Anlatım</a>

