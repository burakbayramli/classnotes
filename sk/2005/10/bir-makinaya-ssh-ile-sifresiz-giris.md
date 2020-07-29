# Bir Makinaya SSH ile Şifresiz Giriş

SSH gizli/açık anahtar kavramına göre çalışır (public/private key
encryption). Bu sistemi başka yazılarda anlatacağız. Şimdilik, uzak
bir makinada şifresiz giriş yapmak, komutlar işletmek, ve scp ile
şifresiz kopyalama yapmak için, şunları yapmamız lâzım.

Kendimiz için bir gizli/açık anahtar çifti yaratmak için şu komutu
çalıştırın.

```
$ ssh-keygen -t rsa
```

Sorulan sorular için hiç cevap girmeden ENTER'e basarak geçin.

Bu bittikten sonra, $HOME/.ssh/ dizini icinde 2 dosya
göreceksiniz. id_rsa.pub ve id_rsa. ($HOME'un nerede olduğunu
Cygwin'den ya da Linux komut satırından echo $HOME ile
öğrenebilirsiniz). Bu kayıtlardan id_rsa isimli olan sizin gizli
anahtarınızdır, id_rsa.pub ise açık anahtarınızdır. Şimdi, id_rsa.pub
kayıdını, erişeceğiniz bilgisayara (meselâ CVS havuzunu tutan
bilgisayara) scp ya da ftp ile gönderin (scp kullanilinca simdilik
sifre sorulacak). O bilgisayardaki kullanıcı adınız altında girin, ve
$HOME/.ssh/ dizini altına id_rsa.pub kayıdını bırakın.

Sonra, sunucu sisteminde sunu calıştırın:

```
cat $HOME/.ssh/id_rsa.pub >> authorized_keys
```

Kutlarım. Artık ssh ya da CVS komutları kullanırken şifreye
ihtiyacınız olmayacak.

Not: `authorized_keys` dosyasina ekleme yapmak yerine, baglanilan
bilgisayardan ssh-copy-id komutu da ayni islemi yapiyor, tabii ki
sifre soruluyor (simdilik), fakat bu yontem daha kolay.

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
@@@@         WARNING: UNPROTECTED PRIVATE KEY FILE!          @
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







