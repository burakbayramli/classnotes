# Samsung Android Tablet Üzerinde Linux Ubuntu

Android üzerinde tam tekmilli Linux işletmek mümkün. Android alt
seviyeleri bilindiği gibi çekirdek seviyesinde Linux kullanır, bu
cekirdek uzerine birkac katman Java, gorsel bilesenler,
vs. eklenmistir. Termux uygulaması Android'in cekirdegi ile iletisim
kurar, ve bildik Unix sistem cagrilarinin islemesini saglar, aynı
zamanda kullanıcıya Linux sistemlerinden tanıdık bir komut satırı
programı sunar. Bu program üzerine pek çok bilinen Unix komutu `pkg
install` ile kurulabilir. Bu konuyu daha önce işlemiştik [4].

Bu yazıyı paylaştığımızdan bu yana Termux bazı iniş çıkışlar yaşadı,
Google Play Store'daki versiyon problem çıkartıyordu, kurulumlar tam
yapılamıyordu, daha sonra bir Android versiyonunda Termux gibi
uygulamaların bir süre sonra süreçlerinin (process) isletim sistemi
tarafından zorla durdurulduğu söylendi. Yeni versiyonlarda bu
problemler düzeltilmiş, Termux'u artık Github üzerinden kuruyoruz, ve
süreç durdurması için ise bir Android 14 üzerinde yeni bir seçenek
var, bunu kullanıyoruz.

Yazımız temel olarak Samsung Galaxy Tab A9 tabletini merkez
alacak. Şuradaki arkadaş [2] A10 tablet kullanmış. Bu donanımlar
saglamdir, işletim sistem versiyonlari Android 14, gerekli seçeneğe
sahipler. Donanımı açınca önce `Settings` | `About Phone` seçip oradan
`Build Number` diyen yere gidiyoruz, ve yedi kere ardı ardına bu yazı
üstüne basıyoruz. Bitince bu işlem ile Android'in geliştirici seviyesi
(developer mode) aktif edilmiş oluyor. Şimdi `Developer Options`
görünür halde olmalı, buna giderek `Disable Subprocess Restrictions`
seçeneğini seçiyoruz.

Termux kurulumu için [2] bağlantısından Termux apk'si indirilir. Bu
indirilen apk üzerinde seçim yapıp onu kurarız, uyarıları iptal edip
devam ederiz, işlemi tamamlarız. Artık bir Termux ikonu program
listesinde gözüküyor olmalı. İkona tıklayıp komut satırına gireriz,
burada `pkg install` ile istenen programları kurmak artık mümkün.

Proot

Fakat hala elimizdeki tam tekmilli bir Unix değil. Unix'te kullanılan
programların Android için derlenmiş hallerini kullanıyoruz. Eğer ciddi
bir Linux, yani dosya sistemi, kullanıcı idaresi vs ile birlikte olan,
bunun için Proot [3] kullanabiliriz.

Proot bir emülasyon sistemidir. Mesela Ubuntu Linux kullanmak
istiyorsak onun işler kodlarını olduğu gibi alır, işletirken sistem
çağrıları varsa onları yakalayıp (intercept) o çağrıları gerekli
Termux çağrısına tercüme eder, Termux kütüphaneleri Android ile
iletişimi halleder. Mesela Ubuntu dosyaya yazmak için bir sistem
çağrısı yapabilir, Proot bunu alıp Termux üzerinden dosyaya yazma
komutu haline getirir.

Peki işler kodlar (executable) ne oluyor? Bazı tur emülasyonlar vardır
ki mesela Intel x86 için yazılmış kodları Motorola işlemcisi üzerinde
işletebilir, yani herşey tercüme edilir. Buradaki durum farklı,
emülasyon baştan kendine uyumlu işler kodları indiriyor, yani en alt
seviye makina kodu bazında emülasyona gerek yok. PRoot üzerinden
emülasyon içinde bir program kurduğumuzda hala kendi işlemcimize göre
kodlar alıyoruz, mesela Samsung üzerinde Ubuntu ARM uyumlu programlar
indirilecektir.


Kurmak için Termux üzerinde,

```
pkg install proot-distro
```

Ardından

```
proot-distro install ubuntu
```

Şimdi sisteme girmek için 

```
proot-distro login ubuntu
```

kullanırız. Bu bizi Ubuntu sistemine sokar. Etrafa bakınınca Unix
demirbaşlarını görebiliyoruz, dosya sistemi, `/var`, `/etc`, ya da
süreçler için `/proc`. Ben hemen `useradd` ile bir normal kullanıcı
yarattım, `root` için `passwd` ile bir şifre atadım, böylece gerekli
sistem kurulumlarını `su` sonrası yapıyorum, diğer her iş için `root`
üzerinden normal kullanıcıya geçiş yapıyorum, `su - user1` gibi.

Bir problem, normal kullanıcı için eğer başlangıç ayarları `.bashrc`
içinde tanımladıysak bunun çağrılması normal Ubuntu'daki gibi otomatik
olmuyor, bir `.bash_profile` ekleyip oradan `. .bashrc` ile çağrıyı
bizim kodlamamız gerekiyor. Bu yapıldıktan sonra `su - user1` ile
giriş yapılınca gerekli ayarlar `.bashrc` içinden çağrılır.

Girer girmez hemen bir `upt update` ve `apt upgrade` yapmak faydalı
olur. Artık `apt install` ile admin üzerinden istediğimiz her Ubuntu
programını kurabiliriz.

İlginç bir nokta Termux üzerinde `pkg install` ile kurulmuş
programların Proot içinden görülebilmesi. Mesela `pkg install htop`
kurmuşsam bu programı Ubuntu'da işletebiliyorum. Fakat tersi olmuyor.
Benim tercihim programları emulasyon içinde, yani Ubuntu ise Ubuntu
üzerinde, o sisteme göre kurmak, Termux'ta degil, böylece o programın
diğerleri ile olan etkileşimi daha rahat olur.

X11

[1] bağlantısında görüldüğü gibi tablet üzerinde Linux masaüstü bile
işletmek mümkün. APK alınan yerde bir Termux:X11 apk'si var, bu
kurulup geri kalan ayarlar yapılınca görsel X uygulamaları da
işletilebiliyor.

Kaynaklar

[1] https://youtu.be/UgRds3iP0BU

[2] https://github.com/olegos2/termux-box

[3] https://github.com/termux/proot-distro

[4] [Android Uzerinde Linux - Termux, Samsung J6](../../2018/09/android-uzerinde-linux-termux.html)
