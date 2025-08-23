# Samsung Android Tablet Üzerinde Linux Ubuntu

Android üzerinde tam tekmilli Linux işletmek mümkün. Android alt
seviyeleri bilindiği gibi çekirdek seviyesinde Linux kullanır, fakat
işletim sistemi seviyesinde sistem çağrıları dışarıya kapatılmıştır.
Termux uygulaması hem bu çağrılara erişip onları dışa bağlar, aynı
zamanda kullanıcıyı Linux sistemlerinden tanıdık bir komut satırı
programı sunar. Bu program üzerine pek çok bilinen Ünix komutu `pkg
install` ile kurulabilir. Bu konuyu daha önce işletmiştik [4].

Bu yazıyı paylaştığımızdan bu yana Termux bazı iniş çıkışlar yaşadı,
Google Play Store'daki versiyon problem çıkartıyordu, kurulumlar tam
yapılamıyordu, daha sonra bir Android versiyonunda Termux gibi
uygulamaların bir süre sonra süreçlerinin (process) isletim sistemi
tarafından zorla durdurulduğu söylendi. Yeni versiyonlarda bu
problemler düzeltilmiş, Termux'u artık Github üzerinden kuruyoruz, ve
süreç durdurması için ise bir Android 14 üzerinde yeni bir seçenek
var, bunu kullanıyoruz.

Yazımız temel olarak Samsung Galaxy Tab A9 tabletini merkez
alacak. Şuradaki arkadaş [2] A10 tablet kullanmış. Bu donanımlar hızlı
işler, işletim sistem versiyonlari Android 14, gerekli seçeneğe
sahip. Donanımı açınca önce `Settings` | `About Phone` seçip oradan
`Build Number` diyen yere gidiyoruz, ve yedi kere ardı ardına bu yazı
üstüne basıyoruz. Bitince bu işlem ile Android'in geliştirici seviyesi
(developer mode) aktif edilmiş oluyor. Şimdi `Developer Options`
görünür halde olmalı, buna giderek `Disable Subprocess Restrictions`
seçeneğini seçiyoruz.

Termux kurulumu için [2] bağlantısından Termux apk'si indirilir. Bu
indirilen apk üzerinde seçim yapıp onu kurarız, uyarıları iptal edip
devam ederiz, işlemi tamamlarız. Artık bir Termux ikonu program
listesinde gözüküyor olmalı. İkona tıklayıp komut satırına gireriz,
burada `pkg ınstall` ile istenen programları kurmak artık mümkün.

Proot

Fakat hala elimizdeki tam tekmilli bir Ünix değil. Unix'te kullanılan
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

kullanırız. Bu bizi Ubuntu sistemine sokar. Etrafa bakınınca Ünix
demirbaşlarını görebiliyoruz, dosya sistemi, `/var`, '/etc', ya da
süreçler için `/proc`. Ben hemen `useradd` ile bir normal kullanıcı
yarattım, `root` için `passwd` ile bir şifre atadım, böylece gerekli
sistem kurulumlarını `su - root` sonrası yapıyorum, diğer her iş için
`root` üzerinden normal kullanıcıya geçiş yapıyorum, `su - user1`
gibi.

Girer girmez hemen bir `upt update` ve `apt upgrade` yapmak faydalı olur.

Artık `apt install` ile istediğimiz her Ubuntu programını kurabiliriz.

İlginç bir nokta Termux üzerinde `pkg ınstall` ile kurulmuş
programların Proot içinden görülebilmesi. Mesela `pkg ınstall htop`
kurmuşsam bu programı Übüntü'da işletebiliyorum. Fakat tersi olmuyor.
Benim tercihim programları emülasyon içinde o sisteme göre kurmak,
Termux'ta degil, böylece o programin diğerleri ile olan iletişimi daha
rahat olabilir.

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

