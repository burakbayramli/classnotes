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
listesinde gözüküyor olmalı. 











[devam edecek]

Kaynaklar

[1] https://youtu.be/UgRds3iP0BU

[2] https://github.com/olegos2/termux-box

[3] https://github.com/termux/proot-distro

[4] [Android Uzerinde Linux - Termux, Samsung J6](../../2018/09/android-uzerinde-linux-termux.html)

