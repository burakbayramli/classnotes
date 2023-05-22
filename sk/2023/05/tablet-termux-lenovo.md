# Lenovo Tablet Üzerinde Termux

Daha önce bir Samsung telefon üzerinde Termux nasıl kurulur yazmıştık
[1]. Şimdi aynı işlemi bir Lenovo tablet için deneyelim. Amacımız
numpy, scipy, emacs ve flask gibi programları, paketleri işletebilmek
olacak.

Google Play Store'daki Termux problem çıkartabilir. En iyisi [2]
adresinden apk dosyasını indirip kurmak. Ya dosyaya tıklanır, ya da
System | About Phone | Build number'a birkaç kere tıklandıktan sonra
geliştirici mod'una geçip Developer Options altında USB Debugging
hazır hale getirmek, bundan sonra Ubuntu dizüstünde

```
sudo apt install adb
```

dersek Android'e USB kablosu üzerinden erisebilen bir ortam kurmuş
oluruz. Artık

```
adb install com.termux_117.apk
```

ile apk kurulumu yapabilir. 

Termux kurulduktan sonra Android üzerinde

```
termux-setup-storage
```

yapmak iyi olur. Bu `$HOME` altında bir `storage` dizini
oluşturur. Dizin içinde Android'in bildik `downloads` `dcim` gibi
dizinlerine sembolik bağlantılar var.

Artık dizüstünden direk USB kablosu ile dosya gönderebiliriz, mesela

```
adb push filanca.tar.gz /storage/emulated/0/Download/
```

Şimdi tablet Termux üzerindeki işlemlere gelelim.

```
pkg upgrade
```

yapmak iyidir, herşey güncellenir. Arada soru sorar, bunları ENTER ile
geçeriz.

Alttaki paketler üzerinde `pkg install`.

```
python3 openssh build-essential python-numpy emacs libxml2 libxslt cmake freetype binutils
```

Bazen kurulum patlayabilir, birkaç deneme iyi olabilir.

Üsttekiler tamamsa `matplotlib` üzerinde `pkg install` denenmeli. Eğer yardımcı
paketlerde problem çıkarsa bunları ayrı ayrı başına 

```
LDFLAGS="-L/system/lib64" CFLAGS="-I/data/data/com.termux/files/usr/include" pip install
```

ekleyerek `pip` ile kurmayı deneyebiliriz.

Dikkat: Kurulum tüm sistem bazında yapılıyor, hala bir izole [4] ortam yaratmadık.
Buradaki sebep ``python-numpy` kurulumunun sistem bazlı olması, diğer baz paketler
de onu izlerse sistem bazlı işler daha rahatlaşıyor.

Bir kez temel paketler kurulunca, artık izole ortamlar mevcut olan
paketler için sistem bazlı olanı kullanabilir, ek yapılan `pip
ınstall` kurulumları hala izole ortamda kalabilir. Şimdi,

```
pip3 install virtualenv
```

Ve `env3` adlı ilk ortamımızı yaratalım,

```
virtualenv --system-site-packages -p /data/data/com.termux/files/usr/bin/python3 env3
```

Artık `source env3/bin/activate` ile ortama girilebilir.

Ek kurulumlar ortam icinde `pip` ile,

```
pip install Pillow bs4 flask folium geopy ipython 
```

### Tuş Değişimi

En alttaki Ctrl, ESC gibi tuşların ekrandan basılmasını sağlayan kısmı
iptal etmek için Ses Açmak + q tuşları. Control tuşu Trust Bluetooth
klavyelerinde rahat erişilen yerde değil, Vim, Emacs kullanıcıları bu
tuşu çok kullanır, ÇAPS tuşunu CTRL yapabiliriz, ek olarak benim
tercihim SPACE yanındaki Command yazan tuşu Left Alt yapmak. Bunun
için Android seviyesinde değişiklik lazım. Şu [3] uygulama ile web
üzerinde isteğe göre üretilen bir .apk bu değişimi yapabiliyor. APK
üretimi arka planda derleme ile üretiliyor muhakkak, bu .apk indirilip
kurulunca (Android uyarılarını dikkate almayız) tus değişimi olur.

Web sitede tanımları liste bazlı seçebiliriz, bahsettiğim Command (ki
sisteme Meta Left olarak gözüküyor) Alt Left bağlantısı yapmak alttaki
şekilde.

![](exkeymo.jpg)

Bu tanım daha sonra yaratılacak .apk içine koyulacak, Download tuşuna
tıklayınca apk alınır. Uygulamaya güvenmeyenler aynı sayfadaki
bağlantıdan kaynak koduna gidip Android kodunu derleyebilir.

Apk kurulduktan sonra tabii klavye seçiminin yeni programı görmesi
lazım; Android'de System | Languages & İnput | Physical keyboard (mesela bir
bluetooth klavye) seçtikten sonra Physical Keyboard altında bir layout seçimi
var, buraya girip listeden "ExKeyMo Layout" seçmek lazım.

Nihayet Termux ekranından `emacs -nw` ile favori editörümüzü başlatıyoruz,

![](emacs-termux.jpg)

Kaynaklar

[1] <a href="../../2018/09/android-uzerinde-linux-termux.html">Android Uzerinde Linux - Termux, Samsun J6</a>

[2] <a href="https://f-droid.org/en/packages/com.termux/">F-Droid Termux</a>

[3] <a href="https://exkeymo.herokuapp.com/">exkeymo</a>

[4] <a href="../../2018/08/virtualenv-python-izole-sanal-calsma.html">virtualenv, Python İzole, Sanal Çalışma Alanı (Python Virtual Environment)</a>

