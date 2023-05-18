# Tablet Uzerinde Termux

```
adb install com.termux_117.apk
```

Android uzerinde

```
termux-setup-storage
```

`$HOME` altinda bir `storage` dizini olusur. Bu dizin icinde
Android'in bildik `downloads` `dcim` gibi dizinlerine sembolik
baglantilar var.

```
pkg upgrade
```

Arada soru sorar, bunlari ENTER ile geceriz.

Alttaki paketler uzerinde `pkg install`.

```
emacs python3 openssh build-essential python3-numpy
```

Bazen kurulum patlayabilir, birkac denemek iyi olabilir.

Sonra

```
pip3 install virtualenv

virtualenv -p /data/data/com.termux/files/usr/bin/python3 env3
```

Simdi `source env3/bin/activate` ile ortama girilebilir.

### Tus Degisimi

En alttaki Ctrl, ESC gibi tuslarin ekrandan basilmasini saglayan kismi
iptal etmek icin Ses Acmak + q tuslari. Control tuşu Trust Bluetooth
klavyelerinde rahat erişilen yerde değil, Vim, Emacs kullanıcıları bu
tuşu çok kullanır, CAPS tuşunu CTRL yapabiliriz, ek olarak benim
tercihim SPACE yanındaki Command yazan tuşu Left Alt yapmak. Bunun
için Android seviyesinde değişiklik lazım. Şu [1] uygulama ile web
üzerinde isteğe göre üretilen bir .apk bu değişimi yapabiliyor. APK
üretimi arka planda derleme ile üretiliyor muhakkak, bu .apk indirilip
kurulunca (Android uyarılarını dikkate almayız) tuş değişimi olur.

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













https://f-droid.org/en/packages/com.termux/

https://f-droid.org/en/packages/de.baumann.browser/

[exkeymo](https://exkeymo.herokuapp.com/)
