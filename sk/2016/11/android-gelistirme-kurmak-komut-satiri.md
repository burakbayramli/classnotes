# Android, Gelistirme, Kurmak, Komut Satiri

Uzun zamandir Android gelistirmesi yapmamistik, bir goruntu isleme
uygulamasi icin gerekti.

Ubuntu uzerinde alttakiler kurulur,

```
sudo apt-get install -y build-essential  zlib1g-dev  libncurses5:i386 libstdc++6:i386 zlib1g:i386 default-jre default-jdk ant
```

Android SDK ve NDK (belki gerekebilir)

https://dl.google.com/android/android-sdk_r24.4.1-linux.tgz

https://dl.google.com/android/repository/android-ndk-r13b-linux-x86_64.zip

Kurduktan sonra ANDROID_SDK/tools/android ile bir GUI baslatilir,
buradan telefonumuz icin gerekli derleme hedefi ve diger bazi ekler
kurulur. Mesela Android 4.2.2 var ise o kurulur, derleme hedefi 17
aktif hale gelir. Bu hedef altta uretilecek build.xml icin gerekli.

Simdi telefona USB'den baglaninca bazi cici uygulamalari kullanabilmek
icin (mesela adb), ve ileride rooting yapabilmek icin telefonda
gelistirici secenekleri (developer option) aktive etmek gerekli. Bunun
icin bazi farkli yollar var(mis) benim telefonda secenekler (options)
sonra About Device'a gidip "Build number" uzerinde 7 kere tiklama
yapmak gerekti.

Ayrica Settings | Developer options'dan Debugging | USB Debugging aktive edilmeli.

Kod

Altta arkadas baz bir kamera uygulamasi kodlamis, baslangic noktasi olarak iyi olabilir,

https://github.com/ikkiChung/MyCamera

Actiktan sonra baktik, icinde sadece cok temel kodlar, ayarlar
var. Derlenebilir proje icin o dizine gidip alttaki komut isletilir,

ANDROID_SDK/tools/android update project  --target 1 --name MyName  --path .

Bu gerekli tum script'leri yaratir. Simdi ant debug yapilir, bu bir
apk  yaratir, bu apk telefona kopyalabilir, vs.

Android icindeki algilayicilarin degerlerini gormek icin ornek kod

https://github.com/onyxbits/sensorreadout

![](usb2.jpg)
