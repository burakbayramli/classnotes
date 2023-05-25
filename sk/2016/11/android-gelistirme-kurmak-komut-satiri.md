# Android, Geliştirme, Kurmak, Komut Satırı

Uzun zamandır Android geliştirmesi yapmamıştık, bir görüntü işleme
uygulaması için gerekti.

Ubuntu üzerinde alttakiler kurulur,

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

Şimdi telefona USB'den bağlanınca bazı cici uygulamaları kullanabilmek
için (mesela adb), ve ileride rooting yapabilmek için telefonda
geliştirici seçenekleri (developer option) aktıve etmek gerekli. Bunun
için bazı farklı yollar var(mış) benim telefonda seçenekler (options)
sonra About Device'a gidip "Build number" üzerinde 7 kere tıklama
yapmak gerekti.

Ayrica Settings | Developer options'dan Debugging | USB Debugging aktive edilmeli.

Kod

Altta arkadas baz bir kamera uygulamasi kodlamis, baslangic noktasi olarak iyi olabilir,

https://github.com/ikkiChung/MyCamera

Açtıktan sonra baktık, içinde sadece çok temel kodlar, ayarlar
var. Derlenebilir proje için o dizine gidip alttaki komut işletilir,

ANDROID_SDK/tools/android update project  --target 1 --name MyName  --path .

Bu gerekli tüm script'leri yaratır. Şimdi `ant debug` yapılır, bu bir
apk  yaratır, bu apk telefona kopyalabilir, vs.

Android icindeki algilayicilarin degerlerini gormek icin ornek kod

https://github.com/onyxbits/sensorreadout

![](usb2.jpg)



