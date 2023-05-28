# Ubuntu, Android 10

Geliştiriciler için bilfiil Android app geliştirmesi için olmasa bile
faydalı olabilecek bazı programlar.

Dizüstünden program kurmak, büyük dosyaları kopyalamak için önce
tablet ya da telefon Android üzerinde System | About Phone | Build
number'a birkaç kere tıklandıktan sonra geliştirici mod'una geçip
Developer Options altında USB Debugging hazır hale getirilir, ve
Ubuntu dizüstünde

```
sudo apt install adb
```

ile Android'e ÜSB kablosu üzerinden erişilebilen bir ortam kurmuş
oluruz. Kablonun bir ucu standart USB diğer ucu mikro USB. Bağlanınca
izin verip vermediğimizi soracak, "allow" seçeriz, ve her şey hazır
olur.

Program kurmak için apk dosyaları lazım, tabii ki güvenilir yerlerden
almak en iyisi, mesela F-Droid [1] bunlardan biri. APK alınınca mesela
app1.apk diyelim, kurmak için

```
adb install app1.apk
```

Dosya göndermek için

```
adb push dosya1.zip /storage/emulated/0/Download/
```

Bu dosya telefon SD kart üzerinde Internal Storage | Download altına
gidecek. Eğer dizüstü ve Android arasında bir dizini senkronize etmek
istiyorsam, bunu ittirme (push) çekme (pull) bazlı yapabilirim. Mesela
ittirme örneği dizüstünde /home/user1/dizin2/work adlı bir dizin
Android'de Download/work dizini ile senkronize edilsin istiyorsam, ki
sadece dizüstünde yeni olan ve değişmiş dosyalar ittirilsin /
gönderilecek şekilde,

```
adb push --sync /home/user1/dizin2/work/ /storage/emulated/0/Download/
```

diyebilirim. Dikkat `work` ibaresi hedef dizinine koyulmadı, bunu
`adb` ekliyor, karışıklık yaratabilir ama sistemi böyle kurmuşlar.

Benzer şekilde `pull` var, bir de her iki işlemi yapan `sync`.

Kaynaklar

[1] <a href="https://f-droid.org/en/packages/com.termux/">F-Droid Termux</a>
