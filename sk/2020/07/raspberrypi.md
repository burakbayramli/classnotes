# Raspberry Pi

Elektronik aletleri kontrol etmek, medya idare merkezi (şarkı, film
göstermek) , hatta ufak masaüstü bilgisayarı olarak bile
kullanılabilecek bir ufak ama kuvvetli bir bilgisayar; Raspberry
Pi. Pi'nin paketten çıktığı haliyle her türlü giriş birimi (klavye,
fare) ya da çıkış (monitör, Wifi, Bluetooth) için desteği
vardır. İşletim sistemi basit bir SD karttan hızla yüklenebilir, Linux
bazlıdır, ve masaüstü şekilde hazırdır,

<img width="300" src="pi.png"/>

RPi kurmak için Mikro SD kart gerekli, isletim sistemi bu karttan
okunuyor. Eğer satın alınan paketten bu şekilde RPi işletim sistemi
olan bir SD çıkmadıysa (bazı paketlere dahil ediliyor) dizüstü
bilgisayardan kurmanız lazım. Ayrı pürüz benim bilgisayarda Mikro SD
değil SD okuyucu olmasıydı (daha büyük bir kart), o sebepe kart
alırken SD / MicroSD çeviricisi olanı aldık,

<img width="200" src="sd.jpg"/>

Dizüstündeki Ubuntu'ya bunu taktık. Ubuntu'da `Disks` programı ile
takılan disk bulunur ve FAT için formatlanır. Şimdi RPi işletim
sistemi imajını SD'ye "yakma" zamanı geldi. [2]'den "Raspberry Pi
İmager for Ubuntu" indirilir, bir deb dosyası,

```
sudo apt-get install qml-module-qt-labs-settings
```

problem verirse,

```
sudo apt --fix-broken install
```

düzeltir. Ardından alınan deb üzerinde,

```
sudo dpkg -i imager_1.4_amd64.deb
```

Şimdi

```
rpi-imager
```

ile program başlatılır. Çıkan listede "İşletim Sistemi Seç (Choose
OS)" için ilk görülen Raspberry Pi seçilebilir. "SD Kart Seç (Choose
SD Card)" için daha önce formatlamış olduğumuz kart seçilir. Tamam
dedikten sonra bu işlem 1 GB üzerinde bir indirme yapacak, yani hızlı
ve yeterince kapasitesi olan bir Internet bağlantımız olması iyi olur.

İlk Sistem Başlatımı

RPi donanımı olağan haliyle HDMİ ekranlara bağlanabilir, ve işletim
sistemi USB'ye takılan klavye, fare gibi araçları tanır. Fakat
çoğumuzda bu tür giriş araçları olmayabilir, mesela kablolu klavye
olmayabilir, ve başta, RPi ilk kez yüklenirken, başlarken bazı
girişlerin olması lazım. Ne yapacağız?

Burada çözüm en minimal kablolu gereci almak, bir fare. Başlarken
kablolu fare USB'den takılır, RPi bunu hemen tanır, ekrana zaten HDMI
ile hemen gösterim olur, sistem kendini kurarken tıklama ile Sonra
(Next) düğmeleriyle ilerleyebiliriz, ve en son noktaya gelindiğinde
Bluetooth açılir ve mesela Bluetooth klavyesi tanıtılır. Artık
klavyeden giriş yapıp daha çetrefil işleri yapabiliriz. Bundan
sonrası, daha da iyi, SSH ile erişim.  Eğer sisteme girebildiysek ve
ekranda Pi gözüküyorsa, güzel. Şimdi SSH ile başka bilgisayardan
bağlanmaya gelelim, böylece ekrana ihtiyaç olmayacak, basit bir Wifi
bağlantısı ile Pi'ye komut satırında girebileceğiz.

Çoğumuzda Android üzerinden cep telefon Internet bağlantısını
paylaşma, hotspot özelliği var. Bağlantıyı paylaşalım, ve Pi'mizi
bağlayalım. İnternet'e bağlanabildiğimizi kontrol edelim, tarayıcı ile
herhangi bir site. Şimdi Pi'nin şifresini set edelim, menüden `Pi |
Preferences | RPi Configuration | System` ile. Ardından `Pi |
Preferences | RPi Configuration | İnterfaces` üzerinde SSH seçeneğini
`Enable` haline getirelim [3]. Şimdi komut satırına gidelim, ve
`ifçonfig` diyelim. Bu bize network donanımlarının bağlı olduğu
adresleri gösterir. Aradığımız IP adresi çoğunlukla `192.168..` diye
giden bir adres. Simdi ikinci (ve ayni Wifi noktasina bagli) dizustu
bilgisayara gidelim ve

```
ssh pi@192.168.... 
```

diyelim. Giriş yapılmış olmalı. Eğer işimizi ilerisi için daha da
kolaylaştırmak istersek, [şifresiz giriş](../../2005/10/bir-makinaya-ssh-ile-sifresiz-giris.md)
ayarlarını yapabiliriz. Bu yöntem işliyor çünkü aynı hotspot'a
bağlandıysak aynı network'un içindeyiz demektir ve aynı network içinde
makinalar birbirlerini bulabilirler.



Kaynaklar

[1] https://www.raspberrypi.org/documentation/installation/noobs.md

[2] https://www.raspberrypi.org/downloads/

[3] https://www.raspberrypi.org/documentation/remote-access/ssh/

