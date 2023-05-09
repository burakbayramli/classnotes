# Ubuntu 16 ve Acer Aspire R11

Bu bilgisayarda ekran 360 derece donup tablet haline
gelebiliyor. Fiyat 300 Eur.

Ubuntu kurmak icin

http://releases.ubuntu.com/16.04/

adresinden 64-bit iso ya da iso icin torrent indirilir, tum iso
alinir. Kurmak icin eger Windows uzerindeyseniz, iso'yu USB flash
disk'e "yakmak" icin

http://www.pendrivelinux.com/universal-usb-installer-easy-as-1-2-3/

Bios'a bilgisayar baslarken F2'yi basili tutarak girebiliriz,
girdikten sonra baslangic seklini "Legacy Mode" haline getirmek lazim,
ve USB diski yukleme sirasinda en uste getirmek lazim. F10 ile
kaydedilir, tekrar baslatilir ve Ubuntu kurulur. Eger kurulus
sirasinda Internet baglantisi yok idiyse, yerel kurulus bitip
Ubuntu'yu ilk girip Internet baglanti sonrasi sudo apt-get update
yapmak lazim.

Not: Alttaki Cinnamon kurulumuna bir ek: Ubuntu 16 hala yamalar almaya
devam ediyor ve en son indirdigimiz guncelleme Cinnamon ile
uyumsuz. Yani eger Cinnamon kullanmak isteniyorsa, belki daha stabil
bir Ubuntu, mesela 14, tercih edilebilir. Yoksa Ubuntu'nun login
ekranindan ayak izi ikonuna tiklanarak pencere idarecisi secim
sirasinda Gnome Flashback mod'u secilirse Windows'a benzeyen bir ortam
hala var.

Masaustu: Biz tabii ki normal Ubuntu kullanis goruntusunu tercih
etmiyoruz, Windows benzer masaustu icin Cinnamon diye bir tema var,

http://www.omgubuntu.co.uk/2016/04/how-to-install-cinnamon-3-0-on-ubuntu

sudo add-apt-repository ppa:embrosyn/cinnamon
sudo apt-get update && sudo apt-get install cinnamon

Sistemden cikip login ekranina donunce Ubuntu sembolu uzerine tiklayin, Cinnamon secenegini goreceksiniz. Artik hersey Windows'a benziyor.

Sol altta Menu | Preferences | Mouse Touchpad'i baslatin, Touchpad'den "Tap to Click"i OFF haline getirilebilir. Ben daha cok klavye bazli is yaptigim ve hizli kullanma amacli oldugum icin pad'in tiklama amacli olmasi karisiklik yaratiyor.

Emacs'de Alt-Space'i cok kullaniyoruz fakat Gnome bu kombinasyonu
"kapmis". Iptal etmek icin Menu | Preferences | Keyboard | Shortcuts
ve Windows | Activate Windows Menu icin Alt-Space iptal edilir.

Ilk kurulacak programlar

```
sudo apt-get install xkbset git ubuntu-restricted-extras transmission 
imagemagick mplayer emacs gksu dvipng calibre djvulibre-bin pdftk gimp
python-pip ffmpeg python-tk recoll texlive texlive-fonts-extra
```

Caps Control Yapmak

.bashrc icinde

setxkbmap -option ctrl:nocaps

Bazi tuslari sol ve sag fare tiklamasi yapmak icin

```
xmodmap -e "keycode 133 = Pointer_Button1"xmodmap -e "keycode 108 = Pointer_Button1"xmodmap -e "keycode 135 = Pointer_Button3"xkbset m
```

Python

En rahati Anaconda kurmak

https://www.continuum.io/downloads#linux

pip install ya da conda install ile

requests ipython numpy scipy matplotlib grip

USB baglayinca masaustu arka resminin degismesi gibi garip bir hata var. Tamir icin

xdg-mime default nemo.desktop inode/directory application/x-gnome-saved-search

Masaustu'nde Dizin Kisayollari

Eger Gnome Flashback ile calismaya mecbur kalindiysa, dosya gosterme
sistemi olagan durumda nemo olmayabilir. Fakat nemo guzel program,
masaustu kisayollarindan nemo ile dizin gostermek icin masaustunde
mesela `$HOME/Desktop/Downloads.desktop` adli bir dosya yaratilir, ve
icine sunlar konur,

```
[Desktop Entry]
Comment=
Terminal=true
Name=Downloads
Exec=nemo /home/burak/Documents
Type=Application
Icon=/usr/share/icons/Win7-icons/filesystems/folder.png
Terminal=false
```

Bu dosya `$HOME/Desktop` altina yazilir yazilmaz bir ikonu masaustu
uzerinde goreceksiniz. Onun uzerine sag tiklama yapin, Properties |
Permissions uzerinden "Allow executing as program"'i secin. Bu kadar.

Alt Serit Uzerinde Kisayollar

Alt serit (Taskbar) uzerine masaustunden birakilan herhangi bir
program, kisayol alt seritte gosterilir. Alt seritten program
cikartmak icin program uzerine gelip Alt + Fare Sol Tiklama, ve Remove
secilir. 

