# Ubuntu 14

sudo apt-get install gnome-shell gnome-session-fallback xkbset git ubuntu-restricted-extras compiz compizconfig-settings-manager texlive vlc default-jdk transmission  imagemagick mplayer compiz-plugins emacs auctex gksu dvipng texlive-fonts-extra calibre djvulibre-bin recoll pdftk gimp x11-utils r-base-core

sudo apt-get remove rhythmbox 

Pencere Gecisi

Applications | System Tools | Preferences | CompwizConfig Settings Manager

Soldaki Windows bolumundeni "Static Application Switcher"'i secin,
boylece Alt-Tab ile pencere gecisi yapilabilir. Eger Next window (All
Windows) icin Alt-Tab demiyorsa, tus kombinasyonunu gosteren dugmeye
basin, ve oradan "Grab key combination" dugmesine basin, ve Alt-Tab
tuslarina basin, boylece bu tus kombinasyonu kaydedilmis olur.

Tuslar Ile Mouse Tiklamasi

Bizim Github kod deposunde remap_mouse.sh'e cagri yap, /etc/profile
icinden, boylece bazi tuslar mouse tiklamasi haline gelir.

Caps Control Yapmak

.bashrc icinde

```
setxkbmap -option ctrl:nocaps
```

Bazi tuslari sol ve sag fare tiklamasi yapmak icin

```nd
xmodmap -e "keycode 133 = Pointer_Button1"
xmodmap -e "keycode 108 = Pointer_Button1"
xmodmap -e "keycode 135 = Pointer_Button3"
xkbset m
```

ustteki iki blok kodu bir sh dosyasi icine yazip .bashrc icinden de
cagirabilirsiniz. Eger farkli tuslar kullanmak istenilirse xev ile bu
tuslarin kodlari ogrenilebilir.

Firefox

Zoom Page eklentisi, default zoom 120 yap.

Ses Efektleri

Eger konsol icine mesela gidilemeyecek durumda bile backspace
yapilinca cikan bip, tan, tun seslerini kapatmak istiyorsak System
Tools | System Settings, oradan Sound ve Sound Effects. Bir ses
kontrol ayari var, onun yaninda Mute secilirse artik uyarici sesleri
cikmaz.

Baslangic Davul Sesini Yokedin

```
sudo rm /usr/share/sounds/ubuntu/stereo/system-ready.ogg
```

Balon tavsiyelerini yoketmek icin

```
sudo mv /usr/share/dbus-1/services/org.freedesktop.Notifications.service
 
/usr/share/dbus-1/services/org.freedesktop.Notifications.service.disabled  
```

Uyuma (sleep) ve tekrar geri gelme sonrasi (resume) eger Wifi /
network baglanmiyorsa,

```
sudo touch /etc/pm/sleep.d/wakenet.shsudo chmod +x /etc/pm/sleep.d/wakenet.shsudo gedit -H /etc/pm/sleep.d/wakenet.sh
```

Dosya icine

```
#!/bin/bash

case "$1" in

    thaw|resume)
      nmcli nm sleep false;;*);;
  esac

exit $?
```

Sag ust kosedeki kisayollar

Eger gnome panelden ikon silmek istiyorsak, Alt Super + sag mouse
tiklamasi yapilir ve "Remove from Panel" secilir. Ya da
`$HOME/.config/gnome-panel` altinda ikona tekabul eden dosya silinir.

Hata Mesajlari

Bir sistem hatasi ciktiginda surekli "sorry ubuntu has experienced
internal error ..." gibi bir diyalog kutusu ekrana geliyor. Bunu iptal
etmek icin sudo gedit /etc/default/apport ile ayar dosyasina girin
ve enabled=0 haline getirin, bilgisayari tekrar baslatin.
