# Ubuntu'da CAPS Tusunu Control Yapmak

Kabul edelim: Caps Lock tusu fazla bir halta yaramiyor. Bu tusu eger
daha cok ise yarayan Control tusu ile degistirmek istiyorsak, alttaki
tarif ise yarayabilir.Once /etc/X11/xorg.conf dosyasina girelim. Bu
dosyada degistirmemiz (yoksa eklememiz) gereken ibare soyle:

```
Section "InputDevice"
Identifier "Generic Keyboard"
Driver "kbd"
Option "CoreKeyboard"
Option "XkbRules" "xorg"
Option "XkbModel" "pc105"
Option "XkbLayout" "us"
Option "XkbOptions" "lv3:ralt_switch, ctrl:nocaps"
EndSection
```

Bilgisayari tekrar baslattiktan sonra CAPS tusunun artik Control
olarak isledigini goreceksiniz.





