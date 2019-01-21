# Klavye Tusunu Mouse Tiklamasi Haline Getirmek


Klavye Tusunu Mouse Tiklamasi Haline Getirmek




Diyelim ki bazi makinalarda olan "Windows" dugmesine (ya da herhangi bir dugmeye) baska bir is yaptirmak istiyoruz, mesela bu tusa basinca sol mouse tiklamasi olmasini istiyoruz. xmodmap bu tur degisimleri cok rahat yapar.

Once tusun kodu lazim; xev programini baslatin, ve tusa basin,  konsolda kodu gozukecek. Diyelim ki kod 133, o zaman soyle bir script yazilir (diger kodlari da ogrendikten sonra)
xmodmap -e "keycode 133 = Pointer_Button1"xmodmap -e "keycode 108 = Pointer_Button1"xmodmap -e "keycode 135 = Pointer_Button3"xkbset m

Bu script'i kaydedelim, ve Applications | System Tools | Preferences | Startup Applications'a giderek bilgisayar her basladiginda isletilmesini istedigimizi belirtelim, sh /dizin/ismi/script.sh gibi mesela, bunu ekleyelim. Artik bilgisayar her basladiginda windows tusu mouse tiklamasi yapabilecek.

Ek bir puruz, sistem uykudan (suspend) uyandiktan sonra ortaya cikiyor - ustteki ayarlar kayboluyor. Bu ayarlarin uyanma sonrasi islemesi icin, mesela  /etc/pm/sleep.d/99_remap adinda bir dosyayi sudo ile yaratin, uzerinde chmod +x yapin, ve icine 

#!/bin/sh
case "$1" in        thaw|resume)                DISPLAY=:0.0 ; export DISPLAY                 su - [KULLANICI] -c /dizin/ismi/script.sh                ;;esac

komutlarini yazin, [KULLANICI] sizin Unix kullanici isminiz olacak. Ayrica script.sh dosyasini u+x ile chmod'lamayi unutmayin.

Kaynak





