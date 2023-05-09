# Klavye Tusunu Mouse Tiklamasi Haline Getirmek

Diyelim ki bazı makinalarda olan "Windows" düğmesine (ya da herhangi
bir düğmeye) başka bir iş yaptırmak istiyoruz, mesela bu tuşa basınca
sol mouse tıklaması olmasını istiyoruz. xmodmap bu tür değişimleri çok
rahat yapar.

Önce tuşun kodu lazım; `xev` programını başlatın, ve tuşa basın,
konsolda kodu gözükecek. Diyelim ki kod 133, o zaman şöyle bir script
yazılır (diğer kodları da öğrendikten sonra)

```
xmodmap -e "keycode 133 = Pointer_Button1"
xmodmap -e "keycode 108 = Pointer_Button1"
xmodmap -e "keycode 135 = Pointer_Button3"
xkbset m
```

Bu script'i kaydedelim, ve Applications | System Tools | Preferences |
Startup Applications'a giderek bilgisayar her başladığında
işletilmesini istediğimizi belirtelim, sh /dizin/ismi/script.sh gibi
mesela, bunu ekleyelim. Artık bilgisayar her başladığında windows tuşu
mouse tıklaması yapabilecek.

Ek bir pürüz, sistem uykudan (suspend) uyandıktan sonra ortaya çıkıyor
- üstteki ayarlar kayboluyor. Bu ayarların uyanma sonrası işlemesi
için, mesela  /etc/pm/sleep.d/99_remap adında bir dosyayı sudo ile
yaratın, üzerinde chmod +x yapın, ve içine

```
#!/bin/sh
case "$1" in
        thaw|resume)
                DISPLAY=:0.0 ; export DISPLAY
                su - [KULLANICI] -c /dizin/ismi/script.sh
                ;;esac
```

komutlarını yazın, [KULLANIÇİ] sizin Ünix kullanıcı isminiz
olacak. Ayrıca script.sh dosyasını u+x ile chmod'lamayı unutmayın.


