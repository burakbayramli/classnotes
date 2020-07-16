# Ubuntu 18, Acer Swift

Acer 400 dollar'dan daha az fiyatlı sağlam makina. Ubuntu kurmak için
USB diskten yüklenebilir Ubuntu problem çıkarmadı.

Paketten çıkan masaüstü program Ünity kullanışlı... Fakat iş alanı
(workspace) ayarı biraz garip, diğer alanlara program yollanabiliyor,
işleyen programlar listesinden diğer alandaki program seçilince alan
değişiyor. Fakat bir alanda iken Alt-Tab ile geçiş yapınca diğer alana
geçiş var. Bunu bilinen hale çevirmek için

```
gsettings set org.gnome.shell.app-switcher current-workspace-only true
```

Tus üzerinden mouse tıklaması yapmak için bu makinada bazı kodlar
farklı, bizde ÇAPS => Ctrl, sağ tıklama => Ctrl, sol tıklama => Alt
Gr, ve Windows tuşu olur, bunun için `.bashrc` içinde

```
setxkbmap -option ctrl:nocaps

xmodmap -e "keycode 133 = Pointer_Button1"
xmodmap -e "keycode 108 = Pointer_Button1"
xmodmap -e "keycode 105 = Pointer_Button3"
xkbset m
```

Dikkat: Touchpad uzerinden sag tiklama icin taam alt koseden basmak
lazim. Pek kullanisli denemez, tuslarla daha iyi.









[1] https://askubuntu.com/questions/1092758/how-to-make-ubuntu-18-04-gnome-workspace-prohibit-alt-tab-to-windows-on-other

