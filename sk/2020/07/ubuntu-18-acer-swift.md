# Ubuntu 18, Acer Swift

Acer 400 dollar'dan daha az fiyatlı sağlam makina. 4 cekirdekli, 128
GB SSD (solid state disk). Ubuntu kurmak için USB diskten
yüklenebilir.

Paketten çıkan masaüstü idarecisi (desktop manager) program Unity
kullanışlı... Fakat iş alanı (workspace) ayarı biraz garip, diğer
alanlara program yollanabiliyor, işleyen programlar listesinden diğer
alandaki program seçilince alan değişiyor. Fakat bir alanda iken
Alt-Tab ile geçiş yapınca diğer alana geçiş var. Bunu bilinen hale
çevirmek için

```
gsettings set org.gnome.shell.app-switcher current-workspace-only true
```

Böylece Alt-Tab sadece aynı iş alanındaki programlara geçiş yapar.

Çalışma alanları arasında gidip gelmek için sol üst köşede
`Activities` seçilir, ekranın sağında bir seçim alanı çıkıyor, buradan
alan değişimi olabiliyor.

Tuş üzerinden mouse tıklaması yapmak için bu makinada bazı kodlar
farklı, bizde CAPS => Ctrl, sağ tıklama => Ctrl, sol tıklama => Alt
Gr, ve Windows tuşu olur, bunun için `.bashrc` içinde

```
setxkbmap -option ctrl:nocaps

xmodmap -e "keycode 133 = Pointer_Button1"
xmodmap -e "keycode 108 = Pointer_Button1"
xmodmap -e "keycode 105 = Pointer_Button3"
xkbset m
```

Dikkat: Touchpad üzerinden sağ tıklama için taam alt köşeden basmak
lazım. Pek kullanışlı denemez, tuşlarla daha iyi.

Ubuntu kurulusu ile pek cok yazilim gelir, Transmission gibi. Alttaki
paketler ilk işlettiğimiz `apt-get install` listesi,


```
git chromium-browser emacs xkbset virtualenv texlive-full pygmentize
vlc mplayer xvkbd htop make libportaudio2 portaudio19-dev python3-dev
cmake
```

Emacs ile F tuşlarını kullananlar için, bu tuşlar Acer tarafından
kullanılmakta, ses, ışık ayarı gibi... Eğer iptal etmek ile uğraşmak
istemiyorsanız, Emacs içinde FN tuşu ile beraber F1, F2, vs. basmak
normal F1, F2 etkisi yapar.

Emacs'i her zaman bir Python
[virtualenv](../../2018/08/virtualenv-python-izole-sanal-calsma.md)
ortamından başlatmak iyi fikirdir, böylece komut satırında işletilen
`python` çağrısı ortamda kurulan paketlere erisebilmiş olur. Bizim
mesela bir `emacs.sh` script var, program böyle başlatılıyor, orada
hem `virtualenv` girişi yapılır, ardından Emacs başlatılır,

```
source /home/burak/Documents/env3/bin/activate 
/usr/bin/emacs25 &
```

Bu script icin de bir `alias` yaratabiliriz,

```
alias em="cd $HOME/Documents/kod; bash emacs3.sh"
```

Bizim Emacs ayarlari [surada](https://github.com/burakbayramli/kod/tree/master/site-lisp).


Kaynaklar

[1] [AskUbuntu](https://askubuntu.com/questions/1092758/how-to-make-ubuntu-18-04-gnome-workspace-prohibit-alt-tab-to-windows-on-other)
