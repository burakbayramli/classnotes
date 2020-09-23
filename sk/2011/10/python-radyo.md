# Python Radyo

Internet'ten bedava radyo dinleyebilmek icin bazi sayfalar var, fakat
hepsine HTML bazli girip, belli yerlere tiklamak, beklemek, vs. gibi
isler cok ugrastirici olabiliyor. Python bazli bir program yazilmis,
kodlar acik, programi komut satirindan baslatiyorsunuz, bir suru kanal
icinden istediginiz seciyorsunuz, alet arka planda o kanaldaki muzigi
calan HTML sayfalarina baglanip oradan gelen muzigi canli olarak
aktariyor (streaming).

Cep ortaminda (Termux ile) bu radyo ile calan muzik baska bir program
ses verdiginde de calismaya devam ediyor, yani radyo programi arka
plan muzigi olarak bir dersi seyrederken kullanilabilir.

Kodlar

https://github.com/coderholic/pyradio

Bu program arka planda islemek icin `mplayer`, `mpv` gibi kodlar
gerektiriyor.

Termux icin not: `mpv` Termux'ta var. Radyonun ses acip kapamasi
islemezse, `mpv` nin olagan ses ayarlarina sistem seviyesinden girebilirsiniz.
Mesela, `$HOME/.config/mpv/mpv.conf` icinde

```
volume=50
```

tanimlarsak sesi yarisina indirmis oluruz.

Eski (alttakiler eski usul)

Kodlar surada:

https://github.com/jspricke/radio

Bazi puf noktalar: Radyoyu baslatabilmek icin komut satiri tam ekran
(fullscreen) olmali, biz o isi otomatik yapmak icin Ubuntu'da bir ikon
yarattik, ve ikon gerekli xterm penceresini gerektigi sekilde
baslatiyor.

```
xterm -fn *-fixed-*-*-*-20-* -maximize -e "python [DIZIN]/jspricke-radio-6956368/radio.py"
```

Ayrica, birkac kez kanal degistirince program takilabiliyor, ama bu
pek onemli degil, 24/7 islemesi gereken bir program degil zaten,
kapatip acinca buyuk bir ihtimalle tekrar islemeye baslayacaktir.




