# ffmpeg, avconv, moviepy

Video birlestirmek icin en rahat arac Python `moviepy` paketi ile,

```
from moviepy.editor import VideoFileClip, concatenate_videoclips
clip1 = VideoFileClip("v1.mp4").resize(0.50)
clip2 = VideoFileClip("v2.mp4").resize(0.50)
final_clip = concatenate_videoclips([clip1,clip2])
final_clip.write_videofile("out.mp4")
```

Bazi ffmpeg, avconv komutlari

Bir video dosyasinin belli zaman araligindaki parcasini cikartmak icin

```
avconv -i girdi.mkv -ss [saniye] -t [saniye suresi] -codec copy cikti.mkv
```

Bir video'nun belli bir parcasini nasil animasyon gif dosyasi haline
ceviririz? Bir frames alt dizini yaratalim, sonra 20. saniyeden
baslayip 10 saniyelik kismi cikartalim. Once sadece goruntu dosyalari
alacagiz,

```
ffmpeg -ss 00:00:20 -t 00:00:10 -i [video dosyasi]  -vf \
   scale=320:-1:flags=lanczos,fps=10  frames/ffout-%03d.png
```

Simdi bu dosyalari ImageMagick ile birlestirip animasyonu yaratalim

```
convert -loop 0 frames/ffout*.png output.gif
```

Format degisimi, kucultmek, mumkun oldugu kadar kaliteyi istenen sekilde tutmak, 440 yuksekliginde, genislik izafi, kalite 23 (en kaliteli 0)

```
ffmpeg -i input.mkv -vf scale=-1:440 -crf 23  output.avi
```

Format degisimi (ornekte mp4'den avi), video'nun bir kismini cekip cikartmak, mesela 100. saniyeden baslayarak 20 saniyelik kisim,

```
ffmpeg -i vid1.mp4 -ss 100 -t 20 -acodec copy -vcodec copy vid2.avi
```

Ustteki islemi daha hizli yapmak icin mp4 formatina degisim ve alttaki ekler gerekebilir

```
 ... -b:v 2500 -c:v mpeg4 ....
```

Video'yu numarali JPG imaj dosyalari haline getirmek

```
ffmpeg -i vid.avi -vcodec mjpeg %05d.jpg
```

Ses ve goruntu dosyalarini islemek icin ffmpeg kullananlar icin Ubuntu 14'da surpriz: ffmpeg programi apt-get ile kurulamiyor. Dolambacli yollardan kurmak mumkun fakat yeni one surulen program avconv. Kurmak icin

```
sudo apt-get install libav-tools
```

Bir ses dosyasinin belli zaman araliklarindaki kismini cikartmak icin

```
avconv -i dosya.mp3 -ss 0:0:6 -t 0:0:8 -acodec copy parca.mp3
```

Bu komut dosyanin 6. saniyesinden baslayakara 8 saniyelik bir kismi cikartiyor. 

Diger bazli ffmpeg komutlari.. Mesela bir gorutuyu  cevirmek icin

```
ffmpeg.exe -i dosya.mp4 -vf "transpose=2" sonuc.mp4
```

transpose parametresine verilen degerlerin anlami var, 1 saat yonu, 2 saat yonu tersi.

Video ufaltmak icin

```
ffmpeg -i dosya.mp4 -vf scale=320:240 sonuc.mp4
```

Ya da

```
ffmpeg -i dosya.mp4 -filter:v scale=640:-1 -c:a copy sonuc.mp4
```

Genisligi 640 yap, geri kalan her seyi ona gore ayarla dedik.

Video birlestirmek icin bir ara formattan gecmek lazim.

```
ffmpeg -i dosya1.mp4 -q:v 1 -filter:v scale=500:-1 -f mpegts dosya1.ts
ffmpeg -i dosya2.mp4 -q:v 1 -filter:v scale=500:-1 -f mpegts dosya2.ts
..
```


Sonra birlestirmek icin

```
ffmpeg -i "concat:dosya1.ts|dosya2.ts|..."  -c copy sonuc.mp4
```

Eger Windows'da bir mp4 cikmiyorsa, bir cevrim sekli de soyle,

```
ffmpeg -i dosya.mp4 -r 30  -codec:v mpeg4 -flags:v +qscale \
  -global_quality:v 0 -codec:a libmp3lame dosya.avi 
```

mencoder

Bir AVI video'nun parcasini kesip cikartmak icin

```
mencoder -ss 00:09:00 -endpos 00:02:00 -oac copy -ovc copy kaynak.avi
-o parca.avi
```

Ustteki komut kaynak.avi dosyasinin 9'uncu dakikasindan 2 dakikalik
parca cikartip parca.avi dosyasina kaydediyor. Dikkat: Ustteki
seceneklerde "endpos" ibaresinin tercumesi "bitis noktasi" olmasina
ragmen komut "reklam edildigi gibi" islemiyor.



