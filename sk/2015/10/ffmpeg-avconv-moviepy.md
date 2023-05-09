# ffmpeg, moviepy

Video birlestirmek icin en rahat arac Python `moviepy` paketi ile,

```
from moviepy.editor import VideoFileClip, concatenate_videoclips
clip1 = VideoFileClip("v1.mp4").resize(0.50)
clip2 = VideoFileClip("v2.mp4").resize(0.50)
final_clip = concatenate_videoclips([clip1,clip2])
final_clip.write_videofile("out.mp4")
```

Bazi ffmpeg, komutlari

Bir video dosyasinin belli zaman araligindaki parcasini cikartmak icin
(baslangictan 4 saniyelik kisim)

```
ffmpeg -ss 00:00:00 -i girdi.mp4 -t 00:00:04 -c copy cikti.mp4
```

Bir video'nun belli bir parcasini nasil animasyon gif dosyasi haline
ceviririz? Bir frames alt dizini yaratalim, sonra 20. saniyeden
baslayip 10 saniyelik kismi cikartalim. Once sadece goruntu dosyalari
alacagiz,

```
ffmpeg -ss 00:00:20 -t 00:00:10 -i [video dosyasi]  -vf \
   scale=320:-1:flags=lanczos,fps=10  frames/ffout-%03d.png
```

Simdi bu dosyalari ImageMagick ile birlestirip animasyonu yaratalim

```
convert -loop 0 frames/ffout*.png output.gif
```

Eger direk gif yaratmak istersek,

```
ffmpeg -ss 61.0 -t 2.5 -i vid/dosya.mp4 -f gif dosya.gif
```

Her saniyede kaç kare (frame rate) olduğunu kontrol etmek için

```
ffmpeg  -ss 10.0 -t 5.0 -i girdi.mp4 -vf "fps=10,scale=320:-1" -f gif cikti.gif
```

10. saniyeden başlayıp 5 saniyelik bir videoyu saniyede 10 kare
üzerinden ve 320 piksel genişliğinde animasyonlu GIF yapmış olduk.

Ya da

```
ffmpeg -y  -filter_complex 'fps=10,scale=460:-1:flags=lanczos' -i girdi.mp4 -f gif cikti.gif
```

GİF içinde yazı yazmak istersek,

```
ffmpeg -ss 30 -t 3 -i [DOSYA] -filter_complex "fps=10,scale=720:-1:flags=lanczos,drawtext=enable='between(t,0,1)':fontfile=font3.ttf:text='word':fontsize=24:fontcolor=white:x=(w-tw)/2:y=(h/PHI)+th,drawtext=enable='between(t,1,2)':fontfile=font3.ttf:text='word what':fontsize=24:fontcolor=white:x=(w-tw)/2:y=(h/PHI)+th,split[x1][x2];[x1]palettegen[p];[x2][p]paletteuse" output.gif
```

Format degisimi, kucultmek, mumkun oldugu kadar kaliteyi istenen
sekilde tutmak, 440 yuksekliginde, genislik izafi, kalite 23 (en
kaliteli 0)

```
ffmpeg -i input.mkv -vf scale=-1:440 -crf 23  output.avi
```

Format degisimi (ornekte mp4'den avi), video'nun bir kismini cekip cikartmak, mesela 100. saniyeden baslayarak 20 saniyelik kisim,

```
ffmpeg -i vid1.mp4 -ss 100 -t 20 -acodec copy -vcodec copy vid2.avi
```

Üstteki işlemi daha hızlı yapmak için mp4 formatına değişim ve alttaki ekler gerekebilir

```
 ... -b:v 2500 -c:v mpeg4 ....
```

Video'yu numaralı JPG imaj dosyaları haline getirmek

```
ffmpeg -i vid.avi -vcodec mjpeg %05d.jpg
```

Diğer bazlı ffmpeg komutları.. Mesela bir görüntüyü  çevirmek için

```
ffmpeg.exe -i dosya.mp4 -vf "transpose=2" sonuc.mp4
```

transpose parametresine verilen değerlerin anlamı var, 1 saat yönü, 2 saat yönü tersi.

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
ffmpeg -i "concat:dosya1.ts|dosya2.ts|..."  -c copy sonuc.mp4
```

Eger Windows'da bir mp4 cikmiyorsa, bir cevrim sekli de soyle,

```
ffmpeg -i dosya.mp4 -r 30  -codec:v mpeg4 -flags:v +qscale \
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

Video'dan Ses Cikartmak

```
ffmpeg -y -ss 00:02:10 -i vid.mp4 -t 00:00:04 -q:a 0 -map a out.mp3
```


Kaynaklar

https://engineering.giphy.com/how-to-make-gifs-with-ffmpeg/

