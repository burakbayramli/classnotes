# ffmpeg, moviepy

### Bazı ffmpeg, komutları

Bir video dosyasinin belli zaman araligindaki parcasini cikartmak icin
(baslangictan 4 saniyelik kisim)

```
ffmpeg -ss 00:00:00 -i girdi.mp4 -t 00:00:04 -c copy cikti.mp4
```

Bir video'nun belli bir parçasını nasıl animasyon gif dosyası haline
çeviririz? Bir frames alt dizini yaratalım, sonra 20. saniyeden
başlayıp 10 saniyelik kısmı çıkartalım. Önce sadece görüntü dosyaları
alacağız,

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

GIF içinde yazı yazmak istersek,

```
ffmpeg -ss 30 -t 3 -i [DOSYA] -filter_complex "fps=10,scale=720:-1:flags=lanczos,drawtext=enable='between(t,0,1)':fontfile=font3.ttf:text='word':fontsize=24:fontcolor=white:x=(w-tw)/2:y=(h/PHI)+th,drawtext=enable='between(t,1,2)':fontfile=font3.ttf:text='word what':fontsize=24:fontcolor=white:x=(w-tw)/2:y=(h/PHI)+th,split[x1][x2];[x1]palettegen[p];[x2][p]paletteuse" output.gif
```

Format değişimi, küçültmek, mümkün olduğu kadar kaliteyi istenen
şekilde tutmak, 440 yüksekliğinde, genişlik izafi, kalite 23 (en
kaliteli 0)

```
ffmpeg -i input.mkv -vf scale=-1:440 -crf 23  output.avi
```

Format degisimi (ornekte mp4'den avi), video'nun bir kismini cekip
cikartmak, mesela 100. saniyeden baslayarak 20 saniyelik kisim,

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

Ya da en rahat arac Python `moviepy` paketi ile,

```
from moviepy.editor import VideoFileClip, concatenate_videoclips
clip1 = VideoFileClip("v1.mp4").resize(0.50)
clip2 = VideoFileClip("v2.mp4").resize(0.50)
final_clip = concatenate_videoclips([clip1,clip2])
final_clip.write_videofile("out.mp4")
```

### mencoder

Bir AVI video'nun parcasini kesip cikartmak icin

```
mencoder -ss 00:09:00 -endpos 00:02:00 -oac copy -ovc copy kaynak.avi
-o parca.avi
```

Ustteki komut kaynak.avi dosyasinin 9'uncu dakikasindan 2 dakikalik
parca cikartip parca.avi dosyasina kaydediyor. Dikkat: Ustteki
seceneklerde "endpos" ibaresinin tercumesi "bitis noktasi" olmasina
ragmen komut "reklam edildigi gibi" islemiyor.

### Video'dan Ses Cikartmak

```
ffmpeg -y -ss 00:02:10 -i vid.mp4 -t 00:00:04 -q:a 0 -map a out.mp3
```

### Pek Çok Parça, Altyazı

Bazen birden fazla video'nun birden fazla parcasini cekip cikarip birlestirip
hepsine zaman indisi verilerek altyazi koymak isteyebiliriz. Alttaki kod
ImageMagick `convert` ve `ffmpeg` cagrisini sarmalayarak bu islemleri yapiyor.

```python
import os

def avigif(pieces, text):
    files = ""
    for i,(file,start,dur) in enumerate(pieces):
        cmd = 'ffmpeg -y -ss %s -i %s -t %s -c copy /tmp/out-%d.mp4' % (start, file, dur, i)
        print (cmd)
        os.system(cmd)       
        cmd = 'ffmpeg -y -i /tmp/out-%d.mp4 -filter_complex "fps=10,scale=360:-1:flags=lanczos,split[x1][x2];[x1]palettegen[p];[x2][p]paletteuse" /tmp/out-%d.gif' % (i,i)
        files += "/tmp/out-%d.gif " % i 
        print (cmd)
        os.system(cmd)
    
    cmd = "convert %s /tmp/output1.gif" % files
    print (cmd)
    os.system(cmd)
        
    w = '"fps=10,scale=360:-1:flags=lanczos,'
    for i,(start,finish,text,pos) in enumerate(text):  
        w += "drawtext=enable='between(t,%s,%s)':fontfile=font3.ttf:text='%s':fontsize=15:fontcolor=white:x=%s:y=%s," % (start,finish,text,pos[0],pos[1])
    w += 'split[x1][x2];[x1]palettegen[p];[x2][p]paletteuse"'
    cmd = "/usr/bin/ffmpeg -y -i /tmp/output1.gif -filter_complex " + w + " /tmp/output2.gif"
    print (cmd)
    os.system(cmd)       
```

Örnek olarak [2]'deki video'yu kullanalım, tek bir mp4 video
dosyasından tek parça çıkartıyoruz, 2'inci ve 6'inci saniye arasındaki
parça bu, ve ilk iki saniyeye bir yazı, sonrakine başka bir yazı
yazıyoruz. Eğer isteseydik bu listeleri büyütebilirdik, farklı
video'lardan farklı parçalar daha fazla alt yazı
olabilirdi. `(100,170)` diye gösterilen GİF içindeki ekran
kordinatıdır, yazının nereye konulacağını kontrol eder.

```python
ps = [['/tmp/bwalk1.mp4','00:00:02','00:00:4']]
text = [[1,2,'Ilk Yazi',(100,170)],
        [3,4,'Sonraki Yazi',(80,170)]]

avigif(ps,text)
```

Kaynaklar

[1] https://engineering.giphy.com/how-to-make-gifs-with-ffmpeg/

[2] [Ornek Video](https://drive.google.com/uc?export=view&id=1nR4E7SYLfKhm8nO0BEfFcw0pwWmMNm19)

