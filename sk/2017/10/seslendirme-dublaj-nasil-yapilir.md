# Seslendirme, Dublaj Nasil Yapilir

Diyelim YouTube'dan bir video'ya Turkce seslendirme yapmak
istiyoruz. Once video'yu indiririz, youtube-dl araci bu is
kullanilabilir,

```
sudo pip install youtube-dl
```

```
youtube-dl [youtube video url adresi]
```

Video alindiktan sonra, mesela video-en.webm diye bir dosya olsun,
video'yu baslatiriz, ayni anda Android cep telefonunda Voice Recorder
aracini baslatiriz. Benim yaklasimim ana video'dan 5-10 saniye kadar
izlemek, video dondurmak (pause), izledigim / duydugum konusmayi
aklimda tercume edip aninda VR ile mikrofonda kaydetmek. Yani baska
hicbir on hazirlik yapmadan direk  kaydi yapmak. Gayet basit. Tabii
video / kayit donduruldugunda dusunmek icin bol vakit var, ve bu
izle-tercume-kaydet islemini 5 dakikalik buyuk bloklara ayiriyorum,
boylece aralarda mola vermek, hata ihtimalini azaltmak kolaylassin. VR
ile kaydedilenler bir daha geri gidip duzeltilmiyor, o sebeple cok
hatali bir kayit varsa sadece son 5 dakikalik blok tekrar kaydedilir,
gerisi kaybedilmez.

Neyse, diyelim 15 dakikalik ana video'dan sonra 3 tane 5'er dakikalik
tercume kayit edildi. VR kayitlari m4a formatinda dosyalar, bu
dosyalari alip dizustune getiririz, v1.m4a, v2.m4a,.. olsun,

```
ffmpeg  -i v1.m4a  v1.mp3
ffmpeg  -i v2.m4a  v3.mp3
ffmpeg  -i v3.m4a  v4.mp3
```

diye mp3 formatina ceviririz. Bu dosyalari birlestirip tek bir dosya
yaratalim,

```
ffmpeg -i "concat:v1.mp3|v2.mp3|v3.mp3" -acodec copy voice.mp3
```

Simdi nihai seslendirme icin video'nun ses kismini bizim kaydettigimiz
mp3 yapalim,

```
ffmpeg -i video_en.webm -i voice.mp3 -map 0:v -map 1:a -c:v copy -c:a libvorbis -ac 2 -shortest video_tr.webm
```

Artik video_tr.webm icinde seslendirilmis video var.






