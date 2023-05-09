# YouTube Video'larini Kaydetmek

En iyisi `youtube-dl` adinda bir Python script'i. pip ile kurulabilir,
`pip install youtube-dl`. Bu sekilde kurulan versiyon problem
cikartirsa, kaynaklar kodlarindan kurmak lazim,

https://github.com/ytdl-org/youtube-dl

İndirmek için

youtube-dl [youtube video baglantisi]

Eğer başka seçenek verilmezse script dosya ismini bile kendisi
yaratıyor, bir mp4 dosyasına video yazılıyor. Çok basit.

Eğer indirme işlemini durdurup tekrar başlatmak isterseniz, ikinci kez
script'i başlattığınızda `-c` seçeneğini kullanabilirsiniz.

Dikkat: kaynaklardan kurunca `setup.py` ile kurulan programın
gerçekten kullanıldığına emin olun. Bu program `youtube-dl/bin`
altında olacak, ve bu programı o dizine gidip `.youtube-dl` ile
işletmeniz gerekebilir (nokta bildiğimiz gibi içine olduğumüz dizine
işaret eder böylece global kurulmuş bir işler programa gidilmesini
engellemiş oluruz)

Daha Ufak Dosyalar

Bazen sadece URL ile video indirince çok büyük dosya rakamları
görülebiliyor, mesela basit bir video için 1 Gigabayt! Daha ufak
şekilde indirmek olmaz mıydı? Bunun için

```
youtube-dl -F [video]
```

deriz,

```
[youtube] Setting language
[youtube] P9pzm5b6FFY: Downloading webpage
[youtube] P9pzm5b6FFY: Downloading video info webpage
[youtube] P9pzm5b6FFY: Extracting video information
[info] Available formats for P9pzm5b6FFY:
format code extension resolution  note 
140         m4a       audio only  DASH audio , audio@128k (worst)
160         mp4       144p        DASH video , video only
133         mp4       240p        DASH video , video only
134         mp4       360p        DASH video , video only
135         mp4       480p        DASH video , video only
136         mp4       720p        DASH video , video only
17          3gp       176x144     
36          3gp       320x240     
5           flv       400x240     
43          webm      640x360     
18          mp4       640x360     
22          mp4       1280x720    (best)
```

gibi bir çıktı gelecek. Burada en iyi (büyük dosya) 22 ile gelecek,
ama belki `flv` de bizim için yeterli, o zaman

```
youtube-dl -f 5 [video]
```

ile indirmeyi yaparız. 






