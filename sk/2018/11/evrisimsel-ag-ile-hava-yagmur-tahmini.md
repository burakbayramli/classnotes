
Günlük baz meteoroloji verileriyle bir(kaç) gün sonrası yağmur yagıp
yağmayacağı evrişimsel ağ (convolutional network -CNN-) ile tahmin
edilebilir mi? Alttaki makalede mümkün olabileceği bahsediliyor.

[https://deepstruct.github.io/ICML17/1stDeepStructWS_paper_2.pdf](https://deepstruct.github.io/ICML17/1stDeepStructWS_paper_2.pdf)

Bu yaklaşım farklı bir veri üzerinde kullanabilir belki.

ABD NOAA'dan dünyada 100K+ civarı istasyondan alınmış 20+ ölçüm öğesi
üzerinden günlük verileri indiririz. Mesela 2016 için

`wget http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/2016.csv.gz`

Biz bu verileri indirip zip haline getirdik,

2012-2018

[https://drive.google.com/open?id=1nPnKSHjDEacKw7fbXaYn5b-Sf86SxOvr](https://drive.google.com/open?id=1nPnKSHjDEacKw7fbXaYn5b-Sf86SxOvr)

[https://drive.google.com/open?id=1J9A6Nwyy4iJ3W56D1_l-ncmODPCaev5m](https://drive.google.com/open?id=1J9A6Nwyy4iJ3W56D1_l-ncmODPCaev5m)

[https://drive.google.com/open?id=1tGpA60u8ZcfwTGaU0qmtGLHFnjC89H8C](https://drive.google.com/open?id=1tGpA60u8ZcfwTGaU0qmtGLHFnjC89H8C)

[https://drive.google.com/open?id=18EeWt12ce4gTtL13mH1SjoAC-a4mWTvW](https://drive.google.com/open?id=18EeWt12ce4gTtL13mH1SjoAC-a4mWTvW)

[https://drive.google.com/open?id=1nR9CPrrPLeFpI3TFPgTvSAiFy_mH3oHW](https://drive.google.com/open?id=1nR9CPrrPLeFpI3TFPgTvSAiFy_mH3oHW)

[https://drive.google.com/open?id=1_YeFlx3oisLIPBfnVN_iJgRLxyVvA_QT](https://drive.google.com/open?id=1_YeFlx3oisLIPBfnVN_iJgRLxyVvA_QT)

[https://drive.google.com/open?id=1VESYnMNwCQhg6tdxp5VZRZ-5cUXEJYgQ](https://drive.google.com/open?id=1VESYnMNwCQhg6tdxp5VZRZ-5cUXEJYgQ)

Veri içeriği

[ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/readme.txt](ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/readme.txt)

İstasyonlar

[https://drive.google.com/open?id=1vuJrb8xc4c2MK6LMbexaE-j-it38MCKU](https://drive.google.com/open?id=1vuJrb8xc4c2MK6LMbexaE-j-it38MCKU)

Türkiye için 10+ civarı istasyon var. 

```
df = pd.read_csv('ghcnd-stations2.txt',sep='|',header=None)
```

ile okunabilir.

Ana veri içeriğine bakınca

```
import zipfile, csv, io
zfile = '2017.zip'
zip_file    = zipfile.ZipFile(zfile)
items_file  = zip_file.open('2017.csv')
items_file  = io.TextIOWrapper(items_file)
rd = csv.reader(items_file)
for row in rd:    
    print (row)

['US1MISW0005', '20170101', 'PRCP', '0', '', '', 'N', '']
['US1MISW0005', '20170101', 'SNOW', '0', '', '', 'N', '']
['US1MISW0005', '20170101', 'SNWD', '0', '', '', 'N', '']
['CA1MB000296', '20170101', 'PRCP', '0', '', '', 'N', '']
['US1MAMD0069', '20170101', 'PRCP', '56', '', '', 'N', '']
['ASN00015643', '20170101', 'TMAX', '274', '', '', 'a', '']
...
```

şeklinde bir çıktı görülür. İlk kolon istasyon kimliği, 2. gün, sonra
öğe. Bu verinin ilk halini isleyen bir kod
[surada](http://jmausolf.github.io/code/Using_Pandas_in_Python/).
`PRCP` 100 üzerinden yağmur seviyesi. `TMAX` bir günün en yüksek
ısısı.

Fikir şudur, günlük seçilmiş birkaç öğe için, bir tahmin noktası (bir
şehir olabilir) etrafında bir izgara (grid) yaratılır, 20x20 boyutunda
diyelim, ızgara köşelerinin kordinatlarına en yakın istasyon verisi
öğeler için toplanır. Bir günde diyelim TMAX, SNOW, vs. iki öğe bir
tensor içinde iki boyut olur, aynen R, G, B kanallarının bir resmin
farklı tensor katmanı, boyutu olması gibi.

![](https://1.bp.blogspot.com/-BSPLV5ATi04/XAGxeG2m9SI/AAAAAAAABxo/-FkYPJWNNrwrZm-QPaDoqw6uxbZqX-s5ACLcBGAs/s1600/weather_1.png)

Üstteki resim orijinal makaledeki katmanları gösteriyor, makale farklı
atmosfer seviyelerindeki basıncı kullanmış. 

İstasyon verisi öğeler için kat kat konur ve tensor yaratılır, bu bir
gün. Kaynak verisi bu, ertesi günün `PRCP` öğesi tahmin hedefi, ve (2
boyutlu) evrişimsel ağ işletilir. Zaman faktörü için makalede
arkadaşın yaptığı gibi önceki günlerin ek tensorları konur ve [3
boyutlu evrişim](https://youtu.be/ecbeIRVqD7g) işletilir.


