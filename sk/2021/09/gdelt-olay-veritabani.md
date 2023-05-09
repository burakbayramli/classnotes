# GDELT Dünya Olayları Veri Tabanı (Event Database)

Bu proje yapay öğrenim (machine learning) teknikleri kullanarak
İnternet'teki haber makalelerini kategorize etmeye uğraşır. Günlük,
yıllık verileri basit CSV dosyaları içinde her satır bir haber olacak
şekilde paylaşıyorlar. Gösterilen [1] bağlantısına gidince her gün
için bir zip dosyası görüyoruz, her gün için dosya ismini hazırlayıp
onu otomatik indiren bir Python script altadır. Örnek olarak 10 Eylül
2022 günü indirildi,

```python
import pandas as pd, datetime
from zipfile import ZipFile
from io import BytesIO
import urllib.request as urllib2

base_conflict_url = "http://data.gdeltproject.org/events"

conf_cols = ['GlobalEventID', 'Day', 'MonthYear', 'Year', 'FractionDate',\
       'Actor1Code', 'Actor1Name', 'Actor1CountryCode', 'Actor1KnownGroupCode',\
       'Actor1EthnicCode', 'Actor1Religion1Code', 'Actor1Religion2Code',\
       'Actor1Type1Code', 'Actor1Type2Code', 'Actor1Type3Code', \
       'Actor2Code', 'Actor2Name', 'Actor2CountryCode', 'Actor2KnownGroupCode',
       'Actor2EthnicCode', 'Actor2Religion1Code', 'Actor2Religion2Code',
       'Actor2Type1Code', 'Actor2Type2Code', 'Actor2Type3Code', \
       'IsRootEvent','EventCode', 'EventBaseCode','EventRootCode',\
       'QuadClass', 'GoldsteinScale','NumMentions','NumSources', \
       'NumArticles', 'AvgTone','Actor1Geo_Type', 'Actor1Geo_FullName',\
       'Actor1Geo_CountryCode', 'Actor1Geo_ADM1Code','Actor1Geo_Lat', \
       'Actor1Geo_Long', 'Actor1Geo_FeatureID','Actor2Geo_Type', \
       'Actor2Geo_FullName','Actor2Geo_CountryCode', 'Actor2Geo_ADM1Code',\
       'Actor2Geo_Lat', 'Actor2Geo_Long']

sd = "%d%02d%02d" % (2022, 9, 10)
url = base_conflict_url + "/%s.export.CSV.zip" % sd
r = urllib2.urlopen(url).read()
file = ZipFile(BytesIO(r))
csv = file.open("%s.export.CSV" % sd)
df = pd.read_csv(csv,sep='\t',header=None)    
url_col = df[57]        
df = df[range(len(conf_cols))]
df.columns = conf_cols
```

Bu noktada veri Pandas DataFrame objesi halindedir. Daha da
filtreleyebiliriz, mesela olay kodu 190 ve 194 içeren olaylara
bakalım. GDELT belgelerine [2] göre bu kodlar saldırı anlamına
geliyor, ya askeri konvansiyonel şekilde genel saldırı. Olaylar ayrıca
1'inci aktör ve 2'inci aktör olarak ayrılmaya uğraşılmış, bir aktör
bir diğerine bir etkide bulunuyorsa birinci ve ikinci aktör bunlar
oluyor. Birinci Rusya ikinci aktör Ukrayna olarak filtreleme yapalım,

```python
df2 = df[(df.EventCode==190)|(df.EventCode==195)|(df.EventCode==194)]
df3 = df2[(df2.Actor1Name=='RUSSIA') & (df2.Actor2Name=='UKRAINE')]
cols = ['Actor1Name','Actor2Name','Actor2Geo_Lat','Actor2Geo_Long']
df3 = df3[cols]
print (len(df3))
print (df3.tail(4))
```

```text
22
      Actor1Name Actor2Name  Actor2Geo_Lat  Actor2Geo_Long
51112     RUSSIA    UKRAINE        60.0000        100.0000
54643     RUSSIA    UKRAINE        46.6558         32.6178
55912     RUSSIA    UKRAINE        60.0000        100.0000
55914     RUSSIA    UKRAINE        49.9808         36.2527
```

Bazı sonuçlar geldi. Sondan dört tane gösterdik, olay sırasında ikinci
aktörün nerede olduğunu göstermiş. Hangi haber baz alınarak mesela son
bilgi toplanmış?


```python
print (df3.loc[55914])
print (url_col.loc[55914])
```

```text
Actor1Name         RUSSIA
Actor2Name        UKRAINE
Actor2Geo_Lat     49.9808
Actor2Geo_Long    36.2527
Name: 55914, dtype: object
https://katcountry989.com/2022/09/10/russian-rockets-hit-ukraines-kharkiv-killing-one-governor/
```

Bağlantıyı ziyaret edince hakikaten bir saldırı haberi olduğunu görüyoruz.

Fakat dikkat: her haber bu şekilde kategorize edilmeyebiliyor. Kullanıcıların
GDELT verisini başlangıç noktası kabul ederek üstüne ek filtreleme yapmaları
gerekebilir.

Yanlış bir örnek olarak alttaki makaleye bakalım,

```python
print (df2.loc[67736][cols])
print (url_col.loc[67736])
```

```text
Actor1Name            NEVADA
Actor2Name        JOURNALIST
Actor2Geo_Lat        38.4199
Actor2Geo_Long      -117.122
Name: 67736, dtype: object
https://twitchy.com/brettt-3136/2022/09/10/reporter-nevada-gubernatorial-candidate-refuses-to-denounce-donald-trumps-normalization-of-attacks-on-reporters/
```

Bu makaleye bakınca evet içinde saldırı kelimesi geçtiğini görüyoruz
fakat bu askeri değil, politika bağlamında sözel bir çekişme var. Demek
ki GDELT her zaman işlemiyor, ek filtreleme yapmak gerekebilir.


Kaynaklar

[1] [GDELT](http://data.gdeltproject.org/events/index.html)

[2] [GDELT Olay Kodlari - PDF](http://data.gdeltproject.org/documentation/CAMEO.Manual.1.1b3.pdf)
