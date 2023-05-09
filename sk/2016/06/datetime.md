# Tarih Zaman Icin datetime

Python ile bazi datetime numaralari.

Bugunun tarihi

```
import datetime

print (datetime.datetime.now())
```

Class datetime uzerinde bir suru ek bilgi vardir, mesela sadece
bugunun ayini almak icin

```
print (datetime.datetime.now().month)
```

Diger ogeler day, ya da year olabilir.

Bir dosyanin yaratilis tarihi icin (ve o tarihin gunu icin)

```
print (datetime.datetime.fromtimestamp(os.path.getctime("[DOSYA]")).day)
```

Java'dan gelen `currentTimeInMillis()` cagrisini normal tarihe
donusturmek icin

```
millis = 1492161538887
from datetime import date
print date.fromtimestamp(millis/1000.0)
```

Sonuc

```
2017-04-14
```

Yilin haftasini bulmak

```
import datetime
print (datetime.date(2019, 1, 4).isocalendar()[1])
now = datetime.datetime.now()
print (datetime.date(now.year, now.month, now.day).isocalendar()[1])
````

İlk sonuç 1 olacaktır, çünkü Ocak 4'ün yılın kaçınci haftası olduğuna
baktık, ve sonuç tabii ki 1. İkinci işlem içinde olduğumu güne göre
hafta hesabı yapar.

String verisi ile tarih yaratmak,

```
import datetime
s = "10/10/11"
d = datetime.datetime.strptime(s, "%m/%d/%y")
print d
```

```
2011-10-10 00:00:00
```

Gun eklemek (ya da cikartmak)

```
print d + datetime.timedelta(days=10)
```

```
2011-10-20 00:00:00
```

Tekrar string'e cevirmek

```
print d.strftime('%Y-%m-%d')
```

```
2011-10-10
```

Ayri ayri yil, gun, vs vererek yaratmak,

```
d2 = datetime.datetime(1999, 1, 1)
print d2
```

```
1999-01-01 00:00:00
```

Bir tarih objesinin yil, gun, ay ogelerine bakmak,

```
print d2.year, d2.month, d2.day
```

```
1999 1 1
```

Zaman Farklari

Iki tarih arasindaki zaman farkini bulmak icin basit cikartma islemi yeterl

```
d1 = datetime.datetime(1994, 1, 1)
d2 = datetime.datetime(1999, 1, 1)

print d2-d1, type(d2-d1)
```

Sonuc

```
1826 days, 0:00:00 
```

Bu bir timedelta objesinden geliyor, bu objenin gecen zamani saniye
olarak gosterme ozelligi de var

```
print (d2-d1).total_seconds()
```

```
157766400.0
```

Arrow, Pandas

Hem arrow hem pandas paketleri datetime işlenmesini destekliyor. Her
iki paket ile herhangi bir formattaki zamanı alıp çevirebiliriz.

```
import arrow

arrow.get("2017-01-12T14:12:06.000")
```

Fakat üstteki yavaş olabilir,

```
import pandas as pd

arrow.get(pd.Timestamp("2017-01-12T14:12:06.000"))
```

Maya diye bir paket de işleri çok kolaylaştırıyor. Herhangi bir
format'tan okumak, zaman dilimi değişimi gibi işler çok rahat.

```
import maya
s = "00:06:13 May 30, 2019 PDT"
print (s)
t = maya.parse(s).datetime(to_timezone='US/Eastern')
print (t.strftime('%Y-%m-%d %H:%M'))
```


