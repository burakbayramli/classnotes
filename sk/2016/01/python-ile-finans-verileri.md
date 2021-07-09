# Python ile Finans Verileri

Finans verilerini indirmek, işlemek Python üzerinde iyice
basitleşti. Veri işleme amaçlı başlatılan Pandas'ın yazarı zaten
projesini ilk başta zaman serilerileri / finans verisi işlemek için
başlattığını söylemişti. Bu yakınlık devam etmiş anlaşılan, şu anda
Yahoo Finance, Google Finance, hatta makroekonomik veriler için FRED
bağlantısı var. Hatta birisi opsiyon (option) verisi indirecek kodları
bile eklemiş - açık yazılımın faydaları.

Bazi ornekler altta,

İndeks verisi, mesela Nasdaq için `^İXİÇ`, biraz paketsiz, kendi işimizi
kendimiz yapmamız gerekiyor,

```
import pandas as pd, datetime, time
from io import BytesIO

end = datetime.datetime.now()
start=end-datetime.timedelta(days=90)
start = int(timelib.mktime(start.timetuple()))
end = int(timelib.mktime(end.timetuple()))

url = "https://query1.finance.yahoo.com/v7/finance/download/^IXIC?period1=" + str(start) + "&period2=" + str(end) + "&interval=1d&events=history&includeAdjustedClose=true"
r = urllib2.urlopen(url).read()
file = BytesIO(r)
df = pd.read_csv(file,index_col='Date')3
```

Aynı şekilde 'fred' ABD merkez bankası tabanından veri indirebiliyor.

Opsiyon

Alttaki url kazinabilir

https://www.nasdaq.com/symbol/aapl/option-chain

Makroekonomik Veriler ve Digerleri

Alttaki ornek ABD/Avro doviz kuru icin; issizlik, enflasyon, vs. gibi
pek cok veri mumkun. Ornek Ingiliz pound ile ABD dolari arasinda,

```
import pandas_datareader.data as web
import quandl, os
auth = '[QUANDL KODU]'
df1 = quandl.get("CURRFX/GBPUSD",
                 returns="pandas",
                 start_date='2010-01-01',
                 end_date='2018-01-01',
                 authtoken=auth)

```







