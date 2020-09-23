# Python ile Finans Verileri

Finans verilerini indirmek, islemek Python uzerinde iyice
basitlesti. Veri isleme amacli baslatilan Pandas'in yazari zaten
projesini ilk basta zaman serilerileri / finans verisi islemek icin
baslattigini soylemisti. Bu yakinlik devam etmis anlasilan, su anda
Yahoo Finance, Google Finance, hatta makroekonomik veriler icin FRED
baglantisi var. Hatta birisi opsiyon (option) verisi indirecek kodlari
bile eklemis - acik yazilimin faydalari. Bazi ornekler altta,

Senet verisi, mesela MSFT

```
import pandas as pd, datetime
import pandas_datareader.data as web

start=datetime.datetime(2013, 1, 1)
end=datetime.datetime(2015, 9, 30)
s = web.DataReader("MSFT", 'yahoo', start, end)
```

Kaynak icin `google` gecilirse veri oradan gelecek.

Ayni sekilde 'fred' ABD merkez bankasi tabanindan veri indirebiliyor.

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







