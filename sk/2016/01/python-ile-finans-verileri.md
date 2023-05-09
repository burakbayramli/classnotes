# Python ile Finans Verileri

Finans verilerini indirmek, işlemek Python üzerinde iyice
basitleşti. Veri işleme amaçlı başlatılan Pandas'ın yazarı zaten
projesini ilk başta zaman serileri / finans verisi işlemek için
başlattığını söylemişti. Bu yakınlık devam etmiş anlaşılan, şu anda
Yahoo Finance, Google Finance, hatta makroekonomik veriler için FRED
bağlantısı var. Hatta birisi opsiyon (option) verisi indirecek kodları
bile eklemiş - açık yazılımın faydaları. Bazı örnekler altta.

İndeks verisi, mesela Nasdaq için `^IXIC`, ama paketsiz, kendi işimizi
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

Aynı şekilde 'FRED' ABD merkez bankası tabanından veri indirilebiliyor. 
ABD gayrı safi milli hasıla verisi için mesela,

```python
import pandas as pd, datetime
from pandas_datareader import data
import quandl

today = datetime.datetime.now()
start=datetime.datetime(1992, 1, 1)
end=datetime.datetime(today.year, today.month, today.day)
cols = ['GDPC1']
df = data.DataReader(cols, 'fred', start, end)
print (df.tail(4))
```

```text
                GDPC1
DATE                 
2021-01-01  19055.655
2021-04-01  19368.310
2021-07-01  19478.893
2021-10-01  19806.290
```

Opsiyon

Alttaki url kazınabilir

https://www.nasdaq.com/symbol/aapl/option-chain

Makroekonomik Veriler ve Digerleri

Alttaki örnek ABD/Avro döviz kuru için; işsizlik, enflasyon, vs. gibi
pek çok veri mümkün. Yanlız Quandl verisi için üye olup bir APİ
anahtarı almak lazım, o anahtarı alıp `auth` içine yazınca herşey
işler. Örnek İngiliz pound ile ABD doları arasında,

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

Bilanco, kar, zarar gibi şirket temel verileri için `yahoo_fin` paketi
faydalıdır, mesela Amazon şirketinin brüt karı (gross profit) ve
toplam hasılat (total revenue) verisi için,

```python
from yahoo_fin.stock_info import get_income_statement
ticker = 'AMZN'
res = get_income_statement(ticker,yearly=False).T
df = res.sort_index()
print (df[['grossProfit','totalRevenue']])
res = get_income_statement(ticker,yearly=True).T
df = res.sort_index()
print (df[['grossProfit','totalRevenue']])
```

```text
Breakdown   grossProfit  totalRevenue
endDate                              
2021-03-31  46115000000  108518000000
2021-06-30  48904000000  113080000000
2021-09-30  47882000000  110812000000
2021-12-31  54577000000  137412000000
Breakdown    grossProfit  totalRevenue
endDate                               
2018-12-31   93731000000  232887000000
2019-12-31  114986000000  280522000000
2020-12-31  152757000000  386064000000
2021-12-31  197478000000  469822000000
```

Hisse Başına Kâr (Earnings Per Share)

Şirketlerin hisse başına karlılık oranları borsacılar tarafından
yakından takip edilir, her şirket her çeyrekte belli günlerde bu
rakamı açıklar ve buna göre senet fiyatları yükselip düşebilir.
Karlılık o kadar kritiktir ki önceden hakkında bir konsensüs tahmini
bile yapılır ve bu açıkça bilinir, bu tahmine yaklaşıp yaklaşılmaması
da senet fiyatını etkiler. Bu veriyi almak için, mesela Walmart
şirketi için,

```python
from yahoo_fin.stock_info import get_earnings_history
import pandas as pd

res =  get_earnings_history('WMT')
df = pd.DataFrame.from_dict(res)
pd.set_option('display.max_columns', None)
df.head(4)[['startdatetime','epsestimate','epsactual']]
```

```text
Out[1]: 
              startdatetime  epsestimate  epsactual
0  2022-08-16T07:00:00.000Z  1.83        NaN       
1  2022-05-17T07:02:00.000Z  1.48         1.30     
2  2022-02-17T07:11:00.000Z  1.50         1.53     
3  2021-11-16T07:01:00.000Z  1.40         1.45     
```

Görüldüğü gibi analist tahmini 1.48 ama birinci çeyrekte karlılık 1.30.
Tahmine eriselemedi ve Walmart senetleri bunu yazdığımız Mayıs 2022'de
ağır bir darbe yedi. Ayrıca bir sonraki çeyrek için tahminin yapılmış olduğunu
görüyoruz, 1.83 diyor. Bakalım ne olacak.





