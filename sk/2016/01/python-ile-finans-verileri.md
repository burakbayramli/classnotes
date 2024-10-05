# Python ile Finans Verileri

### Yahoo Finance

Alttaki yöntemle YF arayüze özel URL oluşturarak bağlanıyoruz. Bu
bağlantı yöntemi arka planda `finance.yahoo.com` sitesinin girdi
mantığını takip ediyor, senetler için direk onların kodu girilebilir,
AAPL, MSFT gibi, indisler ve özel göstergeler başında `^` vardır,
indis verisi, mesela Nasdaq için `^IXIC`,

```
import pandas as pd, datetime, time
import urllib.request as urllib2
from io import BytesIO

end = datetime.datetime.now()
start=end-datetime.timedelta(days=90)
start = int(time.mktime(start.timetuple()))
end = int(time.mktime(end.timetuple()))

url = "https://query1.finance.yahoo.com/v7/finance/download/^IXIC?period1=" + str(start) + "&period2=" + str(end) + "&interval=1d&events=history&includeAdjustedClose=true"
r = urllib2.urlopen(url).read()
file = BytesIO(r)
df = pd.read_csv(file,index_col='Date')
```

Not: Yahoo Finans'ın üstteki bağlantı yöntemi 2024 Eylül'de aksaklığa
uğradı, tamamen kapatıldı mı bilinmiyor, bir alternatif yöntem
altadır. Bu yöntem biraz farklı, sonuç JSON olarak geliyor,

```python
import pandas as pd, datetime, requests
import urllib.request as urllib2

d1 = datetime.datetime.strptime("2015-09-01", "%Y-%m-%d").timestamp()
d2 = datetime.datetime.strptime("2024-09-17", "%Y-%m-%d").timestamp()
print (int(d1))
print (int(d2))
```

```text
1441054800
1726520400
```

```python
ticker = "AAPL"
url = "https://query2.finance.yahoo.com/v8/finance/chart/%s?period1=%d&period2=%d&interval=1d&events=history&includeAdjustedClose=true" 
url = url % (ticker,int(d1),int(d2))
r = urllib2.urlopen(url).read()
```

```python
import json
res = json.loads(r)
ts = res['chart']['result'][0]['timestamp']
adjclose = res['chart']['result'][0]['indicators']['adjclose'][0]['adjclose']
ts = [datetime.datetime.fromtimestamp(x).strftime("%Y-%m-%d") for x in ts]
df = pd.DataFrame(adjclose,index=pd.to_datetime(ts),columns=[ticker])
print (df)
```

```text
                  AAPL
2015-08-31   25.457985
2015-09-01   24.320099
2015-09-02   25.363161
2015-09-03   24.918392
2015-09-04   24.670042
...                ...
2024-09-10  220.110001
2024-09-11  222.660004
2024-09-12  222.770004
2024-09-13  222.500000
2024-09-16  216.320007

[2276 rows x 1 columns]
```

### Polygon.io

Ticari bir servis olsa da bedava olan seviyesi hala işe yarıyor, fakat
bedava serviste bazı kısıtlamalar var, mesela tarihi verilerde iki
sene öncesinde daha fazlası verilmiyor. Gene de faydalı olabilir, servise
üye olduktan sonra API anahtar kelimesi / şifresi alınır, biz alttaki
gibi bir kod kullanıyoruz,

```python
poly_url =  "https://api.polygon.io/v2/aggs/ticker/%s/range/1/day/%s/%s?apiKey=%s"
KEY = "[anahtar buraya]"
d1 = "2018-01-01"
ticker = "AAPL"

conf = json.loads(open(fname).read())
today = datetime.datetime.now().strftime("%Y-%m-%d")
url = poly_url % (ticker, d1, today, KEY)
r = urllib2.urlopen(url).read()
data = json.loads(r)
res = data['results']
res = [[ datetime.datetime.fromtimestamp(float(x['t']) / 1000.).strftime("%Y-%m-%d"), x['c']] for x in res]
df = pd.DataFrame(res)
df.columns = ['Date',ticker]
df = df.set_index('Date')
```

### FRED

FRED, tam açılımıyla "Federal Reserve Economic Data" servisi ile ABD
merkez bankası (federal reserve) tabanından veri indirilebiliyor. ABD
gayrı safi milli hasıla verisi için mesela,

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

### Opsiyonlar, Nasdaq

Alttaki url kazınabilir

[https://www.nasdaq.com/symbol/aapl/option-chain](https://www.nasdaq.com/symbol/aapl/option-chain)

### Quandl

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

### Yahoo Finance (Temel Veriler)

Bilanço, kâr, zarar gibi şirket temel verileri için Yahoo Finance
sitesinin bilançolar kısmına bağlanan `yahoo_fin` paketi faydalıdır,
mesela Amazon şirketinin brüt karı (gross profit) ve toplam hasılat
(total revenue) verisi için,

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
