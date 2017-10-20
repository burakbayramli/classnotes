
```python
import pandas as pd, zipfile
dateparse = lambda x: pd.datetime.strptime(x, '%m/%d/%y %H:%M').date()
with zipfile.ZipFile('/home/burak/Documents/Dropbox/Public/data/retail.zip', 'r') as z:
    df =  pd.read_csv(z.open('online_retail.csv'),parse_dates=['InvoiceDate'],date_parser=dateparse)
print df[df.CustomerID == 15065][['Description','InvoiceDate']].tail(10)
```

```text
                            Description InvoiceDate
433515  PAPER CHAIN KIT 50'S CHRISTMAS   2011-11-02
433516         ROLL WRAP 50'S CHRISTMAS  2011-11-02
433517        IVORY WICKER HEART MEDIUM  2011-11-02
433518  VINTAGE DOILY TRAVEL SEWING KIT  2011-11-02
433519         JUMBO BAG VINTAGE DOILY   2011-11-02
433520        HAND WARMER RED RETROSPOT  2011-11-02
433521          HAND WARMER BIRD DESIGN  2011-11-02
433522    PRETTY HANGING QUILTED HEARTS  2011-11-02
433523          BUBBLEGUM RING ASSORTED  2011-11-02
433524           6 RIBBONS RUSTIC CHARM  2011-11-02
```

```python
print df.columns
print df.InvoiceDate.min(), df.InvoiceDate.max()
print np.array(df[df.CustomerID == 15065].InvoiceDate.unique())[:5]
```

```text
Index([u'InvoiceNo', u'StockCode', u'Description', u'Quantity', u'InvoiceDate',
       u'UnitPrice', u'CustomerID', u'Country'],
      dtype='object')
2010-12-01 00:00:00 2011-12-09 00:00:00
['2011-02-21T00:00:00.000000000' '2011-03-18T00:00:00.000000000'
 '2011-03-27T00:00:00.000000000' '2011-04-26T00:00:00.000000000'
 '2011-06-28T00:00:00.000000000']
```

```python
import pandas as pd

ds = pd.date_range(start=df.InvoiceDate.min(), end=df.InvoiceDate.max())
print ds[:5]
dff = pd.DataFrame(index=ds)
dff['val'] = 0.
dff.loc[np.array(df[df.CustomerID == 15065].InvoiceDate.unique()), 'val'] = 1.0
dff.to_csv('/tmp/out.csv')
print dff.tail(10)
```

```text
DatetimeIndex(['2010-12-01', '2010-12-02', '2010-12-03', '2010-12-04',
               '2010-12-05'],
              dtype='datetime64[ns]', freq='D')
            val
2011-11-30  0.0
2011-12-01  0.0
2011-12-02  0.0
2011-12-03  0.0
2011-12-04  0.0
2011-12-05  0.0
2011-12-06  0.0
2011-12-07  0.0
2011-12-08  0.0
2011-12-09  0.0
```

```python
import datetime

print (pd.datetime.strptime('02/02/2002', '%d/%M/%Y') - pd.datetime.strptime('02/02/2001', '%d/%M/%Y')).days
```

```text
365
```

```python
import datetime
cdates = df[df.CustomerID == 15065].InvoiceDate.unique()
df2 = pd.DataFrame(cdates, columns=['dt1'])
df2['dt2'] = df2['dt1'].shift(-1)
df2 = df2.dropna()
df2['diff'] = df2.apply(lambda x: (x['dt2']-x['dt1']).days,axis=1)
print df2
```

```text
         dt1        dt2  diff
0 2011-02-21 2011-03-18    25
1 2011-03-18 2011-03-27     9
2 2011-03-27 2011-04-26    30
3 2011-04-26 2011-06-28    63
4 2011-06-28 2011-11-02   127
```












































































