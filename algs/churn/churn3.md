
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
print np.array(df[df.CustomerID == 15065].InvoiceDate)[:5]
```

```text
Index([u'InvoiceNo', u'StockCode', u'Description', u'Quantity', u'InvoiceDate',
       u'UnitPrice', u'CustomerID', u'Country'],
      dtype='object')
2010-12-01 00:00:00 2011-12-09 00:00:00
['2011-02-21T00:00:00.000000000' '2011-02-21T00:00:00.000000000'
 '2011-02-21T00:00:00.000000000' '2011-02-21T00:00:00.000000000'
 '2011-02-21T00:00:00.000000000']
```

```python
import pandas as pd

ds = pd.date_range(start=df.InvoiceDate.min(), end=df.InvoiceDate.max())
print ds[:5]
dff = pd.DataFrame(index=ds)
dff['val'] = 0.
dff.loc[np.array(df[df.CustomerID == 15065].InvoiceDate), 'val'] = 1.0
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





















































