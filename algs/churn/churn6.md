
```python
# 15065
# 13808
import pandas as pd, zipfile
dateparse = lambda x: pd.datetime.strptime(x, '%m/%d/%y %H:%M').date()
with zipfile.ZipFile('/home/burak/Documents/Dropbox/Public/data/retail.zip', 'r') as z:
    df =  pd.read_csv(z.open('online_retail.csv'),parse_dates=['InvoiceDate'],date_parser=dateparse)
    dfc =  pd.read_csv(z.open('online_retail_side_data_extended.csv'),index_col='CustomerID')
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
max_date = df.InvoiceDate.max().strftime('%d-%m-%Y')
max_date = datetime.datetime.strptime(max_date, '%d-%m-%Y')
print max_date
```

```text
2011-12-09 00:00:00
```


```python
import datetime
import itertools
res = []
cid = 15065
cdates = df[df.CustomerID == cid].InvoiceDate.dt.strftime('%d-%m-%Y').unique()
for c1,c2 in zip(cdates,cdates[1:]):
    print c1,c2
    c1 = datetime.datetime.strptime(c1, '%d-%m-%Y')
    c2 = datetime.datetime.strptime(c2, '%d-%m-%Y')
    res.append([c1, 0])
    res.append([c1 + datetime.timedelta(days=1),(c2-c1).days-1])
    res.append([c2 - datetime.timedelta(days=1), 1])

res.append([c2 + datetime.timedelta(days=1),(max_date-c2).days-1])
res.append([max_date,0.0])

df2 = pd.DataFrame(res,columns=['dt1','tte'])
print
print df2
```

```text
21-02-2011 18-03-2011
18-03-2011 27-03-2011
27-03-2011 26-04-2011
26-04-2011 28-06-2011
28-06-2011 02-11-2011

          dt1    tte
0  2011-02-21    0.0
1  2011-02-22   24.0
2  2011-03-17    1.0
3  2011-03-18    0.0
4  2011-03-19    8.0
5  2011-03-26    1.0
6  2011-03-27    0.0
7  2011-03-28   29.0
8  2011-04-25    1.0
9  2011-04-26    0.0
10 2011-04-27   62.0
11 2011-06-27    1.0
12 2011-06-28    0.0
13 2011-06-29  126.0
14 2011-11-01    1.0
15 2011-11-03   36.0
16 2011-12-09    0.0
```

```python
d1 = df.InvoiceDate.min().strftime('%d-%m-%Y')
d2 = df.InvoiceDate.max().strftime('%d-%m-%Y')
d1 = datetime.datetime.strptime(d1, '%d-%m-%Y')
d2 = datetime.datetime.strptime(d2, '%d-%m-%Y')
print d1, d2
all_dates = [d1 + datetime.timedelta(days=x) for x in range((d2-d1).days + 1)]
df3 = pd.DataFrame(index=all_dates)
df3['tte'] = df2.set_index('dt1')['tte']
df3['tte'] = df3['tte'].interpolate(method='linear')
```

```text
2010-12-01 00:00:00 2011-12-09 00:00:00
```

```python
last = datetime.datetime.strptime(cdates[-1], '%d-%m-%Y')
df4 = df3.copy()
df4 = df4[df4.index > cdates[0]]
df4['censor'] = 1.0
df4.loc[df4.index > last,'censor'] = 0.0
df4['age'] = dfc.ix[cid]['Age']
df4['gender'] = dfc.ix[cid]['Gender']
df4['gender'] = (df4['gender']=='Male').astype(float)
df4.to_csv('out-%d.csv' % cid)
```

```python
import pickle
file = open("recs.pkl",'r')
recs = pickle.load(file)
file.close()
```

```python
idx = 400
print len(recs[idx])
#print recs[idx]
# tte,censor,age,gender
```

```text
235
```

































































