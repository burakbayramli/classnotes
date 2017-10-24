
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

df2 = pd.DataFrame(res,columns=['dt1','tte'])
print
print 'ilk islem',cdates[0], 'son islem',cdates[-1]
print df2
```

```text
21-02-2011 18-03-2011
18-03-2011 27-03-2011
27-03-2011 26-04-2011
26-04-2011 28-06-2011
28-06-2011 02-11-2011

ilk islem 21-02-2011 son islem 02-11-2011
          dt1  tte
0  2011-02-21    0
1  2011-02-22   24
2  2011-03-17    1
3  2011-03-18    0
4  2011-03-19    8
5  2011-03-26    1
6  2011-03-27    0
7  2011-03-28   29
8  2011-04-25    1
9  2011-04-26    0
10 2011-04-27   62
11 2011-06-27    1
12 2011-06-28    0
13 2011-06-29  126
14 2011-11-01    1
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
print dfc.ix[cid]
```

```text
Gender               Male
Age                    33
Country    United Kingdom
Name: 15065, dtype: object
```

```python
import pickle
file = open("recs.pkl",'r')
recs = pickle.load(file)
file.close()
```

```python
idx = 4
print len(recs[idx])
print recs[idx]
# tte,censor,age,gender
```

```text
176
[[ 100.    1.   33.    1.]
 [  99.    1.   33.    1.]
 [  98.    1.   33.    1.]
 [  97.    1.   33.    1.]
 [  96.    1.   33.    1.]
 [  95.    1.   33.    1.]
 [  94.    1.   33.    1.]
 [  93.    1.   33.    1.]
 [  92.    1.   33.    1.]
 [  91.    1.   33.    1.]
 [  90.    1.   33.    1.]
 [  89.    1.   33.    1.]
 [  88.    1.   33.    1.]
 [  87.    1.   33.    1.]
 [  86.    1.   33.    1.]
 [  85.    1.   33.    1.]
 [  84.    1.   33.    1.]
 [  83.    1.   33.    1.]
 [  82.    1.   33.    1.]
 [  81.    1.   33.    1.]
 [  80.    1.   33.    1.]
 [  79.    1.   33.    1.]
 [  78.    1.   33.    1.]
 [  77.    1.   33.    1.]
 [  76.    1.   33.    1.]
 [  75.    1.   33.    1.]
 [  74.    1.   33.    1.]
 [  73.    1.   33.    1.]
 [  72.    1.   33.    1.]
 [  71.    1.   33.    1.]
 [  70.    1.   33.    1.]
 [  69.    1.   33.    1.]
 [  68.    1.   33.    1.]
 [  67.    1.   33.    1.]
 [  66.    1.   33.    1.]
 [  65.    1.   33.    1.]
 [  64.    1.   33.    1.]
 [  63.    1.   33.    1.]
 [  62.    1.   33.    1.]
 [  61.    1.   33.    1.]
 [  60.    1.   33.    1.]
 [  59.    1.   33.    1.]
 [  58.    1.   33.    1.]
 [  57.    1.   33.    1.]
 [  56.    1.   33.    1.]
 [  55.    1.   33.    1.]
 [  54.    1.   33.    1.]
 [  53.    1.   33.    1.]
 [  52.    1.   33.    1.]
 [  51.    1.   33.    1.]
 [  50.    1.   33.    1.]
 [  49.    1.   33.    1.]
 [  48.    1.   33.    1.]
 [  47.    1.   33.    1.]
 [  46.    1.   33.    1.]
 [  45.    1.   33.    1.]
 [  44.    1.   33.    1.]
 [  43.    1.   33.    1.]
 [  42.    1.   33.    1.]
 [  41.    1.   33.    1.]
 [  40.    1.   33.    1.]
 [  39.    1.   33.    1.]
 [  38.    1.   33.    1.]
 [  37.    1.   33.    1.]
 [  36.    1.   33.    1.]
 [  35.    1.   33.    1.]
 [  34.    1.   33.    1.]
 [  33.    1.   33.    1.]
 [  32.    1.   33.    1.]
 [  31.    1.   33.    1.]
 [  30.    1.   33.    1.]
 [  29.    1.   33.    1.]
 [  28.    1.   33.    1.]
 [  27.    1.   33.    1.]
 [  26.    1.   33.    1.]
 [  25.    1.   33.    1.]
 [  24.    1.   33.    1.]
 [  23.    1.   33.    1.]
 [  22.    1.   33.    1.]
 [  21.    1.   33.    1.]
 [  20.    1.   33.    1.]
 [  19.    1.   33.    1.]
 [  18.    1.   33.    1.]
 [  17.    1.   33.    1.]
 [  16.    1.   33.    1.]
 [  15.    1.   33.    1.]
 [  14.    1.   33.    1.]
 [  13.    1.   33.    1.]
 [  12.    1.   33.    1.]
 [  11.    1.   33.    1.]
 [  10.    1.   33.    1.]
 [   9.    1.   33.    1.]
 [   8.    1.   33.    1.]
 [   7.    1.   33.    1.]
 [   6.    1.   33.    1.]
 [   5.    1.   33.    1.]
 [   4.    1.   33.    1.]
 [   3.    1.   33.    1.]
 [   2.    1.   33.    1.]
 [   1.    1.   33.    1.]
 [   0.    1.   33.    1.]
 [  37.    1.   33.    1.]
 [  36.    1.   33.    1.]
 [  35.    1.   33.    1.]
 [  34.    1.   33.    1.]
 [  33.    1.   33.    1.]
 [  32.    1.   33.    1.]
 [  31.    1.   33.    1.]
 [  30.    1.   33.    1.]
 [  29.    1.   33.    1.]
 [  28.    1.   33.    1.]
 [  27.    1.   33.    1.]
 [  26.    1.   33.    1.]
 [  25.    1.   33.    1.]
 [  24.    1.   33.    1.]
 [  23.    1.   33.    1.]
 [  22.    1.   33.    1.]
 [  21.    1.   33.    1.]
 [  20.    1.   33.    1.]
 [  19.    1.   33.    1.]
 [  18.    1.   33.    1.]
 [  17.    1.   33.    1.]
 [  16.    1.   33.    1.]
 [  15.    1.   33.    1.]
 [  14.    1.   33.    1.]
 [  13.    1.   33.    1.]
 [  12.    1.   33.    1.]
 [  11.    1.   33.    1.]
 [  10.    1.   33.    1.]
 [   9.    1.   33.    1.]
 [   8.    1.   33.    1.]
 [   7.    1.   33.    1.]
 [   6.    1.   33.    1.]
 [   5.    1.   33.    1.]
 [   4.    1.   33.    1.]
 [   3.    1.   33.    1.]
 [   2.    1.   33.    1.]
 [   1.    1.   33.    1.]
 [   0.    1.   33.    1.]
 [  15.    1.   33.    1.]
 [  14.    1.   33.    1.]
 [  13.    1.   33.    1.]
 [  12.    1.   33.    1.]
 [  11.    1.   33.    1.]
 [  10.    1.   33.    1.]
 [   9.    1.   33.    1.]
 [   8.    1.   33.    1.]
 [   7.    1.   33.    1.]
 [   6.    1.   33.    1.]
 [   5.    1.   33.    1.]
 [   4.    1.   33.    1.]
 [   3.    1.   33.    1.]
 [   2.    1.   33.    1.]
 [   1.    1.   33.    1.]
 [   1.    1.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]
 [   1.    0.   33.    1.]]
```





































































