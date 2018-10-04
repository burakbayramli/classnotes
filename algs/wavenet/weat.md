

```python
import pandas as pd
import pandas as pd, zipfile
cols = "Month;Year;Day;Temp;Max;Min;Precip;Wind Speed;Visibility;Dew Point;Max Wind;Wind Gust;Sea Level Pressure;Station Pressure;Snow Depth;Weather Type"
cols = cols.split(';')
with zipfile.ZipFile('/Users/burak.bayramli/Downloads/weather_belgium_1973_2006.csv.zip', 'r') as z:
    df =  pd.read_csv(z.open('weather_belgium_1973_2006.csv'),sep='|',na_values=['N/A*'])
    df.columns = cols

print df.shape
df = df.fillna(0)
data = np.array(df)[:-300]
print data
print data.shape
```

```text
(11363, 16)
[[1 1973 2 ... 0 0 'Fog']
 [1 1973 3 ... 0 0 'Fog']
 [1 1973 4 ... 0 0 'Fog']
 ...
 [2 2006 1 ... '1,021.30' 0 'Fog']
 [2 2006 2 ... '1,020.30' 0 'Fog']
 [2 2006 3 ... '1,020.10' 0 'Rain/Drizzle']]
(11063, 16)
```


