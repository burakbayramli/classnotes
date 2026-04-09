# zip dosyasindaki veriyi isle, birlestirilmis tek bir csv uret
import zipfile, pandas as pd, collections
dfs = []
with zipfile.ZipFile('rainfall.zip', 'r') as z:
     for f in z.namelist():
         if '.csv' not in f: continue
         df = pd.read_csv(z.open(f))
         dfs.append(df)
df2 = pd.concat(dfs,axis=0)
df2['dt'] = df2.apply(lambda x: pd.to_datetime('%d-%d-%02d' % (x.Year,x.Month,x.Day)),axis=1)
print (len(df2))
df3 = df2.set_index('dt')
df3 = df3.sort_index()
df3['Daily Rainfall Total (mm)'].to_csv('rainfall.csv')
