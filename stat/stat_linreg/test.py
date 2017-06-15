import pandas as pd
df = pd.read_csv('nes.dat',sep=r'\s+')

df = df[['presvote','year','gender','income','race','occup1']]
df = df.dropna()
df.to_csv('tmp.csv',index=None)
