import pandas as pd, datetime
from pandas_datareader import data
start=datetime.datetime(1970, 1, 1)
end=datetime.datetime(2018, 1, 1)
df = data.DataReader(['A4102C1A027NBEA','GDP','CMDEBT','NCBDBIQ027S','POPTOTUSA647NWDB','LNS11300060'], 'fred', start, end)
df.to_csv('gdp.csv')
