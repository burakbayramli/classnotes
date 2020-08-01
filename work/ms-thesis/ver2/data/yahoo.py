import datetime
import pandas as pd
import pandas.io.data

df = pd.io.data.get_data_yahoo('XU100.IS', 
                                 start=datetime.datetime(1999, 4, 29), 
                                 end=datetime.datetime(2015, 7, 26))
df.to_csv('ise.csv')

df = pd.io.data.get_data_yahoo('^DJI', 
                                 start=datetime.datetime(1990, 8, 28), 
                                 end=datetime.datetime(2015, 7, 26))
df.to_csv('dji.csv')

