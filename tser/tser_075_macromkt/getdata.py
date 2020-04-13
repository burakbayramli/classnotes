import pandas_datareader.data as web

import datetime
start = datetime.datetime(2013, 9, 8)
end = datetime.datetime(2014, 10, 18)
f = web.DataReader('GOOG', 'yahoo', start, end)
f.to_csv('GOOG.csv')
exit()

import quandl, os

fname = '%s/.quandl' % os.environ['HOME']
if not os.path.isfile(fname):
    print ('Please create a %s file ' % fname)
    exit()
auth = open(fname).read()

df1 = quandl.get("FRED/GDPC1-Real-Gross-Domestic-Product", returns="pandas",authtoken=auth)
df2 = quandl.get("RATEINF/INFLATION_USA-Inflation-YOY-USA", returns="pandas",authtoken=auth)
df1.to_csv('quandl-gdp.csv')
df2.to_csv('quandl-inf.csv')

# deprecated
#aapl_options = web.Options('AAPL', 'yahoo')
#aapl_options = aapl_options.get_all_data().reset_index()
