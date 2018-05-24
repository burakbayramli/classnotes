import pandas as pd, datetime
from pandas_datareader import data
start=datetime.datetime(1970, 1, 1)
end=datetime.datetime(2017, 1, 1)
df = data.DataReader(['GDP','IR3TIB01USM156N'], 'fred', start, end)
df.columns = ['gdp','shortrate']
df.to_csv('rates.csv')

import pandas as pd, datetime
from pandas_datareader import data
start=datetime.datetime(1970, 1, 1)
end=datetime.datetime(2017, 1, 1)
df = data.DataReader(['M2SL','GDP','CRDQUSAPABIS','REALLN'], 'fred', start, end)
df.columns = ['m2cd','gdp','nonfincred','realest']
df.to_csv('money.csv')

import pandas as pd, datetime
from pandas_datareader import data
start=datetime.datetime(1970, 1, 1)
end=datetime.datetime(2017, 1, 1)
# Personal Consumption Expenditures - DPCERD3Q086SBEA
# Federal government total expenditures - W019RCQ027SBEA
# State local government total expenditures -  W079RCQ027SBEA
# Real Gross Private Domestic Investment - GPDIC1
# Net Exports of Goods and Services - NETEXP
# Gross Domestic Product - GDP
# Total Credit to Private Non-Financial Sector - CRDQUSAPABIS
# Real Estate Loans, All Commercial Banks - REALLN
df = data.DataReader(['DPCERD3Q086SBEA','W019RCQ027SBEA','W079RCQ027SBEA','GPDIC1','NETEXP','GDP','CRDQUSAPABIS','REALLN'], 'fred', start, end)
df.columns = ['consump','gov1exp','gov2exp', 'invest','netexp','gdp','nonfinloan','constructloan']
df.to_csv('crowd.csv')

#Total Borrowings of Depository Institutions from the Federal Reserve
#BORROW
#Central Bank Assets to GDP for Japan
#DDDI06JPA156NWDB
#Gross Domestic Product for Japan
#JPNNGDP

import pandas as pd, datetime
from pandas_datareader import data
start=datetime.datetime(1970, 1, 1)
end=datetime.datetime(2017, 1, 1)
df = data.DataReader(['BORROW','DDDI06JPA156NWDB','JPNNGDP','EXJPUS'], 'fred', start, end)
df.columns = ['us','DDDI06JPA156NWDB','JPNNGDP','curr']
df.to_csv('curr.csv')

# http://wwe.economagic.com/em-cgi/data.exe/bjap/hms11
# get this by copy and paste, into bojliq.csv
