import pandas as pd, datetime, os
from pandas_datareader import data

#####
start=datetime.datetime(1970, 1, 1)
end=datetime.datetime(2018, 5, 1)
df = data.DataReader(['CRDQUSAPABIS','REALLN','CRDQJPAPABIS','JPNRGDPEXP','GDPC1','EXJPUS','MYAGM2USM052S','MYAGM2USM052S'], 'fred', start, end)
df.columns = ['nonfinloanus','constructloanus','nonfinloanjp','realgdpjp','realgdpus','xjpus','m2us','m2jp']
df.to_csv('exch.csv')
    
#####
start=datetime.datetime(1970, 1, 1)
end=datetime.datetime(2017, 1, 1)
df = data.DataReader(['GDP','IR3TIB01USM156N'], 'fred', start, end)
df.columns = ['gdp','shortrate']
df.to_csv('rates.csv')

#####
start=datetime.datetime(1950, 1, 1)
end=datetime.datetime(2017, 1, 1)
df = data.DataReader(['M2SL','GDP','CRDQUSAPABIS','REALLN','FPCPITOTLZGUSA','FEDFUNDS'], 'fred', start, end)
df.columns = ['m2cd','gdp','nonfincred','realest','inflation','irate']
df.to_csv('money.csv')

#####
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

#####
# speculative traders, noncommercial open position
# wget http://www.cftc.gov/files/dea/history/deacot2002.zip
# use year 2012, 2015, etc in the file name
# put them under jpy directory

if os.path.isdir("jpy"):
    res = []    
    for y in range(2002,2018):    
        print y
        with zipfile.ZipFile('cad/deacot%d.zip' % y, 'r') as z:
            df = pd.read_csv(z.open('annual.txt'))     
            df = df[df["Market and Exchange Names"].str.contains("JAPANESE YEN")]
            df = df[["As of Date in Form YYYY-MM-DD","Noncommercial Positions-Long (All)","Noncommercial Positions-Short (All)","Commercial Positions-Long (All)","Commercial Positions-Short (All)"]]
            res.append(df.copy())

    df2 = pd.concat(res)
    print df2
    df2.to_csv('jpypos.csv',index=None)

    
