import datetime
import pandas as pd, zipfile
import numpy as np, pickle

dateparse = lambda x: pd.datetime.strptime(x, '%m/%d/%y %H:%M').date()
with zipfile.ZipFile('/home/burak/Documents/Dropbox/Public/data/retail.zip', 'r') as z:
    df =  pd.read_csv(z.open('online_retail.csv'),parse_dates=['InvoiceDate'],date_parser=dateparse)
    dfc =  pd.read_csv(z.open('online_retail_side_data_extended.csv'),index_col='CustomerID')
print df[df.CustomerID == 15065][['Description','InvoiceDate']].tail(10)

recs = []
#cid = 15065

min_date = df.InvoiceDate.min().strftime('%d-%m-%Y')
min_date = datetime.datetime.strptime(min_date, '%d-%m-%Y')
max_date = df.InvoiceDate.max().strftime('%d-%m-%Y')
max_date = datetime.datetime.strptime(max_date, '%d-%m-%Y')

for cid in dfc.index:
    try: 
        res = []
        print cid
        cdates = df[df.CustomerID == cid].InvoiceDate.dt.strftime('%d-%m-%Y').unique()
        for c1,c2 in zip(cdates,cdates[1:]):
            #print c1,c2
            c1 = datetime.datetime.strptime(c1, '%d-%m-%Y')
            c2 = datetime.datetime.strptime(c2, '%d-%m-%Y')
            res.append([c1, 0])
            res.append([c1 + datetime.timedelta(days=1),(c2-c1).days-1])

        #res.append([c2 + datetime.timedelta(days=1),(max_date-c2).days-1])
        res.append([max_date,0.0])

        df2 = pd.DataFrame(res,columns=['dt1','tte'])
        d1 = df.InvoiceDate.min().strftime('%d-%m-%Y')
        d2 = df.InvoiceDate.max().strftime('%d-%m-%Y')
        d1 = datetime.datetime.strptime(d1, '%d-%m-%Y')
        d2 = datetime.datetime.strptime(d2, '%d-%m-%Y')
        #print d1, d2
        all_dates = [d1 + datetime.timedelta(days=x) for x in range((d2-d1).days + 1)]
        df3 = pd.DataFrame(index=all_dates)
        df3['tte'] = df2.set_index('dt1')['tte']
        df3['tte'] = df3['tte'].interpolate(method='linear')

        last = datetime.datetime.strptime(cdates[-1], '%d-%m-%Y')
        df4 = df3.copy()
        df4 = df4[df4.index > cdates[0]]
        df4['censor'] = 1.0
        df4.loc[df4.index > last,'censor'] = 0.0
        df4['change'] = (df4['tte'].diff() > 0).astype(float)
        df4['age'] = dfc.ix[cid]['Age']
        df4['gender'] = dfc.ix[cid]['Gender']
        df4['gender'] = (df4['gender']=='Male').astype(float)        
        if not np.any(pd.isnull(df4)):
            recs.append(np.array(df4))
        #break
        #if len(recs)>200: break
        #if len(recs)>500: break
    except:
         print 'error'
    #break    

output = open('recs.pkl', 'wb')
pickle.dump(recs, output)
output.close()

