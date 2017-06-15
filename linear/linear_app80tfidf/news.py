from scipy.io import mmwrite, mmread
import urllib2, datetime, pickle, os
import pandas as pd, collections
from scipy.io import mmwrite, mmread
from statsmodels.regression.linear_model import OLS
from sklearn.preprocessing import normalize
import urllib2, datetime, pickle
import pandas as pd, re
from sklearn import linear_model
import scipy.sparse as sps
import numpy as np

nfile = '../../../sabah.pkl'
gtd = '../../../gtd_0615dist.csv'
terr = '../../../terrn.csv'
tffile = '../../../tfidf.mtx'
colfile = '../../../cols.csv'

skip = ['br','t','td','tr','haber','reklam','new','here','&&','appname','jpg',
        'err','end','description','sans-serif','img','return','navy','navigator',
        'top','mm_returnvalue','com','catch','bname','if','cgi?www','the','left',
        'www','right','true','noshade','!--','table','description;','showstatus',
        'http','center','function','helvetica','https','window','sabah','100%',
        'strong','arial',';return','target','maxsize','sans-serif;','stylesheet',
        'gif','imgrestore','middle','html','--'
]

def newspaper():
    start_date = pd.to_datetime("20000101", format='%Y%m%d')
    news = collections.OrderedDict()
    if os.path.isfile(nfile): 
        p = open(nfile, 'rb')
        news = pickle.load(p)
        start_date = pd.to_datetime(news.keys()[-1], format='%Y/%m/%d')    
    end_date = pd.to_datetime("20160101", format='%Y%m%d')
    delta = end_date - start_date
    dates = []
    for i in range(delta.days + 1): dates.append(start_date + datetime.timedelta(days=i))    
    for i,d in enumerate(dates):
        tries = 0
        d = d.strftime('%Y/%m/%d')
        while tries < 8:
            print d
            try: 
                url = "http://arsiv.sabah.com.tr/%s/" % d                
                html1 = urllib2.urlopen(url).read()
                news[d] = html1
                break
            except:
                print 'conn failure... try again'
                tries += 1
                continue
            
        if i % 120 == 0:
            print 'checkpoint save'
            output = open(nfile, 'wb')
            pickle.dump(news, output)
            output.close()    

    output = open(nfile, 'wb')
    pickle.dump(news, output)
    output.close()
    
def tokenize():
    cols = pd.Series()
    p = open(nfile, 'rb')
    tmp = pickle.load(p)
    D = 12000; N = len(tmp)
    A = sps.lil_matrix((N,D))
    for i,d in enumerate(tmp.keys()):
        print d
        s = tmp[d].decode('latin5').replace("'"," ").replace(","," ").replace('"',' ')
        for col in re.split('\s|>|<|,|\.|\(|\)|:', s):
            if col == "" or "=" in col: continue
            col = col.encode('latin5').lower()
            if  len(col)==1 or "/" in col or '#' in col: continue
            if col in skip: continue
            h = hash(col) % D
            if h not in cols.index: cols.loc[h] = {}
            if cols.ix[h].get(col)==None: cols.ix[h][col] = "1"
            A[i,h] += 1
        #if i==20: break
        
    A[A > 0] = 1.
    idf = A.sum(axis=0)
    idf[idf.nonzero()] = np.log(N/idf[idf.nonzero()])
    tf = A.tocoo()
    tf.data = 1 + np.log(tf.data)
    tfidf = sps.csr_matrix(tf.multiply(idf))
    tfidf = normalize(tfidf, norm='l2', axis=1)
    mmwrite(tffile, tfidf)

    cols.to_csv(colfile)
    
def terrnk():
    df = pd.read_csv(gtd,sep=';')
    import datetime
    def f(x):
        try: return datetime.date(x['iyear'], x['imonth'], x['iday'])
        except: return 0
    df2 = df[df.country_txt == 'Turkey']
    filt = pd.isnull(df2.nkill)==False
    df2.loc[filt,'nkill'] = df2[filt].nkill.map(lambda x: str(x).split(",")[0])
    df2.loc[filt,'nwound'] = df2[filt].nwound.map(lambda x: str(x).split(",")[0])
    df2.loc[:,'cdate'] = df2.apply(f, axis=1)
    df2 = df2[df2['cdate'] != 0]
    df2 = df2.set_index('cdate')
    df2 = df2[['nkill','nwound']]
    df2['nkill'] = df2.nkill.astype(float)
    df2['nwound'] = df2.nwound.astype(float)
    df2['damage']  = df2.nkill*2 + df2.nwound
    df2 = df2.drop_duplicates()
    df2['damage'].to_csv(terr)

def regr(shifts=[0]):
    p = open(nfile, 'rb')
    news = pickle.load(p)
    cols = pd.read_csv(colfile,index_col=0,header=None)
    df = pd.read_csv(terr,index_col=0,parse_dates=True)    
    df2 = pd.DataFrame(pd.to_datetime(news.keys(), format='%Y/%m/%d'))
    
    df2 = df2.reset_index().set_index(0)
    df3 = df2.join(df)
    df3.columns = ['idx','damage']
    df3['damage'] = df3.damage.fillna(method='ffill')
    tfidf = mmread(tffile)
    X = tfidf.tolil()
    X = X[df3.idx,:]
    for s in shifts:
        print 'regress', s
        lin = linear_model.LinearRegression()
        y = df3.shift(s).damage.fillna(0)
        res = lin.fit(X, y)
        print "R^2", lin.score(X, y)
        res2 = pd.DataFrame(res.coef_)
        res2 = res2[res2 > 0].dropna().sort_values(by=0,ascending=False)
        for i in res2.head(10).index: print cols.ix[i]


def test():
    html = {}
    html['2001/01/01'] = "k lkajsdaksdf aksdf kakskf jalksdf news lkalsdf"
    html['2001/01/02'] = "k lkajsdaksdf aksdf kakskf jalksdf news lkalsdf"
    html['2001/01/03'] = "k lkajsdaksdf aksdf kakskf jalksdf news lkalsdf"
    html['2001/01/04'] = "k lkajsdaksdf cicek filan jalksdf news lkalsdf"
    html['2001/01/05'] = "k cicek aksdf kakskf jalksdf news lkalsdf"
    html['2001/01/06'] = "k aslan aksdf kakskf jalksdf news lkalsdf"
    html['2001/01/07'] = "k lkajsdaksdf bocek kakskf jalksdf news lkalsdf"
    html['2001/01/08'] = "k lkajsdaksdf aksdf kakskf jalksdf  lkalsdf"
    html['2001/01/09'] = "k cicek aksdf kakskf jalksdf news lkalsdf"
    html['2001/01/10'] = "k lkajsdaksdf bocek kakskf jalksdf news lkalsdf"
    html['2001/01/11'] = "k lkajsdaksdf aksdf kakskf jalksdf news lkalsdf"
    output = open(nfile, 'wb')
    pickle.dump(html, output)
    output.close()    
    tokenize()
    evs = np.array([1,2,1,10,15,1,3,1,20,1,1])
    ds = pd.to_datetime(html.keys(), format="%Y/%m/%d")
    evs = pd.DataFrame(evs, index=ds)
    evs.columns = ['damage']
    evs.to_csv(terr)
    regr()
    
if __name__ == "__main__": 
    #newspaper()
    #terrnk()
    #tokenize()
    regr([20,40,60])
    #test()
