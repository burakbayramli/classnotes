import Quandl, os

fname = '%s/.quandl' % os.environ['HOME']
if not os.path.isfile(fname):
    print 'Lutfen %s dizininde bir .quandl dosyasi yaratin, icine Quandl API anahtari olmali' % fname
    exit()
auth = open(fname).read()

wti = Quandl.get("FRED/DTB3", returns="pandas",authtoken=auth)
wti.to_csv("treas3M.csv")
wti = Quandl.get("FRED/DTB6", returns="pandas",authtoken=auth)
wti.to_csv("treas6M.csv")
wti = Quandl.get("FRED/DTB1YR", returns="pandas",authtoken=auth)
wti.to_csv("treas1Y.csv")
wti = Quandl.get("FRED/DGS5", returns="pandas",authtoken=auth)
wti.to_csv("treas5Y.csv")
wti = Quandl.get("FRED/DGS10", returns="pandas",authtoken=auth)
wti.to_csv("treas10Y.csv")
wti = Quandl.get("FRED/DGS30", returns="pandas",authtoken=auth)
wti.to_csv("treas30Y.csv")


# https://www.quandl.com/collections/usa/usa-interest-rates
