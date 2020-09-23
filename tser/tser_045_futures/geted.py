import Quandl, os

fname = '%s/.quandl' % os.environ['HOME']
if not os.path.isfile(fname):
    print 'Lutfen %s dizininde bir .quandl dosyasi yaratin, icine Quandl API anahtari olmali' % fname
    exit()
auth = open(fname).read()

wti = Quandl.get("CME/EMK2015", returns="pandas",authtoken=auth)
wti.to_csv("edk.csv")
wti = Quandl.get("CME/EMU2015", returns="pandas",authtoken=auth)
wti.to_csv("edu.csv")

#Jan F Jul N
#Feb G Aug Q
#Mar H Sep U
#Apr J Oct V
#May K Nov X
#Jun M Dec Z

# https://www.quandl.com/collections/futures/cme-1-month-eurodollar-futures
