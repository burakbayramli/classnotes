import Quandl, os

fname = '%s/.quandl' % os.environ['HOME']
if not os.path.isfile(fname):
    print 'Lutfen %s dizininde bir .quandl dosyasi yaratin, icine Quandl API anahtari olmali' % fname
    exit()
auth = open(fname).read()

wti = Quandl.get("CBOE/VXK2015", returns="pandas",authtoken=auth)
wti.to_csv("vixmay.csv")
wti = Quandl.get("CBOE/VXM2015", returns="pandas",authtoken=auth)
wti.to_csv("vixjune.csv")
wti = Quandl.get("CBOE/VXN2015", returns="pandas",authtoken=auth)
wti.to_csv("vixjuly.csv")


#Jan F Jul N
#Feb G Aug Q
#Mar H Sep U
#Apr J Oct V
#May K Nov X
#Jun M Dec Z

# https://www.quandl.com/collections/futures/cboe-s-and-p-500-vix-futures
# historical contracts bolumu
