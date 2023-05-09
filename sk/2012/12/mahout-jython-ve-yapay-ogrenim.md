# Mahout, Jython ve Yapay Ogrenim

Bir diger unlu yapay ogrenim kutuphanesi Mahout. Kurmak icin su
sayfanin basindaki hareketler takip edilebilir

```
$ apt-get install maven2

$ cd /opt
$ svn co http://svn.apache.org/repos/asf/mahout/trunk
$ mv trunk mahout_trunk
$ ln -s mahout_trunk/ mahout
$ cd mahout$ mvn install
```

Burada Maven denen ve Java dunyasinda pek sevilmeyen bir program
kullaniliyor. Maven, Ant yerine / ustune ona bir paket sistemi
kazandiran bir sistem, mvn install sirasinda bir suru paketin
Internet'ten indirildigini goreceksiniz. Yani Maven paket
baglantilarini takip ederek (guya) bizi bir suru dertten kurtariyor,
gereken her seyi indiriyor, fakat goreceginiz gibi mvn install asamasi
saatler surebilir. Neyse, Mahout kullanabilmek icin bu aci ilaci icmek
gerekecek (!), ve bundan sonra Maven bir daha da kullanilmayacak.

Kurulus bitinca `$HOME` dizini altinda bir `.m2` dizini olusturulur. Maven
tum gerekli jar dosyalarini buraya koymustur. Biz bu jar dosyalarini
Jython'a disaridan kullandirtacagiz. Bunun icin ufak bir bash script
numarasi yeterli. Dizin .m2 seviyesinden baslayarak ozyineli
(recursive) olarak asagi ineriz, ve onumuze cikan her jar dosyasini
alip iki nokta ustuste ile ayirarak CLASSPATH'e ekleriz. Bu noktadan
sonra Jython gerekli tum jar'lara sahip olacaktir.

```
CLASSPATH=`find $HOME/.m2/ -name *.jar`export CLASSPATH=`echo $CLASSPATH | sed 's/jar\s/jar\:/g'`jython test.py
```

Ornek bir Jython Mahout programini altta bulduk

https://gist.github.com/2878249

Yanliz biz bu programda bazi degisiklikler yaptik, ustteki kod icinde
CLASSPATH Python programi "icinde" olusturuluyor, bu pek temiz degil,
biz bu isleri bash script ortaminda yaptik, boylece Python kodu sadece
Mahout ile ilgili isler yapabiliyor. Bizim versiyon test.py altta

```
from datetime import datetime
from org.apache.mahout.common import RandomUtils
from org.apache.mahout.cf.taste.common
import TasteException
from org.apache.mahout.cf.taste.eval import *
from org.apache.mahout.cf.taste.impl.eval import *
from org.apache.mahout.cf.taste.impl.model.file import *
from org.apache.mahout.cf.taste.impl.neighborhood import *
from org.apache.mahout.cf.taste.impl.recommender import GenericUserBasedRecommender
from org.apache.mahout.cf.taste.impl.recommender.slopeone
import SlopeOneRecommender
from org.apache.mahout.cf.taste.impl.similarity import *
from org.apache.mahout.cf.taste.model import *
from org.apache.mahout.cf.taste.neighborhood import *
from org.apache.mahout.cf.taste.recommender import *
from org.apache.mahout.cf.taste.similarity import *
from java.io import *from java.util import *
class GenericRecommenderBuilder(RecommenderBuilder):
    def __init__(self):
        pass
    def buildRecommender(self, model):
        similarity = PearsonCorrelationSimilarity(model)
        neighborhood = NearestNUserNeighborhood(2, similarity, model)
        return GenericUserBasedRecommender(model, neighborhood, similarity)

class SlopeOneRecommenderBuilder(RecommenderBuilder):
    def __init__(self):
        pass    def buildRecommender(self, model):
        similarity = PearsonCorrelationSimilarity(model)
        neighborhood = NearestNUserNeighborhood(2, similarity, model)
        return SlopeOneRecommender(model)def main():
    RandomUtils.useTestSeed()    model = FileDataModel(File("intro.csv"))
    builder = GenericRecommenderBuilder()
    print 'Starting run at %s' % datetime.now()
    for builder in [GenericRecommenderBuilder(), SlopeOneRecommenderBuilder()]:
        print 'Starting evaluations of recommender created using %s at %s...' % (builder, datetime.now())
        for evaluator in [AverageAbsoluteDifferenceRecommenderEvaluator(), RMSRecommenderEvaluator()]:
            print 'Evaluating recommender using %s at %s...' % (evaluator, datetime.now())
            score = evaluator.evaluate(builder, None, model, 0.7, 1.0)
            print 'Score evaluated by %s=%.2f' % (evaluator, score)
    print 'Ending run at %s' % datetime.now()

if __name__ == '__main__':
    main()
```

Bu kod intro.csv adli bir dosyaya ihtiyac duyar. Bu ornek veri

1,101,5.01,102,3.01,103,2.52,101,2.02,102,2.52,103,5.02,104,2.03,101,2.53,104,4.03,105,4.53,107,5.04,101,5.04,103,3.04,104,4.54,106,4.05,101,4.05,102,3.05,103,2.05,104,4.05,105,3.55,106,4.0






