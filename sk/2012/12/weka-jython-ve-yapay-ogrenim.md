# Weka, Jython ve Yapay Ogrenim


Weka, Jython ve Yapay Ogrenim




Yapay ogrenim (machine learning) paketlerinden / yazilimlarindan unlu
 birisi Weka. Paket icinde karar agaclari, SVM gibi bilinen ML 
algoritmalari var; Weka Java ile yazilmistir fakat Jython uzerinden 
Python ile erisim mumkundur. 

Kurmak icin site

http://www.cs.waikato.ac.nz/ml/weka/

Zip acilir ve kurulum bu kadar. Su baglantida Jython ornegi var

http://weka.wikispaces.com/Using+WEKA+from+Jython

Jython kullanmak icin Ubuntu uzerinde apt-get install jython. Sonra bir script yazin, mesela iris.sh, 

export CLASSPATH=$CLASSPATH:[WEKA_DIZIN]/weka-3-6-8/weka.jarjython iris1.py

Kodunuz iris1.py icinde olacak, ve suna benzeyecek (ustteki siteden alindi)

import sysimport weka.classifiers.trees.J48 as J48import java.io.FileReader as FileReaderimport weka.core.Instances as Instancesimport weka.core.converters.CSVLoader as CSVLoaderimport weka.classifiers.trees.J48 as J48file = FileReader("[WEKA_DIZINI]/weka-3-6-8/data/iris.arff")data = Instances(file)data.setClassIndex(data.numAttributes() - 1)j48 = J48()j48.buildClassifier(data)print j48

Bu kodu iris.sh ile isletin. Soyle bir sonuc gelecek

petalwidth <= 0.6: Iris-setosa (50.0)petalwidth > 0.6|   petalwidth <= 1.7|   |   petallength <= 4.9: Iris-versicolor (48.0/1.0)|   |   petallength > 4.9|   |   |   petalwidth <= 1.5: Iris-virginica (3.0)|   |   |   petalwidth > 1.5: Iris-versicolor (3.0/1.0)|   petalwidth > 1.7: Iris-virginica (46.0/1.0)Number of Leaves  :     5Size of the tree :     9

Iris veri seti ML'de iyi bilinen veri setlerinden, ve ustteki kod ile J48 algoritmasi ile bu veri uzerinde bir karar agaci olusturduk.

Fakat
 pratikte bir problem var, is dunyasinda cogunlukla veriyi ARFF 
formatinda bulmayiz, cogunlukla CSV formati karsimiza cikiyor. Dert 
degil, Weka'nin CSV okuyucusu da var, simdi Iris verisini CSV'den okuyan 
ornek gorelim.

import sysimport weka.classifiers.trees.J48 as J48import java.io.FileReader as FileReaderimport java.io.File as Fileimport weka.core.Instances as Instancesimport weka.core.converters.CSVLoader as CSVLoaderimport weka.classifiers.trees.J48 as J48file = File("/bir/dizin/iris.csv")loader = CSVLoader()loader.setSource(file)data = loader.getDataSet()data.setClassIndex(data.numAttributes() - 1)j48 = J48()j48.buildClassifier(data)print j48

Ustteki kod benzer bir cikti verecek.

Veri dosyasi iris.csv'i Internet'ten bulabilirsiniz, bir ornegi surada mesela





