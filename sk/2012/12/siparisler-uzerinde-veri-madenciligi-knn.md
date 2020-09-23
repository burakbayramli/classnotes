# Siparisler Uzerinde Veri Madenciligi - kNN


Siparisler Uzerinde Veri Madenciligi - kNN




Daha once regresyon ile atillik tahmini yapmistik (lineer, ve GLM ile). Bu yazida En Yakin k-Komsu (k-Nearest Neighbor -kNN-) teknigini kullanacagiz.

kNN metodu veriden model cikartmaz, verinin kendisini model olarak kullanir. Bu sebeple ornek bazli metotlardan (instance based methods) bilinir. Temeli basit; model kurmadan etiketleri bilinen veriyi bir kenarda tutariz sonra etiketsiz yeni veri noktasi gelince bu veriye donup bakariz, eldeki veriye "en yakin"  k-tane ornegi buluruz ve bu k ornek icinde en fazla hangi etiket var ise, yeni  etiketin bu olacagina karar verilir. "En yakinlik" sozu bir kordinat sistemi / uzaklik kavramini cagristirir, ve evet kNN numerik degerlerle bir kordinate sisteminde islem yapar, bu acidan k-means'e (ve pek cok diger yapay ogrenim metotuna) benzedigi soylenebilir. Regresyon ornegindeki gibi  gibi kategorik degerler var ise bunlar sayisal hale cevirilmelidir.

kNN'in detaylarina ileride girecegiz, simdilik dellstore2 uzerindeki ornegi gorelim.

from sklearn import neighborsfrom patsy import dmatriximport numpy as npfrom pandas import *__day__ = 130cols = """0 + month + day + day_of_week + rank + categoryname + income + season+ cat_freq + creditcardtype"""orders = read_csv("dell-train.csv",sep=",")train = dmatrix(cols,orders)last_value = orders[['last_visit']].as_matrix()[:,0] > __day__print len(last_value[last_value==True])print len(last_value[last_value==False])knn = neighbors.KNeighborsClassifier(n_neighbors=10)knn.fit(train,last_value)orders = read_csv("dell-validate.csv",sep=",")validate = dmatrix(cols,orders)last_value = orders[['last_visit']].as_matrix()[:,0] > __day__pred = knn.predict(validate)success = sum(pred[last_value==True] == 1) + \          sum(pred[last_value==False] == 0)print success / float(len(pred)) * 100

Burada kNN'e bir 0/1 tahmini yaptirdik, bu etiketleri atillik sayisindan urettik, 130 gunden buyuk atillik degeri 1, kucukleri 0 olsun dedik. Etiket sayisinda bir sinirlama yok (SVM'in aksine), bu ornekte 2 tane kullandik, daha fazla da olabilirdi. Bu arada kNN regresyon amacli da kullanilabilir, k yakin komsu bir etiket oyu verecegine, belli agirliklarla bir ortalama sayi da uretebilir.

Kullandigimiz kNN paketi  scikits-learn Python kutuphanesi. Kategorik degerleri sayisallastirmak  icin Pasty kullanildi.  

Ustteki kodun basari orani yuzde 86'dir. Gercek dunya verisinde yuzde 85 degerini gorduk (atillik esik degeri ne olursa olsun basari orani ayni -biz 53 gun kullandik-). Metot kuvvetli.





