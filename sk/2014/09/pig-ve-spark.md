# Pig ve Spark

Apache Spark Buyuk Veri dunyasinda Hadoop'un yerine gecmeye
talip. Spark Hadoop'un disk bazli esle / indirge mimarisi yerine
hafiza bazli transform ve aksiyon mimarisi getiriyor ve bu urune en
son ek Hadoop'ta iyi bilinen Pig dilinin aktarilmasi [link].

Bu buyuk bir haber: Pig dilini isledik, bu dil mesela birlestirim
(JOIN) yapilmasini saglayan komutlara sahipti, Hadoop versiyonu
diyelim ki JOIN komutunu aldiginda bunu arka planda pek cok makinaya
esle / indirge progamciklari olarak tercume ediyordu. Bu sayede 100
GB'lik bir dosyayi bir diger 100 GB'lik dosyaya birlestirebiliyorsunuz
(bu islemi tek makinalik RDBMS uzerinde yapmanin ne kadar zor oldugunu
tahmin edebiliriz). Buyuk Veri dunyasi genelde denormalize edilmis
(surekli tekrar edilen) veri ile is yapsa da bazen JOIN'e ihtiyac
olur.

Yani simdi ayni dili (Pig) kullanarak arka planda Spark'a is
yaptirmak, Spark'a Pig sonuclarini aktarip, ya da oradan veri almak
kolay olacak. 

Pig Spark surumu daha cok yeni, su anda Spork adi altinda gelismeye
devam ediyor

https://github.com/sigmoidanalytics/spork/







