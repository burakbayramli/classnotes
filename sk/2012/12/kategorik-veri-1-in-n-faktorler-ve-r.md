# Kategorik Veri, 1-in-n, Faktorler, R ve Patsy

R dilini kullanarak kategorik veri iceren kolonlarin 1-in-n seklinde
yana dogru "genisletilerek" 1/0 iceren hale cevirilebildiginden
bahsetmistik. Fakat R bu degismis hali disa yansitmaz, lm(), nnet()
gibi komutlar ic isleyisleri sirasinda bu degisimi kullanirlar, lm()
sonuc raporunde bu yeni kolonlari gorebilirsiniz mesela, ama veri
disari cikmaz.

Eger bu veriyi mesela matris formatinda istiyorsaniz, su komut
dizisini kullanabilirsiniz (dellstore2 ornegindeki season ve
categoryname kolonlari icin)

```
orders <- read.csv ("data.csv",header=TRUE,sep="|")orders <- cbind(orders, model.matrix( ~ 0 + season + categoryname, orders))orders <- orders[, setdiff(names(orders), c("season", "categoryname"))]write.csv (orders,"out.csv")
```

Sondan ikinci satir lazim cunku genisletilmis kolonlar eklense bile
orijinal kolon hala orders icinde tutuluyor. Bu kolona artik ihtiyac
yok ve cikartilmasinda bir zarar olmayacak.

Eger ayni islemi Python ile yapmak istersek Patsy adli paket yardimci
olabilir. En son versiyon

http://pypi.python.org/pypi/patsy/

Ayrica Pandas kurmak ta gerekiyor, bunun icin "sudo pip install
pandas" yeterli.

Kullanmak icin mesela

```
from patsy import dmatrix
from pandas import *
orders = read_csv("dell.csv",sep=",")
matrix = dmatrix("0 + month + netamount + gender + season + cat_freq + creditcardtype",orders)
print matrix.design_info
print matrix[0,:]
```

Ust son iki satirda matrisin ic yapisini (yeni kolon isimlerini gormek
icin) ekrana bastirdik, ayrica matrisin ilk satirini ornek olarak
gosterttik.

Sonuc

```
DesignInfo(['gender[F]', 'gender[M]', 'season[T.SPRING]', 'season[T.SUMMER]', 'season[T.WINTER]', 'creditcardtype[T.c2]', 'creditcardtype[T.c3]', 'creditcardtype[T.c4]', 'creditcardtype[T.c5]', 'month', 'netamount', 'cat_freq'],  ....

[  0.     1.     0.     0.     0.     0.     0.     0.     0.    11.     5.08   1.  ]
```

gibi bir cikti goreceksiniz, yani faktor kolonlari 1-in-n formatina
cevirilerek 1/0 degeri tasiyacak sekilde "yana dogru"
genisletilmistir. Yeni kolon isimleri mesela season[T.SPRING] gibi,
yaz mevsimi icin apayri bir kolon vardir,ve ona tekabul eden 0/1
degeri olacaktir.

Bu yeni matrisle istediginiz yapay ogrenim rutinini
cagirabilirsiniz. Matris Numpy formatiyla uyumludur. 







