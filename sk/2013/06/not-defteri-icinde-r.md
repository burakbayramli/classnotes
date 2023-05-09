# Not Defteri Icinde R

iPython not defterleri icinde R kodu bile kullanmak mumkun. Bazi puf
noktalar / uzak durulmasi gereken seyler: Mesela read.csv metoduna ek
parametre (mesela ayrac -seperator-) gecilmesi not defterini rahatsiz
edebiliyor, o zaman olagan (default) ayrac hep kullanilabilir, ki bu
virgul isareti.

Ornek bir kullanim asagida

```
In [1]:%load_ext rmagic

In [31]:%R -n -d myData myData <- read.csv('dosya.csv')

In [*]:%%Rlibrary(rpart)tree <- rpart(y ~ kolon1 + kolon2, method="class", data=myData)

In [ ]:%%Rlibrary(rpart.plot)

rpart.plot(tree,type=4, extra=1)
```

Magic (buyu) kullanimi not defterlerinin pek cok ek ozelligi
cagirabilmesini sagliyor, su anda sql icin bile bir magic eklemesi var
mesela.

iPython ortami hakikaten bilim, veri analizi, vb. kullanimlar icin
ideal bir ortam haline geliyor.



