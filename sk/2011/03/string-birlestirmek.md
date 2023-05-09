# String Birlestirmek

Python ile String parcalarini biraraya getirmenin birkac yolu
var. Bunlardan en basiti ve ilk akla geleni + isaretini kullanmaktir:

```
s = "aaa" + "-" + "bbb" + "-" + "ccc"
print s
```

Sonuc

```
aaa-bbb-ccc
```

olacak. Diger bir yontem yerine gecen String (subsitution) yontemi. Bu
yontem aslinda C/++ dilinde bilinen sprintf mantigina benziyor. Format
belirleyen bir kisim var, bir de formatin tanimladigi yerlere yeni
degerler parametre olarak gecilen degerler var, ve bu degerler ile
yeni bir String olusturuluyor. Ornek:

```
s = "%s-%s-%s" % ("aaa","bbb","ccc")
```

s ekrana basildiginda ilk ornekle ayni sonucu gorecegiz. Bu kullanimin
birkac avantaji var, String tipini temsil eden %s yerine diger tipler
de kullanilabilir, mesela float'lari temsil eden %f. O zaman hem
String birlestirme hem de tiplere gore formatlama ayni anda
yapilabilecektir.

Ornek:

```
s = "Burada bir float deger var: %f" % (3.43455)
```

Bu ornekte 3.43455 degeri %f yerine koyulmus olacak ve ekranaBurada
bir float deger var: 3.434550basilacak. %f daha sofistike sekilde de
kullanilabilir. Mesela:s = "Burada bir float deger var: %3.2f" %
(3.43455)

Bu formatlamaya gore float degerini noktadan sonra sadece 2 basamak
olacak sekilde ayarladik. O zaman sonuc:Burada bir float degenr var:
3.43 olacaktir.
