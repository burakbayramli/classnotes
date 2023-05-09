# Postgresql ile Yuzdelik (Percentile) Hesabi

Eger bir olcutun dagilimi tek tepeli ve simetrik degilse, ortalama
(mean) ve standart sapma (std deviation) bize uc noktalar (outliers)
ve hakiki ortalama degerler hakkinda iyi sonuclar vermeyebilir. Bu
durumlarda yuzdelikler (percentile) hesabi daha iyidir, ki medyan
kavrami burada devreye girer, medyan 50. yuzdeliktir.

Postgresql ile bu hesabi yapmak icin, mesela dellstore2 verisinde

```
select totalamount, ntile(20) over (order by totalamount) as cume from orders
```

kullanilabilir. Bu ilk basamak, PG analitik ntile .. over komutunu
kullandi. Bu komut tum satirlari totalamount'a gore siralayarak onlari
20 yuzdelikten birine atayacak.

Eger mesela 95. yuzdeligin en buyuk degerini istersek (ki mesela bu
degeri bir uc nokta olarak kabul etmeyi secebiliriz, ve bu degerden
buyuk her degeri gurultu, asiri buyuk deger, vs. gibi kabul
edebiliriz), o zaman

```
select max(totalamount) AS max_var
from(  select totalamount, ntile(20) over (order by totalamount)
as cume from orders) as tmp where cume = 19
```

kullanilabilir. Ustteki sorgu 410.41 dondurur. Eger "ortalamadan iki
standart sapma uzagi" hesabini yaparsak (ki bu hesabinda uc nokta
hesabi oldugu kabul edilir), o zaman

```
select avg(totalamount)+2*stddev(totalamount) from orders
```

ve

464.19

degeri gelecek. Goruldugu gibi arada buyuk fark var.

