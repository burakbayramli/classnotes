# Postgres Min, Partition

PG tabaninin bir kolonu temel alarak partition etme ozelligi var. Bu
komut, secilen kolonun tekrar ettigi satirlar uzerinden / icinde /
arasinda islemler yapmanizi sagliyor. Mesela dellstore tabanini ornek
alirsak (onceki yazimiz) bu tabanda orders var, ve musteri numarasi
tabii ki belli satirlarda tekrar ediyor, cunku ayni musteri gelip
tekrar alisveris yapmis (istenen bir sey degil mi?). Partition
ozelligini kullanarak, tum satirlara bakarken ek bir bilgi daha
ekleyebiliriz. Mesela ayni customerid uzerinden en az orderdate'i
cekip cikartmak ve bu en az tarihi ayni musteri icin tum satirlara
yaymak.

```
select customerid,min(o.orderdate) over (partition by o.customerid ) as
reg_date
from orders o
order by customerid
```

```
customerid | reg_date | orderdate
------------+------------+------------ ...
40 | 2004-07-06 | 2004-07-06
41 | 2004-06-02 | 2004-06-02
41 | 2004-06-02 | 2004-08-09
41 | 2004-06-02 | 2004-10-15
42 | 2004-12-23 | 2004-12-23
43 | 2004-05-19 | 2004-05-19
44 | 2004-03-18 | 2004-03-18
44 | 2004-03-18 | 2004-12-08
```

Dikkat edelim, id 41 tekrar ediyor, ve reg_date yani bu musterinin ilk
satin alim yaptigi tarih 2004-06-02 id 41 icin tekrar edilmis.Boylece
bir ek bilgi, ek kolon yaratmis olduk. Bu bilgiyi nasil
kullanabiliriz? Mesela bir "musteri sadakati" sorgusu dusunulebilir,
belli bir tarih arali icin "kac eski musteri bu aralikta alim yapmis"
diye bir soru sorulabilir. Eski musterinin tanimi reg_date'i o
araliktan once olanlar ve orderdate'i aralik icinde olan kisiler
olabilir.Ilginc bir ust sorgu soyle olabilir. Mesela ilk alim
(reg_date) ve mevcut alim (orderdate) bilgisinin sadece ay ve yil
kismi uzerinden bir GROUP BY yaparsak, o zaman tum ilk alim, mevcut
alim tarih kombinasyonlarini goruruz.

```
select extract (month from reg_date) as month,extract (year from
reg_date) as year,extract (month from odate),extract (year from
odate),count(distinct(customerid))
from (select customerid,min(o.orderdate)
over (partition by o.customerid ) as reg_date,orderdate as odate from
orders o) as sub1group byextract (month from reg_date),extract (year
from reg_date),extract (month from odate),extract (year from
odate)
```

```
reg_month | reg_year | odate_month | odate_year | count
-----------+----------+-------------+------------+-------
1 | 2004 | 1 | 2004 | 979
1 | 2004 | 2 | 2004 | 43
1 | 2004 | 3 | 2004 | 46
1 | 2004 | 4 | 2004 | 54
1 | 2004 | 5 | 2004 | 43
```

Bu kombinasyon ciktisi uzerinden ek sorularimiza cevap
bulabiliriz. Mesela bakiyoruz, 2004 Ocak ayinda ilk alimini yapan ve
sonra 2004 Subat ayinda alim yapan 43 kisi varmis, vs.Ay bazinda
gruplamanin bir diger yolu tarih kolonunda ay oncesi tum bilgiyi ayni
seye esitlemek, ay kalacak sekilde "yoketmek (truncate)". Bu teknik
altta, bazi durumlarda daha kullanisli bulunabilir.

```
select cast (date_trunc('month',reg_date) as date) as reg_date,cast
(date_trunc('month',odate) as date) as
cdate,count(distinct(customerid)) as
customer_countfrom(selectcustomerid,min(o.orderdate) over (partition
by o.customerid ) as reg_date,orderdate as odatefrom orders o) as
sub1group bydate_trunc('month',reg_date),date_trunc('month',odate)
```
