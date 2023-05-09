# Postgres First_Value, Sum, Partition

Su yazida PG tabaninin bir kolondaki degerin ayniligini temel alarak o
grup icinde / uzerinde islemler yaptigini gosterdik. Bu gruba PG bir
"pencere (window)" adini veriyor. Klasik GROUP BY islemi satirlari
azaltir, ozetler, pencere bazli OVER .. PARTITION komutu ise satirlari
oldugu gibi birakip onlara ek bilgi eklenebilmesini saglar. Akla
gelebilecek her seyi bir pencereye yayabilirsiniz. min teknigini
gorduk, ama su da mumkun,

```
select 
o.orderid, 
o.customerid,
o.totalamount, 
sum(o.totalamount) over (partition by o.customerid ) as sum,
o.orderdate
from orders o
order by customerid
```

Burada ayni musterinin (aynilik kriteri customerid) siparis tutarlari toplaniyor ve bu musterinin tum satirlarina yayiliyor. Ustteki sorguyu isletin, ve id = 13 icin


```
 orderid | customerid | totalamount |  sum   | orderdate  
---------+------------+-------------+--------+------------
     379 |         13 |      246.21 | 336.39 | 2004-01-14
    9447 |         13 |       90.18 | 336.39 | 2004-10-10
```

sonucunu goreceksiniz. Toplam tekrarlanmis.Satirlarin toplami yerine,
aynen ilk siparis tarihi ornegine benzer sekilde "o grubun ilk
satirindaki herhangi bir kolonun degerini" de yayabiliriz. Bunun icin
`first_value()` cagrisi `OVER .. PARTITION` ile beraber kullanilir,

```
select o.orderid, first_value(o.orderid) over (partition by
o.customerid order by o.orderdate) as
first_order_id,o.customerid,o.totalamountfrom orders oorder by
customerid
```

Sonuc

```
 orderid | first_order_id | customerid | totalamount 
---------+----------------+------------+-------------
     379 |            379 |         13 |      246.21
    9447 |            379 |         13 |       90.18
```

Musteri 13'un ilk siparisi id 379, sonraki 9447. Biz first_value ile
379'u aldik, first_order_id uzerinden otekine "yaydik". first_value
cagrisina kolon degeri verilebildigi gibi depolu islem (stored
procedure) cagrisi da verilebilir.

Boyle islemler nerede ise yarar? Mesela musterinin her sipariste hangi
(belki degisik bir) kanaldan geldigini biliyoruz, Web kanalindan mi,
Internet kanalindan mi, vs. Fakat raporlama amacli olarak bu
musterinin bize getirdigi tum kazanci "ilk geldigi kanalin" hanesine
yazmak istiyoruz. Bu gibi durumlarda bu ilk kanal bilgisini
"cogaltarak" o musterinin tum diger kayitlarina yaymak,
over.. partition ile mumkun olur.

Sonra SQL'in geri kalan mekanizmalari oldugu gibi calisirlar, GROUP BY
mesela bu yeni yaratilan kolonu sanki gercek bir kolonmus gibi
kullanabilmeye baslar. SQL kumesel olarak dusunur, ve her satirin bu
sebeple atomik olmasi iyidir, satirlarin kendi hakkindaki bilgiyi
kendi uzerinde tasimasi lazimdir -- OVER.. PARTITION ile gereken bu
tur ek bilgileri satirlara ekleyebilmis oluruz.

