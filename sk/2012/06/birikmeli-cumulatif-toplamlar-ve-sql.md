# Birikmeli (Cumulatif) Toplamlar ve SQL

Mesela dellstore2 tabaninda tum siparisler uzerindeki "aktif
musterileri" bulmak istiyoruz. Aktif musterinin tanimi orders
tablosunda olmaktan ibaret, o zaman tekil (unique) olarak

```
select count(distinct(customerid)) from orders
```

dersek, tum zamanlar icin gerekli rakami bulabiliriz. Fakat ya bize
birikimsel (cumulative) olarak, ay bazinda yavas yavas buyuyen bir
rakam bulmamiz istenseydi? O zaman Postgresql'in OVER (ORDER BY ...)
numarasini kullanmamiz lazim. Dikkat, burada PARTITION komutu yok.

Fakat aklimiza ilk gelen basit kodlamayi yaparsak

```
select 

date_trunc('month',orderdate),

sum(count(distinct(customerid)))
   over (order by date_trunc('month',orderdate))

from orders group by date_trunc('month',orderdate) 
```

dogru sonucu almadigimizi goreceksiniz. Sebep farkli aylardaki
customerid kayitlarinin toplami sisirmesidir. Tekil musteri sayisini
aradigimiz icin bu bize dogru sonucu vermiyor. O zaman her customerid
icin tek bir tarih kullanmamiz gerekir, tercihen musterinin ilk
yaptigi siparisin ay kismi kullanilabilir. Ornek


```
SELECT  OrderDate,SUM(COUNT(DISTINCT customerid)) OVER (ORDER BY OrderDate)FROM
    (
  
 SELECT CustomerID,
        DATE_TRUNC('MONTH', MIN(OrderDate)) AS OrderDate
        FROM
    orders
        GROUP BY CustomerID) AS ordersGROUP BY OrderDate
```

Goruldugu gibi min(..) ile customerid icin mumkun olabilecek en erken
tarih alinmis. Bu kod toplami duzeltecektir.

