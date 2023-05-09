# Hatali Tarih Saptamak, Java

Eger SimpleDateFormat'i tarih bilgisini String tipinden Date tipine
gecmek icin kullaniyorsaniz dikkat: Bu class, eger ozellikle
belirtilmezse, herhangi bir gun ve ay bilgisini mutlu bir sekilde
(kafasina gore) bir Date'e cevirebiliyor. Hicbir kontrol
yapmiyor.

Mesela 40-2-2008 tarihi 10 Mart 2008'e donusebiliyor!Bu hatadan
kacinmak icin setLenient(false) cagrisi yapmaniz gerekli. Bizim bu isi
yapan DateUtil class'imizdaki metot suna benziyor.

```
public static Date DMYtoDate(int day, int mon, int year)
{
  SimpleDateFormat fmt = new SimpleDateFormat("dd-MM-yyyy");
  String ds = day + "-" + mon + "-" + year ;
  Date date = null;
  try {
    fmt.setLenient(false); date = fmt.parse(ds);
  } catch (java.text.ParseException e) {
     log.error(e); return null;
  }
 return date;
}
```

Bu metot, eger hatali bir tarih varsa, null dondurecektir.





