# MySQL ve CSV

Komut su sekilde

```
mysql -e 'SELECT * FROM [tablo]' --user=[kullanici] --password=[sifre]
--host=[makina] > /bir/dizin/dosya.csv
```

Secenek -e sonrasi sorgu SQL'in izin verdigi herhangi bir sorgu
olabilir, yani ek WHERE sartlari burada tanimlanabilir. Eger tum tablo
alinacaksa ustteki format yeterli. Ustteki komut bir shell script
icinden isletilebilir. Ubuntu uzerinde sudo apt-get install
mysql-client ile en azindan MySQL baglanti programlari kurulmus
olmali.






