# Enlemler, Boylamlar ve PostgreSql PostGis

Lokasyon bazli veri isleyebilmek icin Postgresql'in Postgis eklentisi
gerekli. Bu eklenti ile enlem (latitude), boylam (longitude) kordinat
verileri tabanda depolanabilir, dunya uzerinde "geometrik alanlar"
tanimlanarak mesela belli kordinatlarin o alanlarin icine dusup
dusmedigi sorgulanabilir. Ya da bir veya daha fazzla geometrik sekilin
kesisim noktasinin nasil, ne buyuklukte, nerede bir baska bicim
olacagini hesaplatirabilirsiniz. Suradaki

http://trac.osgeo.org/postgis/wiki/UsersWikiPostGIS20Ubuntu1110src 

tarifler takip edilebilir. GDAL ve GEOS'u biz kaynaklardan kurduk. 

```
sudo apt-get install gdal-bin
```

http://trac.osgeo.org/geos/

Sonra

```
sudo -u postgres psql
```

ile girip

```
\c [taban]
```

ile istediginiz tabana baglanin. Unutmayalim, eklentiler (extensions) taban bazli yaratilir. Sonra

```
CREATE EXTENSION postgis;
```

ile gerekli tipi yaratiriz.

```
\dT geography
```

ve

```
\dT geometry
```

ile tiplerin yaratildigini kontrol edin. Bu konu hakkinda ornek kodlar
bu yaziya eklenecek.



