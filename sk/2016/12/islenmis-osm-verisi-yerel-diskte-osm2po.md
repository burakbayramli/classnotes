# İşlenmiş OSM Verisi Yerel Diskte, osm2pgsql, PostGIS

Daha once OSRM projesi ile OSM verisi uzerinden nasil servis
sunabilir, bunu gorduk. Peki ya ham veriyi almak istiyorsak ne
yapariz?

osm2pgsql adli arac apt-get ile kurulabiliyor, yerel diskteki osm.bz2
tipindeki dosyalari isleyip direk Postgresql tabanina veriyi
yazabiliyor. PG kurmak altta. Ek olarak bir de lokasyon modulu nasil
kurulur onu da gorecegiz, bu eklere PostGIS adi veriliyor, PG'nin
lokasyon verisinin islenmesini saglayan ozel fonksiyonlari var.

```
sudo apt-get install postgresql make cmake g++ libboost-dev libboost-system-dev
  libboost-filesystem-dev libexpat1-dev zlib1g-dev
  libbz2-dev libpq-dev libgeos-dev libgeos++-dev libproj-dev lua5.2
  liblua5.2-dev
 osm2pgsql postgresql-9.5-postgis-2.2 pgadmin3 postgresql-contrib-9.5
```

Not: Uzerine oldugumuz Ubuntu versiyonuna gore hangi PG kuruldugu
degisik olabilir, bu durumda 9.5 yerine o versiyon koyulabilir.

Baslamak, durdurmak icin sudo service postgresql stop ya da
start. Kurulus sirasinda mevcut kullanicinizin direk erisebilecegi
postgres adli bir kullanici yaratilmis olmali.

```
sudo -u postgres createdb gis
```

ile taban yaratilir.

```
sudo -i -u postgres
```

ile oteki kullaniciya girilir,

```
psql -d gis -c 'CREATE EXTENSION postgis; CREATE EXTENSION hstore;'
```

Simdi

```
osm2pgsql berlin-latest.osm.bz2 --slim
```

Tabana bakariz, `psql -d gis`, komut satirinda `\dt`

```
planet_osm_line 
planet_osm_nodes
planet_osm_point
planet_osm_polygon
planet_osm_rels 
planet_osm_roads
planet_osm_ways 
```

tablolarini gormemiz lazim. Artik gereken ham veri bu tablolar icinde. Eger veriyi disari cikartmak istiyorsak, mesela tabandaki tum caddeler,

```
psql -d gis -t -A -F";" -c \
     "SELECT  osm_id,name,highway,\
     st_asText(st_startpoint(st_transform(way, 4326))), \
     st_asText(st_endpoint(st_transform(way, 4326))) \
     FROM planet_osm_line " > output.csv
```

Not: Ayni cadde ismi ustteki sorgu sonucu birden fazla satirda ve
baslangic / bitis noktasi ile gozukebilir. Bu durumda buyuk cogunlukla
ayni caddeyi parcalar haline goruyoruz demektir.

PostGIS icin iyi bir kitap PostGIS in Action.

osm2po

Alttaki Java bazli bir proje (kapali kod ne yazik ki), Geofabrik
sitesinden gerekli veriyi alip bir Postgresql tabani uzerinde
isletilebilecek SQL dosyasi uretiyor.

http://osm2po.de

```
java -Xmx1g -jar osm2po-core-5.1.0-signed.jar prefix=tr tileSize=x http://download.geofabrik.de/europe/turkey-latest.osm.pbf postp.0.class=de.cm.osm2po.plugins.postp.PgRoutingWriter
```

cagrisi yeterli. Gerekli dosya tr altdizini icinde olacak. Fakat bu
kutuphanenin bazi verileri disarida biraktigini gorduk.

osm2pgrouting

Ham OSM verisinin Postgresql tabana yazabilen bir proje

https://github.com/pgRouting/osm2pgrouting.

Derlemek gerekli.. Ayrica tabani pgrouting icin hazirlamak lazim, bazi
create extension komutlari gerekiyor. Ustteki README icinde bunlar
var.

Bu kodun bir problemi eger yeterince hafiza yoksa (2 GB bile
yetmeyebilir) tek bir bolge verisi uzerinde bile program cokebiliyor.

Postgresql GUI

Uzun zamandir taban semasini gosteren, sorgu isleten GUI araci olarak Squirrel kullaniyorduk (ondan once Toad). Fakat Squirrel projesinde fazla hareket yok; alternatif bir arac SQL Workbench/J.

http://www.sql-workbench.net/

Masaustu ikonu

```
[Desktop Entry]
Comment=
Terminal=false
Name=SWJ
Exec=/bin/sh /home/burak/Downloads/Workbench-Build121/sqlworkbench.sh
Type=Application
Icon=/usr/share/icons/Win7-icons/apps/gnome-networktool.png
```

Farkli tabanlara baglanti destegi var. Postgresql icin JDBC jar suradan

https://jdbc.postgresql.org/download.html

SWJ icinden File | Manage Drivers secilir, oradan Postgresql icin
indirilen jar secilir.

SQLLite icin jar

https://repo1.maven.org/maven2/org/xerial/sqlite-jdbc/3.8.9.1/

Yine manage drivers'dan sqllite icin yeni profil yaratilir. Sonra yeni
baglanti yaratilir, File | Connect Window'dan sol ust kosedeki dugme
ile.







