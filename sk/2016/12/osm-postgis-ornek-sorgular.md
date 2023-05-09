# OSM, PostGIS, Örnek Sorgular

Daha onceki yazida OSM (Open Street Map) veri tabanını nasıl
Postgresql (PG) tabanına yükleneceğini gördük. PG + PostGIS ve OSM
verisi hakikaten çok kuvvetli bir üçlü. OSM verisi Wikipedia'nın
ansiklopedik bilgi için yaptığını dünya yer verisi için yapmaya
uğraşıyor, gönüllüler kendi uğraşları ile bu tabanı güncel
tutuyorlar. PG tabanı zaten Oracle'ın yerine geçmesi için yazılmış bir
açık yazılım, PostGIS uzantısı iyi işliyor. Açıkça söylemek gerekirse
bu üçlü etrafında koca bir şirket kurulabilir. İngilizce bir deyimi
tercüme etmek gerekirse "sınır gökyüzü (sky is the limit)".

OSM etraftaki pek çok cismi, yeri, binayı enlem / boylamı ile birlikte
kategorize de etmiş, yani veri zengin. Mesela tüm restoranları görmek
için

```
select name,st_asText(st_transform(way, 4326)) from planet_osm_point
where amenity = 'restaurant';
```

Soyle bir sonuc geldi

```
Remise
POINT(13.0948499639389 52.4143189771462)

RBB-Kantine „Teestube“
POINT(13.1212677994774 52.3899105025706)

Mensa Griebnitzsee
POINT(13.1278047499684 52.3935486680721)

Piazza Toscana
POINT(13.122158209587 52.3947633697572)
..
```

Cep telefonundan "bana yakındaki restoranları göster" gibi bir komut
bu veriyi kullanabilir mesela.

Bu amenity (tesis) kolonunda pek çok diğer ilginç değer var, bazı örnekler

```
select distinct(amenity) from planet_osm_point;
```

```
police_box
car_sharing
bank;post_office
lockbox
conference_centre
car_club
waste_basket
hospital
register_office
nursing_service
contemporary_art_gallery
stock_exchange
ebsuinesslotse
public_building
lockers
biergarten
post_office
elevator
doctors
food_court
gambling
stroller_repair
variety_store
gallery
taxi
Beratungsstelle
toilets
luggage_locker
shelter
nightclub;restaurant;art_gallery;pub
fire_station
coworking_space
..
```

Evet bir cop tenekesi bile tabanda bulunabiliyor,

http://wiki.openstreetmap.org/wiki/Tag:amenity%3Dwaste_basket

Caddeler

Mesela belli bir enlem / boylamın 1 km yakınındaki tüm yürünebilen
caddeleri görmek istiyoruz,

```
SELECT
osm_id,
name,
st_asText(st_startpoint(st_transform(way, 4326))),
st_asText(st_endpoint(st_transform(way, 4326))),
st_distance(st_startpoint(st_transform(way, 4326)),st_endpoint(st_transform(way, 4326)))
FROM planet_osm_line
where 1=1
and highway in ('footway','living_street','residential')
and st_dwithin(st_transform(way, 4326),st_geomfromtext('POINT(13.427 52.540)',4326)::geography, 1000)
and st_distance(st_startpoint(st_transform(way, 4326)),st_endpoint(st_transform(way, 4326))) > 0
```

Daha çetrefil sorgular düşünülebilir, mesela PostGIS iki coğrafi obje
arasındaki kesişmeleri bulabiliyor. Belli bir kordinat yakınındaki tüm
yürünebilen caddeleri alalım, ve bu kümeyi kendisi birleştirelim
(join) ve hangi yol hangisi ile kesişiyor onu bulalım,

```
SELECT p1.osm_id as o1, p2.osm_id as o2,
       st_asText(st_transform(st_intersection(p1.way,p2.way), 4326)) 
FROM
       planet_osm_line as p1 inner join planet_osm_line as p2
       on st_intersects(p1.way,p2.way) 
where 1=1
      and p1.highway in ('footway','living_street','residential')
      and p2.highway in ('footway','living_street','residential')
      and st_dwithin(st_transform(p1.way, 4326),st_geomfromtext('POINT(13.4270146842 52.5408961579)',4326)::geography,1000)
      and st_dwithin(st_transform(p2.way, 4326),st_geomfromtext('POINT(13.4270146842 52.5408961579)',4326)::geography,1000)
      and p1.osm_id < p2.osm_id 
```

Hic fena degil.

Kitap olarak PostGIS In Action tavsiye edilir.






