# Yol Tarifi, Harita Bilgisi: osrm-backend


Yol Tarifi, Harita Bilgisi: osrm-backend




Verili herhangi bir enlem / boylam kordinatına en yakın sokak, yol isimlerini, yerlerini bulabilen, hatta bir noktadan diğerine nasıl gidileceğini veren bir veri tabanı ve program ÖSRM. Aslında bu arkadaşlar OpenStreetMap adli projenin veri tabanını daha iyi erişilebilir hale getirmişler.

https://github.com/Project-OSRM

Proje C++ bazlı, hızlı işliyor. İçinde OSM projesinin dosyalarını alıp onları daha ufaltan, ve bir servis ve APİ üzerinden dış erişime hazır hale getiren kodlar var. Bu servisin nasıl işlediğini göstermek için kendileri bir bedava servis işletiyorlar,

http://router.project-osrm.org

Mesela alttaki kordinata en yakin cadde hangisidir

http://router.project-osrm.org/nearest/v1/foot/29.036428,40.987659

ile bulunabiliyor, çıktı JSON formatında. İki nokta arasında nasıl gidilir, hangi caddeler kullanılır,

http://router.project-osrm.org/route/v1/foot/29.036428,40.987659;29.039228,40.992186

URL icinde v1 ardindan gidis sekli verilmeli, araba icin car, bisiklet icin bicycle; ustteki yuruyerek.

İstenen her  türden veri üstteki bedava servisten alınabilir, ama başkasının servisini fazla yormamak (!) için (ve yerel bazlı erişim daha hızlı olur tabii) kendi servisimizi oluşturabiliriz.

Kod Github'dan indirilir, ve apt-get install alttakiler uzerinde isletilir,

cmake libtbb-dev lua5.2 libboost-all-dev liblua52 libluabind-dev liblua5.2-dev libluabind-dev libstxxl-dev libxml2 libxml2-dev libosmpbf-dev libbz2-dev libprotobuf-dev

Simdi proje dizinine girilip

mkdir -p build
cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
cmake --build .
sudo cmake --build . --target install

isletilir. Ayrica ayni dizinde hangi ulasim sekli var ise, onun lua dosyasina Unix sembolik baglanti kurulmali, mesela araba yontemi icin

ln -s profiles/car.lua .

Kurulum bitti, simdi veri lazim, mesela Turkiye icin

http://download.geofabrik.de/europe/turkey.html turkey-latest.osm.bz2

Tum veri icin http://download.geofabrik.de

Bu dosya acilir, simdi dosya uzerinde

osrm-extract turkey-latest.osm

ve

osrm-contract turkey-latest.osrm

Dikkat bu komutlari lua linki neredeyse oradan isletmek lazim. O zaman osrm dosya referansina dizin ismi eklemek gerekebilir.

Bu son komut uzun surecek, 30-40 dakika gibi.. Bittikten sonra artik veri hazir, veri dizininde

osrm-routed map.osrm

isletilir. Bu komut 5000 port'unu dinleyen bir servis baslatir. Artik ustteki turunden tum komutlarda http://router.project-osrm.org yerine http://localhost:5000 kullanilabilir.

Baglantilar

https://www.digitalocean.com/community/tutorials/how-to-set-up-an-osrm-server-on-ubuntu-14-04

https://github.com/Project-OSRM/osrm-backend/wiki/Running-OSRM

https://github.com/Project-OSRM/osrm-backend/blob/master/docs/http.md





