# OSM Haritaları, PBF Dosyaları, En Kısa Yol, Djikstra  

Eğer yollar ağını içeren OSM haritasını kendimiz işleyip çıktı
dosyalarını rahat okunabilir düz CSV formatında tutmak istersek bu
mümkündür. Sonuçta OSM dosyaları [1] sitesinde bedava paylaşılıyor, ve
bahsedilen işlemi yapabilecek bir kod Rust [5] ile yazılmış
`osm4routing` kodudur. Kurmak için

```
cargo install osm4routing
```

Örnek olarak Şeysel (Seychelles) adalarına bakalım, ufak bir dosya
olduğu için örnekleri göstermek işletmek hızlı olur. Haritası Afrika
dizini altında, oradaki osm.pbf dosyası indirilir.  `$HOME/Downloads`
altında olduğunu farzedelim,

```
osm4routing $HOME/Downloads/seychelles-latest.osm.pbf
```

Program oldukça hızlı işler, bitince iki tane dosya, `edges.csv` ve
`nodes.csv` üretilmiş olmalı. İçeriklerine bakalım (ilk 10 satır),

```python
! head -10 nodes.csv
```

```text
id,lon,lat
5766693114,55.2028362,-3.7270749
2737905006,55.694301599999996,-4.3259219
2710742974,55.4645328,-4.6004604
6808483052,55.462411599999996,-4.632185799999999
8979383052,55.518222699999995,-4.716342399999999
9144642426,55.4628521,-4.5883534
6407473046,55.4584707,-4.6071178999999995
8979071049,55.4408661,-4.624990299999999
3789265673,55.401677899999996,-4.6558733
```

```python
! head -10 edges.csv
```

```text
id,osm_id,source,target,length,foot,car_forward,car_backward,bike_forward,bike_backward,train,wkt
26771422-0,26771422,293645412,1159221829,113.98597980501721,Allowed,Secondary,Secondary,Allowed,Allowed,Forbidden,"LINESTRING(55.7601390 -4.3462977, 55.7602171 -4.3463866, 55.7602852 -4.3464620, 55.7603906 -4.3466138, 55.7604949 -4.3467581, 55.7605343 -4.3468123, 55.7605801 -4.3468621, 55.7606248 -4.3468929, 55.7606791 -4.3469152, 55.7607355 -4.3469237, 55.7608292 -4.3469269, 55.7608735 -4.3469255)"
26771422-1,26771422,1159221829,2330448860,71.52456370873085,Allowed,Secondary,Secondary,Allowed,Allowed,Forbidden,"LINESTRING(55.7608735 -4.3469255, 55.7613600 -4.3469271, 55.7614284 -4.3469290, 55.7614753 -4.3469333, 55.7615168 -4.3469417)"
26771422-2,26771422,2330448860,3025148125,28.923130985314625,Allowed,Secondary,Secondary,Allowed,Allowed,Forbidden,"LINESTRING(55.7615168 -4.3469417, 55.7615615 -4.3469545, 55.7616053 -4.3469776, 55.7616605 -4.3470097, 55.7617463 -4.3470621)"
26771422-3,26771422,3025148125,1159221343,38.10915100020672,Allowed,Secondary,Secondary,Allowed,Allowed,Forbidden,"LINESTRING(55.7617463 -4.3470621, 55.7618673 -4.3471685, 55.7619319 -4.3472277, 55.7620079 -4.3472833)"
26771422-4,26771422,1159221343,293645423,185.86358983936378,Allowed,Secondary,Secondary,Allowed,Allowed,Forbidden,"LINESTRING(55.7620079 -4.3472833, 55.7620788 -4.3473207, 55.7621627 -4.3473508, 55.7622172 -4.3473674, 55.7622874 -4.3473801, 55.7624471 -4.3474119, 55.7627090 -4.3474714, 55.7629027 -4.3475234, 55.7630251 -4.3475510, 55.7631092 -4.3475733, 55.7631855 -4.3476010, 55.7632561 -4.3476295, 55.7633785 -4.3476932, 55.7634737 -4.3477542, 55.7635244 -4.3477846, 55.7635869 -4.3478002)"
27539253-0,27539253,302653684,6465428329,33.680546394034714,Allowed,Residential,Residential,Allowed,Allowed,Forbidden,"LINESTRING(55.7178609 -4.3285000, 55.7179612 -4.3284562, 55.7181532 -4.3284834)"
27539253-1,27539253,6465428329,6465428366,286.87307167995203,Allowed,Residential,Residential,Allowed,Allowed,Forbidden,"LINESTRING(55.7181532 -4.3284834, 55.7182217 -4.3284969, 55.7182984 -4.3285174, 55.7183556 -4.3285060, 55.7184043 -4.3284612, 55.7185242 -4.3283945, 55.7189109 -4.3281743, 55.7193023 -4.3279508, 55.7195251 -4.3278331, 55.7195690 -4.3277377, 55.7196137 -4.3275597, 55.7196765 -4.3271970, 55.7197193 -4.3270878, 55.7197741 -4.3270417, 55.7199169 -4.3269372)"
27539253-2,27539253,6465428366,302325742,561.5506919930622,Allowed,Residential,Residential,Allowed,Allowed,Forbidden,"LINESTRING(55.7199169 -4.3269372, 55.7200442 -4.3269033, 55.7203764 -4.3268316, 55.7204300 -4.3268310, 55.7206230 -4.3268957, 55.7207681 -4.3269289, 55.7210028 -4.3269182, 55.7214062 -4.3269155, 55.7214761 -4.3268524, 55.7215157 -4.3267523, 55.7216959 -4.3264271, 55.7218848 -4.3262473, 55.7220907 -4.3262217, 55.7222023 -4.3262730, 55.7225027 -4.3264613, 55.7226572 -4.3265640, 55.7228804 -4.3265640, 55.7233782 -4.3264527, 55.7235524 -4.3264362, 55.7237215 -4.3263757, 55.7238674 -4.3263073, 55.7238326 -4.3262122, 55.7237155 -4.3261143, 55.7235396 -4.3258330)"
27539300-0,27539300,302325742,302325755,183.84092506580686,Allowed,Residential,Residential,Allowed,Allowed,Forbidden,"LINESTRING(55.7235396 -4.3258330, 55.7236362 -4.3257327, 55.7238713 -4.3256612, 55.7240734 -4.3256226, 55.7242633 -4.3256423, 55.7242641 -4.3255324, 55.7240806 -4.3254677, 55.7239876 -4.3253829, 55.7238701 -4.3252136, 55.7239435 -4.3249890)"
```

Çizit (graph) teorisi açısından bakarsak üstte veri bir ağ / çizit
yapısı var, ilk dosyadakiler düğümler (nodes) ikincidekiler ise
kenarlar (edges). Düğümler yeryüzünde bazı noktalar, bir durak
olabilir, yol ağzı olabilir, ya da yol üzerindeki bir nokta. Her
düğümün bir `id` kimliği var, ve bu `id` ile o noktanın kordinat
değerlerine enlem boylam üzerinden erişebiliyoruz. Kenarlar bir düğümü
bir diğerine bağlayan yollar gibi görülebilir, bağlantı parçaları. Her
kenarın da bir kimliği var, ve ayrıca çıkış noktası `source` bitiş
noktası `target` bilgisini taşıyor. Bu iki kolon tabii ki düğüm
verisindeki `id` değerlerine tekabül ediyor, kenar bir düğümden çıkıp
diğerinde bitiyor.

Kenarların, yani yolların taşıdığı bazı ek önemli bilgiler var; mesela
bir yolun yürümeye elverişli olup olmadığı (`foot` kolonunda `Allowed`
değeri var ise), aynı şekilde araba, bisiklet kullanımına uygun olup
olmadığı yol bilgisi içinde mevcut. 


Kaynaklar

[1] http://download.geofabrik.de/index.html

[2] ../../2016/11/yol-tarifi-harita-bilgisi-osrm-backend.html

[3] ../../2023/04/yol-bolmak-osm-osmnx.html

[4] https://github.com/Tristramg/osm4routing2

[5] <a href="../../2023/01/rust.html">Rust</a>

[6] <a href="../../2023/05/python-sozluk-dictionary.html">Python Sözlük (Dictionary) Veri Yapısı</a>

[7] <a href="https://burakbayramli.github.io/dersblog/algs/algs_035_dijks/dijkstra_algoritmasi_ile_en_kisa_yol.html">Dijkstra Algoritması ile En Kısa Yol</a>

[8] <a href="https://www.ics.uci.edu/~eppstein/161/python/">University of California Bilgisayar Bilim Kodları</a>

