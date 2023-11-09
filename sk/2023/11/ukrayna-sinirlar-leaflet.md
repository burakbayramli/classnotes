# Leaflet ile Sınır Çizimi, Ukrayna

Çizgi grafiklemek, seçim listesinden parametre seçme örnekleri için faydalı
olabilir, Ukrayna'daki çatışmada iki taraf arasındaki sürekli değisen sınırları
herhangi iki tarih için öncesi ve sonrası olarak gösteren bir araç alttadır,

[https://burakbayramli.github.io/ukrconf](https://burakbayramli.github.io/ukrconf)

Gösterilen kodlama örnekleri, mesela uzaktaki bir JSON dosyasını okumak,

```
url = ...
var xmlHttp = new XMLHttpRequest();
xmlHttp.open( "GET", url = url, false ); 
xmlHttp.send( null );
result = xmlHttp.responseText;
res = JSON.parse(result);
```

Bir haritada enlem, boylam listesini cizgi olarak basmak,

```
map = ...
var line = new L.Polyline(aft, {
	color: 'red', weight: 2, opacity: 0.5, smoothFactor: 1
});
line.addTo(map);
```

Ayrıca basılan bir çizgi serisini haritadan silmek,

```
lines = [];
function plot(before,after) {
    lines.forEach(function(x) {	
	x.remove(map);
    });
    lines = [];
    ...
```
