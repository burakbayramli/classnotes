# jQuery Grafikleme Eklentisi - Flot


jQuery Grafikleme Eklentisi - Flot



Bir liste icindeki x-y kordinati uzerinden verilmis sayi degerlerinin grafigini (plot) gostermek icin jQuery eklentisi Javascript kutuphanesi Flot iyi isliyor. Pur zaman bazli olan Y degerlerini grafiklemek istesek mesela (diyelim ki bir hisse senetinin fiyatlari), Y bazli dizine ekleme yaparak, 1'den baslayarak birer birer artacak x degerlerini de eklemek  zorundayiz. Google App Engine servis Python tarafi:class Aksiyon(webapp.RequestHandler):def get(self): prices = ... # fiyatlar res = [] for index, stock in enumerate(prices):     res.append( [index, stock.price] ) self.response.out.write(simplejson.dumps( res ))Dizin prices icinde pur Y kordinatindaki fiyatlar var. Biz bu listeyi alip, 2 birimli listelerin listesi haline getiriyoruz, [[1,44], [2,55], ..] gibi. Sonra JSON ile bu liste Javascript'e donuluyor.Not: enumerate() cagrisinin kullanilmasinin sebebi "index" degiskenini kullanabilmek; bu degisken bir listede gezilen her oge icin onun numerik (indis) yerini raporlayabilen bir cagridir.HTML icinde grafigin gosterilecegi yeri bos bir div olarak birakmak yeterli. CSS stili "display:none" ile ilk yuklendiginde gorulmez halde birakilabilir.<div id="placeholder"></div>jQuery cagrisi ise soylefunction grafik() {$("#placeholder").show();$.getJSON('/aksiyon',    { ... },    function (data) {                 $.plot($("#placeholder"), [data]);    });}[data] kullanimi "liste icinde liste icinde liste" haline gelmis oluyor.




