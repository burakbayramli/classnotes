# Resimlerinizin Haritasini Çikartin




Resimlerinizin Haritasini Çikartin



 IMAGE map'ler kullanicinin belirli bir resmin degisik alanlarina tikladiginda farkli adreslere gidebilmesine olanak tanir. Iki sekilde hazirlanabilecek Image Map'ler istege göre HTML sayfasina veya sunucu üzerindeki baska bir dosyaya yerlestirilebilir. Biz bu konumuzda HTML sayfalarina yerlestirilen Image Map'leri inceleyecek Paint Shop Pro yardimiyla bir resmin haritasinin nasil çikarilacagini gösterecegiz.                               <MAP NAME="map1"><!-- Ã�rnek resmin HTML kodlari --><AREA SHAPE="RECT"  COORDS=' 7, 7, 119, 74'  HREF="http://www.draskin.150m.com"><AREA SHAPE="CIRCLE"  COORDS=' 178, 41, 35'  suraya="http://www.idg.com.tr"></MAP>               Öncelikle HTML sayfasinda yukaridaki gibi kodlar kullanir ve istedigimiz resimde hangi noktalara tiklandiginda hangi adreslere baglanilacagini belirtiriz. Daha sonra  gibi bir ifade kullanilarak haritasinin çikarilmasini istedigimiz resmi sayfaya yerlestirir ve bu resim için ' map1'  isimli haritanin kullanilacagini belirtiriz.                MAP: TAG'I <MAP NAME='deneme' > AREA takilariyla tanimlanan haritaya baslangiç yapar ve bir isim verir.                AREA: TAG'I <AREA COORDS=koordinatlar SHAPE=tip HREF=url TARGET=pencere>                 COORDS: Tanimlanan sekle göre degisik koordinatlar girebilmenizi saglar                 SHAPE=tip: Tip RECTANGLE, CIRCLE, POLYGON seçeneklerinden biri olabilir. RECTANGLE Bir kare tanimlar ve ' x1, y1, x2, y2'  degerleri kullanilir. CIRCLE Bir çember tanimlar ve ' OrtaX, OrtaY, Yariçap' degerleri kullanilir. POLYGON Bir poligon tanimlar. Birçok noktadan olusabilir.                  HREF: Sekle tiklandiginda hangi adrese gidilecegini belirler.                 TARGET: Gidilecek adresin hangi pencerede açilacagini belirler                Peki tanimladigimiz sekillerin koordinatlarini nasil bulacagiz? Bu da çok basit, en büyük yardimcimiz olan Paint Shop Pro'yu kullanarak istedigimiz bir noktadaki koordinatlari bulabiliriz. Örnegin bir karenin sinirlarini ögrenmek istiyorsunuz, hemen resmi açin ve tanimlamak istediginiz karenin üst sol noktasina gidin. X1 ve Y1 noktalarini durum çubugundan ögrenip bir kenara not ettikten sonra sol alt sinira gidin ve X2, Y2 noktalarini ögrenin. Iste bu kadar basit.                Yazarin izni ile http://draskin.150m.com sitesinden alinmistir




![](image_map.gif)
