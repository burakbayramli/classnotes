# Veri Bilimi, Ceviklik, Anlik Veri Ambari

Yeni yaklaşıma göre veri ambarı şemasının önceden yaratılmadığını,
Hadoop merkezli teknolojiler ile sonradan, ihtiyaca göre ortaya
çıkarıldığını söyledik. Bunu yapıyoruz çünkü yapabiliyoruz, elimizde
devasa bir analiz kapasitesi var, ayrıca yapısız, dağınık, bol
miktarda ve (başta) kirli veri setleri var.

Bu bağlamda bazı ticari teknolojilerden bahsettik. Fakat bahsedilen
teknolojilerin önemli bir problemi, ticari olmalarıdır. Altyapı
dünyasında açıkca gördük ki açık yazılım daha tercih edilen bir
yaklaşım - programcılar bir projeden diğerine, bir işten diğerine
yanlarında rahatça taşıyabilecekleri teknolojileri tercih ediyorlar,
ayrıca açık ürünlerin etrafında daha rahat bir kullanıcı ağı
oluşabiliyor, bu soruların daha çabuk cevaplanması demek olabilir,
daha iyi kullanıcı belgelendirmesi, hataların daha hızlı tamiri demek
olabilir.. "Satış temsilcisi", "deneme amaçlı yazılım indirme" gibi
işlerle uğraşmaya gerek kalmaz (Platfora şirketi mesela bize bu
şekilde bir yazılım sağlayamadı).

Peki eğer açık yazılım ürünlerini merkeze koyarsak, yeni veri bilim
işi neye benzer? Bir soru daha: Bir veri bilimci (data scientist)
günlük bağlamda nasıl çalışır? Nelere odaklıdır? Her gün üstüne biraz
daha eklediği iş birimi, öbeği hangi tür çabanın etrafında döner?

Bu soruları cevaplamaya çalışan bir kitap: Çevik Veri Bilimi (Ağile
Data Science). 
  
Kitabın yaklaşımına göre kullanılan araçlar Hadoop etrafında döner.

Tüm veri HDFS (ya da Amazon S3'e) gönderilir [1]. Oradan Pig ile
transform edilip, temizlenip, MongoDB'ye "yayınlama" amaçlı koyulur
[2]. Aynı anda bir "veri Web arayüzü" yazılır, ve arayüz veriyi,
gösterebildiği şekilde, yavaş yavaş sunmaya başlar. İlk başta bu tek,
atomik kayıt bazlı olabilir (kullanıcı verisi mesela), daha sonra
toparlanarak özetleme amaçlı çıktılar sunulur, bol bol, akla gelen her
türlü grafik eklenir. Bu geliştirme eforu bir süreç haline gelir, veri
gönder / geri çek / temizle / yayınla. Bu sırada şirkette herkes bu
arayüzü kullanmaktadır, şirket lideri, programcılar, diğerleri, o
arayüz etrafında / sayesinde bir veri anlayışı, veri bakışı oluşur, ve
yavaş yavaş keşifler ortaya çıkmaya başlar.

Yapay öğrenim (machine learning) fırsatları bu noktada ortaya çıkmaya
başlayabilir, bunlar kodlanınca onların çıktısı da MongoDB'ye koyulur,
yani "yayınlanır". MongoDB'de olan rahat bir şekilde hem veri
arayüzüne, hem de ana programa alınabilir (kullanıcılarını analiz
ettiğimiz program yani), böylece çember tamamlanmış olur. Bu süreç
devam ettirilir.

Platfora, Datameer, Hünk (Splunk) gibi ürünler genel anlayış olarak
doğru yoldalar, yani veri ambarını dinamik olarak yaratmak, "siz bize
Hadoop kümenizi gösterin, biz ona bağlanıp size güzel raporlar
sunarız" bakışında bir avantaj var. Sadece bunu bahsedilen ürünler
üzerinden yapmak, onları her şirketin kendi amaçları için
kullanabilmek külfetli hatta yetersiz kalabilir. Bu açıdan Çevik Veri
Bilimi veri bilimciye kendi araçlarını kullanmasını öğütler, ve
sürekli bir GUİ geliştirerek rutin bir şekilde veride görülen,
keşfedilen sonuçları arka arkaya yayınlanmasını tavsiye eder. Kitabın
araç demeti Hadoop, Pig, Flask, MongoDB, Amazon Elastic Map/Reduce,
dotCloud ürünleri / servisleridir.

Rutin bir şekilde veri yayınlamak, elde avuçta ne varsa onu hemen
göstermek sürekli büyüyen, gün geçtikçe daha çok işlev edinen bir
uygulamanın inşası, veri bilimci bağlamında "bu adam ne üzerinde
çalışıyor?" sorusunu da rahatça çevaplar. Ne bulunmuşsa, üzerinde ne
çalışılmışsa, gittiği yer bellidir. Yapay öğrenim ile bulunanların
yayını da aynı şekilde tek bir yere gitmesi faydalıdır. Alternatif
olarak mesela veri bilimci bir odaya kapanıp lojistik regresyon,
kmeans, vs gibi algoritmaları ardı ardına veri üzerinde kullanarak iki
ay sonra bir şey "bulacaktır", fakat bu bulunan sonuç ile ne
yapılacaktır? Koridorda yürüyen CEO'ya kenara çekilip "bakın vs vs
buldum, şimdi bunu kullanalım" mı denilecektir? Pek çok açıdan Çevik
Veri Bilimi yaklaşımının veri analiz sürecine faydaları var.

Not: Niye Hadoop? Çünkü tıklama verisi tanım itibariyle "büyüktür", bu
veri bir kere bir yere gidince, tüm diğer alakalı verinin de onunla
beraber aynı yere gitmesi gerekir.

Not: Ham veri "büyük" ise o veriden analiz sonrası çıkan veriyi
tutacak yapı da "büyük" olmalıdır, yani anahtar / değer temelli NoSQL
tarzında bir yapı olmalıdır, Hadoop ve NoSQL tabanların atbaşı
şeklinde sürekli yanyana bahşedilmesinin bir sebebi de bu.





