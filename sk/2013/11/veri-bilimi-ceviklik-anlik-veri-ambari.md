# Veri Bilimi, Ceviklik, Anlik Veri Ambari




Veri Bilimi, Ceviklik, Anlik Veri Ambari




Suradaki yazidan devam edelim:

Yeni yaklasima gore veri ambari semasinin onceden yaratilmadigini, Hadoop merkezli teknolojiler ile sonradan, ihtiyaca gore ortaya cikarildigini soyledik. Bunu yapiyoruz cunku yapabiliyoruz, elimizde devasa bir analiz kapasitesi var, ayrica yapisiz, daginik, bol miktarda ve (basta) kirli veri setleri var.

Bu baglamda bazi ticari teknolojilerden bahsettik. Fakat bahsedilen teknolojilerin onemli bir problemi, ticari olmalaridir. Altyapi dunyasinda acikca gorduk ki acik yazilim daha tercih edilen bir yaklasim - programcilar bir projeden digerine, bir isten digerine yanlarinda rahatca tasiyabilecekleri teknolojileri tercih ediyorlar, ayrica acik urunlerin etrafinda daha rahat bir kullanici agi olusabiliyor, bu sorularin daha cabuk cevaplanmasi demek olabilir, daha iyi kullanici belgelendirmesi, hatalarin daha hizli tamiri demek olabilir.. "Satis temsilcisi", "deneme amacli yazilim indirme" gibi islerle ugrasmaya gerek kalmaz (Platfora sirketi mesela bize bu sekilde bir yazilim saglayamadi).

Peki eger acik yazilim urunlerini merkeze koyarsak, yeni veri bilim isi neye benzer? Bir soru daha: Bir veri bilimci (data scientist) gunluk baglamda nasil calisir? Nelere odaklidir? Her gun ustune biraz daha ekledigi is birimi, obegi hangi tur cabanin etrafinda doner?

Bu sorulari cevaplamaya calisan bir kitap: Cevik Veri Bilimi (Agile Data Science). 



  
Kitabin yaklasimina gore kullanilan araclar Hadoop etrafinda doner.

Tum veri HDFS (ya da Amazon S3'e) gonderilir [1]. Oradan Pig ile transform edilip, temizlenip, MongoDB'ye "yayinlama" amacli koyulur [2]. Ayni anda bir "veri Web arayuzu" yazilir, ve arayuz veriyi, gosterebildigi sekilde, yavas yavas sunmaya baslar. Ilk basta bu tek, atomik kayit bazli olabilir (kullanici verisi mesela), daha sonra toparlanarak ozetleme amacli ciktilar sunulur, bol bol, akla gelen her turlu grafik eklenir. Bu gelistirme eforu bir surec haline gelir, veri gonder /  geri cek / temizle / yayinla. Bu sirada sirkette herkes bu arayuzu kullanmaktadir, sirket lideri, programcilar, digerleri, o arayuz etrafinda / sayesinde bir veri anlayisi, veri bakisi olusur, ve yavas yavas kesifler ortaya cikmaya baslar.

Yapay ogrenim (machine learning) firsatlari bu noktada ortaya cikmaya baslayabilir, bunlar kodlaninca onlarin ciktisi da MongoDB'ye koyulur, yani "yayinlanir". MongoDB'de olan rahat bir sekilde hem veri arayuzune, hem de ana programa alinabilir (kullanicilarini analiz ettigimiz program yani), boylece cember tamamlanmis olur. Bu surec devam ettirilir. 

Platfora, Datameer, Hunk (Splunk) gibi urunler genel anlayis olarak dogru yoldalar, yani veri ambarini dinamik olarak yaratmak, "siz bize Hadoop kumenizi gosterin, biz ona baglanip size guzel raporlar sunariz" bakisinda bir avantaj var. Sadece bunu bahsedilen urunler uzerinden yapmak, onlari her sirketin kendi amaclari icin kullanabilmek kulfetli hatta yetersiz kalabilir. Bu acidan Cevik Veri Bilimi veri bilimciye kendi araclarini kullanmasini ogutler, ve surekli bir GUI gelistirerek rutin bir sekilde veride gorulen, kesfedilen sonuclari arka arkaya yayinlanmasini tavsiye eder. Kitabin arac demeti Hadoop, Pig, Flask, MongoDB, Amazon Elastic Map/Reduce, dotCloud urunleri / servisleridir.

Rutin bir sekilde veri yayinlamak, elde avucta ne varsa onu hemen gostermek surekli buyuyen, gun gectikce daha cok islev edinen bir uygulamanin insasi, veri bilimci baglaminda "bu adam ne uzerinde calisiyor?" sorusunu da rahatca cevaplar. Ne bulunmussa, uzerinde ne calisilmissa, gittigi yer bellidir. Yapay ogrenim ile bulunanlarin yayini da ayni sekilde tek bir yere gitmesi faydalidir. Alternatif olarak mesela veri bilimci bir odaya kapanip lojistik regresyon, kmeans, vs gibi algoritmalari ardi ardina veri uzerinde kullanarak iki ay sonra bir sey "bulacaktir", fakat bu bulunan sonuc ile ne yapilacaktir? Koridorda yuruyen CEO'ya kenara cekilip "bakin vs vs buldum, simdi bunu kullanalim" mi denilecektir? Pek cok acidan Cevik Veri Bilimi yaklasiminin veri analiz surecine faydalari var. 

--

[1] Niye Hadoop? Cunku tiklama verisi tanim itibariyle "buyuktur", bu veri bir kere bir yere gidince, tum diger alakali verinin de onunla beraber ayni yere gitmesi gerekir. 

[2] Ham veri "buyuk" ise o veriden analiz sonrasi cikan veriyi tutacak yapi da "buyuk" olmalidir, yani anahtar / deger temelli NoSQL tarzinda bir yapi olmalidir, Hadoop ve NoSQL tabanlarin atbasi seklinde surekli yanyana bahsedilmesinin bir sebebi de bu. 




