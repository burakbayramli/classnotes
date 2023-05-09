# PDF Formlari Otomatik Olarak Doldurmak

English 

PDF bazli formlari otomatik olarak doldurmak icin ekteki Python scriptlerini paylasiyoruz. Kullanicinin yapmasi gerekenler once PDF dosyasini jpg dosyalarina cevirmek, sonra UI aracimiz uzerinden bu imajlar uzerinde kutularin yerlerini tiklayarak isaretlemektir. Sonra doldurulacak bilgiler bir text dosyasina yazilir, ve script tum bunlari otomatik olarak birlestirir.

Once cevirme islemi

python convert.py BELGE.pdf [hedef dizin]

Program bitince [hedef dizin] icinde BELGE-0.jpg, BELGE-1.jpg,
vs. gibi dosyalar olacak.

Simdi kutu yerlerini belirleyin

python locs.py [hedef dizin]/BELGE-0.jpg

GUI programi baslayacak, burada kutulara belli bir sirayla
tiklayin. Siranin mantiki bir sira olmasi iyi olur, yukaridan asagi,
soldan saga gibi. Cogu PDF formu kutular uzerinde bir sira no'su da
verir, bu kullanilabilir mesela.

Her tiklama o tiklanan kordinati BELGE-0.jpg.loc dosyasina
yazacaktir. Dosyaya yazim islemi her tiklamada yapilir, yani isiniz
bitince locs.py programini kapatmak yeterli.

Simdi doldurulacak veriler icin BELGE-0.jpg.fill diye bir dosya
baslatin, diger dosyalar ile ayni dizinde olsun. Bu dosyadaki her
bilgi, loc dosyasindaki kordinat satirina tekabul ediyor olmali.

Eger gerekiyorsa, mesela [down=30] komutu ile 30 piksel asagi
gidilebilir. Ayni sekilde down, yerine up, left, right komutlari da
kullanilabilir. Font kucultulmek, buyultulmek isteniyorsa, mesela
[font=20] ile yeni deger tanimlanabilir.

Bu dosya tamamlaninca

python fill.py [hedef dizin]/BELGE-0.jpg

Bu son script loc, fill, jpg dosyalarini birlestirecek, ve doldurulmus
formu BELGE-0.jpg-out.jpg olarak goreceksiniz.

Aracimiz ImageMagick kullanir, baslamadan once kurulmus oldugunu
kontrol edin. Gerekli diger Python kutuphaneleri icin Ubuntu uzerinde

sudo apt-get install python python-tk idle python-pmw python-imaging
python-imaging-tk

Bu program icin bir potansiyel ilerleme bir oruntu tanima algoritmasi
kullanarak kutularin yerlerini otomatik olarak belirlemektir; boylece
kutulari tiklama ile isaretleme islemine gerek kalmaz, loc dosyasi
kendiliginden ortaya cikar.

Indir

https://github.com/burakbayramli/kod/tree/master/formfill


