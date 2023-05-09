# SQL veri analizi nasıl hızlandırılır

Veri inçlemek için SQL dilini kullanan programcılar için
yazıyoruz. Özellikle CRM, yani veri ambarı olan programlarda, SQL
dilini çok kullanacaksınız. Servis programlarında genelde SQL veri
analizi 30,000 satırlık veriyi geçmez.  Veri ambarları daha çok veri
işler, o yüzden daha değişik tekniklere ihtiyaç duyarlar.  Eğer ambar
SQL kodunu, internet sitelerinde kullanılan SQL kodu gibi yazarsanız,
saatlerce ekran başında beklersiniz, SQL kodu katiyen işini bitirip
geri gelmez.  Bu yüzden, SQL kodunuzu hızlandırmanın yolunu bulmanız
lazım. Kullanılan metodlar arasında

* Tablo uzerinde dizin yaratma

* SQL koduna 'dizin' kullandirtma

* 'Geçici' tablo olurturma İndeks nedir, acele işleyelim. Bildiğiniz
gibi veri tabanı kayıt tutar. Bu kayıtlar tablolar içinde
tutulur. Tablo ne olduğunu anlamak için, diğer yazılarımıza
bakabilirsiniz.

Hemen örnek bir veri tabanı tablosu gösterelim.

MUSTERI(ISIM VARCHAR2(100),SOYAD VARCHAR2(100),EMAIL VARCHAR2(100))

Tablo veri türüdür. Yani veri tabanına diyorsunuzki "Bu şekilde
verileri bu tablo adı altında gireceğim, hazır ol". Bundan sonra veri
tabanına SQL dilini kullanarak veri girebilirsiniz. Mesela

INSERT INTO MUSTERI ('Burak', 'Bayramli', 'burakbayramli@sk.com');

Eğer veriye erişmek istiyorsanız, (mesela bütün verileri ekranda
gösterelim), o zaman tekrar SQL dilinde SELECT * from MÜŞTERİ; .. diye
bir kod işletmeniz yeterlidir.

Fakat, sadece belli kayıtlara erişmek istiyorsanız, o zaman 'seçici'
SQL kodu kullanmanız lazım. Mesela sadece ismi 'burak' olan kayıtları
bulalım.  SELECT * from MÜŞTERİ WHERE ISİM = 'Burak';

İşte dizinler, bu gibi SQL kodu hızlandırmak için işinize
yarar. dizinler kütüphanelerde olan kitap kartları gibidir, hani bir
kitabı bulmak için önce o kartlara bakıp, nerede olduğunu
öğrenirsiniz, ve direk o bölüme gidersiniz. Veri tabanı dizinleri aynı
şekilde işler. Her tablo üzerinde dizin yaratabilirsiniz. Bir tabloda
birden fazla dizin olabilir.

İndeks yaratmak için bir örnek verelim.  CREATE INDEX MÜŞTERİ_DİZİN ÖN
MÜŞTERİ(İSİM) Bu komutu işleterek veri tabanına dedinizki "Eğer isim
hanesini kullanara müşteri tablosuna erisenler olursa, işlemi dizin
kullanarak yap".

İndeks kullanan SQL kodu daha çabuk işler. Genelde dizinler otomatik
olarak bulunur ve kullanılır veri tabanı tarafından. SQL programınızın
değişmesi gerekmez.  Not: Veri tabanlarında tabii ki hiçbir şey bir
başka şey kaybetmeden kazanılmaz. İndekslerin sürekli güncel tutulması
gereklidir, buda zaman alır. O yüzden her İNSERT kodu artık daha yavaş
olacaktır.

Alın size muazzam bir mühendislik problemi: "Programınız daha çok
analizmi yapıyor, yoksa verimi ekliyor". Eğer analiz yapıyorsanız, çok
dizin eklemenin pek zararı olmaz. Ekleme yapıyorsa, dizinleri azaltın.
Gelelim ikinci metoda: İndeks kullandırma. Biraz önce bahsettik,
dizinlerin kullanılması otomatik olarak veri tabanı tarafından
yapılır. Fakat bazen Oracle gibi gelişmiş veri tabanları bile, hangi
dizini kullancağını karıştırabilir. Bu aslında çok normal, sonuçta
milyonlarca kodluk programlar olsada, SQL programcılarının beynini
okuyacak seviyede değiller. Bazen Oracle çok kötü analiz kararı
alabilir.  İşte bu gibi vahim zamanlarda, SQL kodunuza "tiyo" vermeniz
gerekir. Yani diyeceksinizki "Sayın oracle, biraz kafan karıştı
galiba, hangi dizin kullanacağını unuttun, al bunu kullan". Bunun kod
olarak şekli: SELECT /*+ INDEX (MÜŞTERİ_DİZİN) */ İSİM FROM MÜŞTERİ
WHERE ISİM = 'burak'; Böylece Oracle, doğru dizini bularak veriye
hızlı şekilde erisebilir.  Üçüncü teknik, geçici tablo
oluşturma. Geçici tabloları, çok zor görünüşlü SQL kodunu parçalamak
için kullanın. Unutmayın, eğer 3~4 sayfalık SQL kodu yazmışsanız,
Oracle perde arkasında komik şeyler yapabilir. Veri tabanını belli
şekilde işleme 'zorlamak' için, SQL kodunuz parçalara ayırın, ve
geçici tablolar oluşturun, o tabloları alıp sonraki tabloya koyun,
vs. Unutmayın, eğer geçici tabloyu siz oluşturduysanız kontrol sizde,
eğer Oracle oluşturursa, sistemin vefasına kaldınız
demektir. Kontrolün elde olması her zaman daha iyidir.





