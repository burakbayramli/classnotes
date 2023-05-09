# Android Üzerinde Veri Tabani Kullanımı - sqllite

Mobil uygulamalarımızda hafızaya sığmayacak kadar büyük, anahtar bazlı
hızlı erişim, çetrefil sorgulama gerektiren veri depolaması için
ilişkisel bir taban kullanmak gerekebilir. Her Android sürümüne dahil
edilen Sqlite veri tabanı bu ihtiyacı rahatça karşılar. Sqlite ufak
boyutlu tabanlar kategorisinde oldukça popüler bir ürün. Firefox,
Skype, Python, Mac OS-X kullanıyorsanız, zaten Sqlite kullanıyorsunuz
demektir [1]; bu ürünlerin hepsinin içinde Sqlite var, onu kendi iç
veri yapılarını depolamak, ona hızla erişmek için kullanıyorlar.

Başlamadan önce önemli bir nokta: Sqlite'ın Android içindeki kullanımı
JDBC üzerinden değil. Bu erisim mobil ortam içinde JDBC ile
yapılabilir, fakat biz bunu tavsiye etmeyeceğiz; tabana erişimin
Google'ın kendi API'ı üzerinden yapılmasını tavsiye edeceğiz. Bu
Google tarafından açılmış bir "yol", ve bu yolun güç kullanımı (power
consumption), performans gibi konular açısından en optimal yol olması
gerekiyor. Gücü, işlemci hızı sınırlı bir platformda kodlama
yaptığımızı hatırlarsak, o zaman bütün çözümler buna göre
şekillenmeli. Test etme bölümüne gelince ve geliştirme ortamımız
içinden Android DB API çağrılarını taklit etmek gerekli olunca, bunun
arka plan kodlarını JDBC ile kodlayacağız, bunlar nasıl olsa dışarıda
işleyen kodlar.

Dosyalar ve Erişim

Android geliştirme ortamından, emulatöründen blogumuzdaki su yazıda
[2] bahsetmiştik. Sqlite veri tabanı kendi dosyalarını telefon
üzerindeki mevcut dosya sistemine yazıyor. Bu dosyalara emulatörü
başlattıktan sonra [ANDROID_SDK]/tools altından "adb shell" komutunu
vererek göz atabiliriz [3]. IDE makinamıza tek bir telefon bağlı ise
(emulatör de bir telefon olarak addediliyor) o zaman "adb shell" bizi
direk emulatöre götürecek. Bu ortam aynen bir Unix shell ortamıdır,
dosyaları, dizin yapısı var. Sqlite dosyaları /data/data altında
bulacağız; "cd /data/data/[UYGULAMAMIZIN PAKETI]/databases altında
uygulamamızın veri tabanını görebiliriz. Bu tabana daha yakından göz
atmak için ise telefon shell'i üzerinde "sqlite3 /data/data/.../[TABAN
ISMI] komutu yeterli. Bu ikinci ortam herhangi bir SQL shell'ine
benziyor. SELECT, INSERT gibi sql komutları burada işletilebiliyor.

Eğer veriyi telefondan geliştirme makınamıza çıkartmak istiyorsak, bu
makina shell'i üzerinden "adb pull" komutu kullanılabilir, diğer yöne
gitmek için "adb push" gerekiyor.

Veri Tabanı Yaratmak - Genel Kullanım

Bir tabanının şemasını yaratmak ve kullanmaya başlamak için
yöntemlerden biri Android uygulama kodları içinde SQLiteOpenHelper
class'ından miras alan bir yardımcı class yaratmak ve bu objeyi bir
Activity içinden işleme sokmak. Bu noktadan sonra helper objesi bizim
veri tabanına olan bağlantımızdır, bu referans üzerinden istediğimiz
DB çağrılarını yapabiliriz.

```
public class DataSQLHelper extends SQLiteOpenHelper {

 private static final String DATABASE_NAME = "[DB ISMI]";
 private static final int DATABASE_VERSION = [DB VERSIYON];
 ..
 public DataSQLHelper(Context context) {
   super(context, DATABASE_NAME, null, DATABASE_VERSION);
 }
 @Override
 public void onCreate(SQLiteDatabase db) {
   String sql = "create table ... ";
   db.execSQL(sql);
   ..
 }
 @Override
 public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
   if (oldVersion >= newVersion)
     return;
   String sql = null;
   if (oldVersion == 1)
     sql = "alter table ...";
   if (sql != null)
     db.execSQL(sql);
 }
}

public class Demo extends Activity {

 DataSQLHelper db;

 @Override
 public void onCreate(Bundle savedInstanceState) {
   db = new DataSQLHelper(this);
    ..
   }

 @Override
 public void onDestroy() {
   super.onDestroy();
   db.close();
 }
 ..
}
```

DataSQLHelper.onCreate metodunda CREATE TABLE komutları kodlanacak. Bu
komutlar Android tarafından eğer veri tabanı mevcut değilse,
işletilir. Activity DataSQLHelper objesini yarattığı zaman, o obje
üzerinde tanımlanan int DATABASE_VERSION sayısını bir kenara
kaydediyor. Eğer bu rakamı sonraki sürümlerde kod içinde
değiştirirseniz, Android tarafından çağrılacak onUpgrade() altında
koyacağınız ALTER TABLE komutları devreye sokulabiliyor, böylece
database şemanızı bir versiyondan sonrakine otomatik olarak
geçirtebilirsiniz. onUprade metoduna (mevcut) eski versiyon ve en son
versiyon geçilecektir (Android altyapısı tarafından), ve bu rakamlar
uzerinde if..else irdelemeleri kullanarak o geçişe uygun şema değişimi
yapmak mümkün olacaktır. Eğer '1' versiyonundan '2' versiyonuna
geçiyorsam, şu ALTER komutlarını işlet, yoksa şunları işlet,
vs. şeklinde.

Üstteki örnek kodlar [4] her işletildiğinde, otomatik olarak
yaratılacak tabloya bir satır veri yazılacak ve aynı anda ekranda
önceden yazılmış veriler ekranda gösterilecek. Versiyon değişimini
test etmek için versiyonu 1 rakamindan 2'ye geçirelim, ve programı
derleyip tekrar emulatöre gonderelim. Mevcut tabloya yeni bir kolon
eklendiğini göreceğiz.

Sqlite 3 icin geçerli veri tipleri şurada [5] bulunabilir.

API

Veri tabanina erişimin Google'ın kendi arayüzleri üzerinden olduğunu
söylemiştik. Arayüzlerden en çok kullanılacak olanlar SQLiteDatabase
ve Cursor objeleri olacak. Bir INSERT, UPDATE komutu işletmek için
SQLiteDatabase üzerindeki execSQL(..) çağrısı kullanilabilir, bu
çağrıya String olarak bir DDL komutu, yani INSERT, UPDATE gibi arka
plandaki Sqlite tabanı için anlamlı komutlar verilebilir. Ayrıca
execSQL'e verilen SQL komutu içinde sabit veri gömmek yerine aynen
JDBC'de oldugu gibi değer olarak soru işareti '?' karakteri
kullanılabiliyor, ve bu soru işaretlerinin içinin nasıl doldurulacağı
execSQL'a geçilebilecek ikinci bir Object[] parametresi ile
tanımlanabiliyor. Soru işaretlerinin sırası, değerlerin Object[]
içindeki yerlerine birebir uymalı.

Veri okuma amaçlı olarak rawQuery çağrısı yapılabilir, aynı şekilde
SQL komutu ve parametreleri (gerekiyorsa, yoksa null) geçilerek, ve
SELECT komutu içeren bu cağrıdan, okuma amaçlı olarak bir Cursor
objesi geri döndürülür. Cursor,

```
while (cursor.isLast()) {
cursor.moveToNext();
String val1 = cursor.getString(0);
int val2 = cursor.getInt(1);
 ..
}
```

şeklinde gezilebilir. getString, getInt çağrılarına geçilen sıfır
temelli indeks değeri, SELECT komutunda listelenen kolon isimlerinin
sırasına tekabul ediyor olmalı. Daha fazla detay icin [6] dökumanına
danışılabilir.

Veri Tabanı Yaratmak - Statik ve Büyük Çapta Veri

Biraz önceki kullanım, ufak çapta veri tanımlaması, depolaması icin
uygun. Fakat çok büyük çapta veriyi, uygulama başlamadan hızlı bir
şekilde hazır etmek istiyorsak, o zaman bir Sqlite tabanını olduğu
gibi telefona göndermenin yollarını aramamız lazim. Muhakkak helper
onCreate() içinde DDL komutlarını uyguladıktan sonra, Internet'teki,
ya da res/raw dizinindeki [7] APK içinde paketlenmiş bir düz dosyayı
açarak, satır satır okuyup tabana INSERT ile yazabiliriz. Fakat bu tür
işlemler kapasitesi sınırlı mobil ortamda çok uzun zaman
alacaktır. Tipik kullanıcı uygulamayı ilk başlattığında saniyeler
sonra onun hazır olmasını bekler, dakikalar sürecek bir hazırlık
evresi uygun olmaz.

Sqlite için database bir dosyadan ibarettir, ve bu dosyayı gerekli
yere (/data/data altındaki dizine yani) kopyalamak o tabanı hazır hale
getirmek demektir, bu basit bir dosya kopyalama operasyonu
olacaktır. Bu çözümle ilerlersek, iki önemli nokta ortaya çıkar: 1)
Res/raw altına koyup telefona gönderebileceğimiz dosya 1.2 MB uzerinde
olamıyor 2) sürekli db dosyası kopyalamamak için (işlem her ne kadar
hızlı olsa da) üstteki versiyon yöntemine benzer hızlı bir kontrol
yöntemi gerekli.  O zaman: Sqlite veri tabanını önceden (telefon
dısında) pişirmek için geliştirme ortamımızda bir main() içinden
tetikleyerek (bunun nasıl olacağını test etme bölümünde göreceğiz
-hatta burada pür JDBC bile kullanılabilir-) yaratabildiğimizi
farzedelim. Bu dosyayı APK içinde göndermenin tek you onu res/raw
altına koymaktır. Eğer dosya çok büyük ise, onu Unix split ile
parçalara bölüp, kopyalama işlemi tarafından onu tekrar birleştirmemiz
gerekli. Bölme komutu:

split [DOSYA] -b 1M [yenidb-]

Boylece [DOSYA] 1 megabaytlık parçalara bölünecek ve yenidb-a,
yenidb-b, .. gibi yeni dosyalar ortaya çıkacak. Bu dosyaları
geliştirme dizinimizde res/raw altına koyalım. Java kod bağlamında
DB'yi hazırlamak için yapılacak ilk çağrı önce 1)
/data/data/[UYGULAMA]/databases altında belli bir isimde ve belli bir
büyüklükteki bir dosyanın olup olmadığını kontrol edecek, 2) yoksa,
res/raw altındaki parçaları birleştirerek yeni taban dosyasını
yaratacak. Bu yöntem, biraz çetrefil gibi kulağa gelse de, hiç
şüphesiz INSERT ile taban yaratmaktan kat kat daha hızlı
olacaktır. Önemli nokta: InputStream üzerinden parçalı bir dosyayı tek
bir dosyaymış gibi okumak icin InputStreamChain [8] yöntemini
kullanmak gerekli.

Tüm bu teknikleri kullanan bizim kodumuz [9] dosyasındaki
DatabaseHelper.importDB içinde bulunabilir. Büyük veri transferi için
kullanılan DatabaseHelper class'ının ilk örnekte olduğu gibi
SQLiteOpenHelper'dan miras almadığına dikkat edelim: Bu yapılmadı
çünkü gerekli değildi, statik veri durumunda versiyon kontrolunu taban
dosyası büyüklüğü (size) üzerinden biz kendimiz yapıyoruz. DDL zaten
gerekli değil, çünkü taban dosyasını olduğu gibi kopyalıyoruz.

Birim Testleri

Taban büyük ya da küçük olsun, eger geliştirme makinamızda Android DB
çağrılarını test etmek istiyorsak bu çağrıları taklitlememiz (mock)
lazım. Eğer telefon üzerinde JDBC kullanıyor olsaydık, taklitlemek
kolay olurdu, test kodları telefon yerine dışarıdakı bir tabanı açardı
(test kodlari bu tür bir bağlantıyı işlem kodlarına verirdi) ve malum
Connection, Statement, ResultSet kullanımları olduğu gibi
işlerdi. Fakat Android SQL durumunda özel API'leri taklitlemek
gerekiyor. Buradaki okurlar için iyi haber şu: Biz kendi projemiz icin
bu taklitlemeyi gerçekleştirdik, ve buradan paylaşıyoruz. Şu [9] zip
dosyası içinde SQLiteDatabase, SQLException, ve Cursor kodları arka
planda JDBC üzerinden bir Sqlite tabanına bağlanıp iş yapacak şekilde
tekrar yazıldı, böylece işlem mantığının dünyadan haberi olmayacak,
onlar kendini mobil platformda zannedecek ama aslında taklit kodları
işletiyor olacaklar. Sqlite JDBC jar kodları şuradan [10]
indirilebilir.

Tekniğin işlemesi için derlemek sürecinde, kod bazında taklit kodları
işlem kodlarından ayırmak, bu kodları src/ dizininden bağımsız, onunla
aynı seviyede "test" adlı bir başka dizine koymamız gerekti. Böylece
emulatöre (ya da telefona) kod gönderirken test dizini normal derleme
komutları tarafından derlenmeyeceği için (Ant ve Eclipse sadece src
dizinine göre hazırlanmış), taklit kodları emulatöre gitmemiş
olacak. Geliştirme, test ortamında ise taklit kodların cağrılması için
src ve test dizinlerini bizim eklerimizle aynı anda derleriz, ve
istediğimiz çağrımlar Android kodları yerine bizim kodlara gider. Aynı
zip dosyasindaki [9] build-add.xml icinde Ant ortamında bunun tekniği
mevcut: test-compile hedefi görüldüğü gibi iki kaynağı eşit seviyeden
alarak derliyor ve sonuç class dosyalarını bin/ altına
koyuyor. Eclipse gibi IDE kullanıcıları benzer derleme eklerini
yapmanın yolunu bulacaktır. Test kodları ise şuna benzeyecek:

```
public TestEdilenClass {
 DatabaseHelper helper;
 public TestEdilenClass(DatabaseHelper helper) {
   this.helper = helper;
 }
 public void method1() {
   ..
   helper.execSQL(...);
   ..
 }
 ...
}

public class TestAlg {

 @Test
 public void test() throws Exception  {
   DatabaseHelper helper = new DatabaseHelper();
   SQLiteDatabase db = new SQLiteDatabase(); // bu taklit class aslında
   helper.db = db;
   TestEdilenClass alg = new TestEdilenClass(helper);
   alg.method1();
 }
}
```

Görüldüğü gibi normal bir class (taklit edilmemiş) olan
DatabaseHelper'ı yaratıp onun uzerinde taklit edilmiş olan
SQLiteDatabase objesini set ediyoruz. Böylece DatabaseHelper Android
ortamında normal veri tabanı çağrıları yaptığını zannederken aslında
bizim taklit kodları çağırıyor, çünkü Helper class execSQL gibi
çağrıları db referansı üzerinden yapıyor. Bu kadar.

Not: JDBC kodlarının semantiği (cağrılış şekli) ve Android SQL
kodlarının farkları sebebiyle cursor üzerinde moveToNext() öncesi
sadece bir kere isLast() çağrılması gerekiyor. Eğer işlem mantığımızda
isLast birkaç kere çağrılacaksa, bu birim testlerimizi bozabilir, bu
değeri bir boolean icinde depolayıp cağrımı teke indirmemiz lazım. Bu
pek önemli bir nokta değil aslında, çünkü tipik kullanım bunu
gerektirmeyecektir.

Bu yazi ilk kez Java Dergisi 2008 Mayis ayi'nda mobil bolumunde
yayinlanmisti.

Kaynaklar 

[1] Well-Known Users of SQLite, http://www.sqlite.org/famous.html 

[2] Android Gelistirme Ortami | Sayilar ve Kuramlar Blog, http://sayilarvekuramlar.blogspot.com/2009/12/android-kurulumu.html 

[3] Android Debug Bridge | Android Developers, http://developer.android.com/guide/developing/tools/adb.html

[5] Datatypes In SQLite Version 3, http://www.sqlite.org/datatype3.html 

[6] Data Storage | Android Developers, http://developer.android.com/guide/topics/data/data-storage.html 

[7] Android ve Statik Dosyalari Okumak  | Sayilar ve Kuramlar Blog, http://sayilarvekuramlar.blogspot.com/2009/12/android-ve-statik-dosyalari-okumak.html 

[8] InputStreamChain | Paranoid Engineering Blog, http://paranoid-engineering.blogspot.com/2008/11/inputstreamchain.html

[10] SqliteJDBC, http://www.zentus.com/sqlitejdbc/ 

[11] Catalina Creek Blog, Placing a prepopulated sqlite database in an Android app, http://www.catalinacreek.com/blog/Placing_a_prepopulated_sqlite_database_in_an_Android_app 

[12] Android Examples - SQL Demo, http://marakana.com/forums/android/android_examples/55.html 

