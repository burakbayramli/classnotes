# Projemiz Niye Başarılı Oldu?

Alttaki liste, Hibernate'in ürününün çok popüler olmasında etken olan
geliştirme süreci ve yöntemleri açısından doğru yaptığımız şeylerin
(fikrimce) bir listesidir.  Hızlı Sürüm Yapmak Sıkça yaptığımız
sürümler (hattâ bazen bir iki gün içinde) projemizi hatasız ve
kullanıcarımızı 'ilgili" tutmanın en çabuk yoluydu diyebilirim.

Sık sürüm gören kullanıcılar projemizin aktif ve ivmeli olduğuna
kanaat getirdiler, ve projeye ilgili kaldılar. Ayrıca bir yazılımda
hangi özelliğin (functionality) kullanılan, ihtiyaç duyulan bir
özellik olduğunu anlamanın en iyi yolu, işleyen bir yazılımı şöyle bir
dışa açmaktır.  Regresyon Testleri Herhalde artık tüm Java cemaati,
otomize edilmiş birim testlerinin önemini biliyor. Çok sayıda ve
detaylı bir test havuzu, bizim projemizin kaynak kodlarının idare
edilir ve stabil hâlde kalmasında en büyük faktördür, çünkü projemiz
inanılmaz büyüklükte dizayn ve özellik listesi değişiklikleri
yaşadı.

Mentalite öyle olmalıdır ki, eğer bir özellik (feature) için bir test
yoksa, o özelliğin çalışır mı çalışmaz mı olduğu hakkında hiçbir
fikrimiz olamaz.

'Tek' Bir Şeyi 'İyi' Yapın

Bir şey konusunda en iyi olun. En iyi olamayacağınız şeylerde bırakın
öteki projeler iyi olsun.

'Aşırı Dizayn' Hastalığına Kapılmayın

Çok fazla soyutluk ve ürününüze çok fazla esnekliği projenin çok
başında tanıştırmak istemek, büyük bir zaman kaybı olacaktır.

Bu zamanı çok daha verimli bir şekilde kullanıcıların ihtiyacı olan
gerçek problemleri çözmek için kullanmalısınız. İşleyen en basit
çözümü kodlayın. Kullanıcıları alakâdar etmeyen problemleri çözmek
için uğraşmayın. Kod açısından gerçekleştirimin zarif olup olmadığı
HİÇ ÖNEMLİ DEĞİLDİR, en azından projenin en başında bu
böyledir. Önemli olan, "kullanışlı özellikleri" "zamanında"
kullanıcıya verebilmektir.  Merkezi Vizyon Bir komite ile karar
veriyor olmanız için, devasa bir projede olmanız lazım. Diğer çoğu
projelerde, bir iki tane açık fikirli insanın projenin vizyonunu ve
yönünü idare etmesi, geliştirmesi yeterlidir. Bu sayede proje tek bir
şeyi iyi yapmaya odaklanabilir. Kanımca açık yazılım projelerini
çökerten en yaygın sebeplerden biri özellik enflasyonudur (scope
creep).

Belgeleme

Belgelenmemiş özellik sözü, telâfuzu imkansız bir şey, anlambilimsel
bir imkansızlıktır. Eğer kullanıcılarınız bir özelliğin olduğunu
bilmiyorsa, o özellik, yok demektir. Ya belgeleyin, ya da kaynak
koddan özelliği atın. Kaynak kodunuzda yer işgal etmekten başka bir
şey yapmıyor.  Standartizm Hastalığından Korunun Doğru hazırlanmış
standartlar (meselâ JMS, JDBC) sistemlerarası çalışabilen programlar
ve taşınabilir kodlar için fevkalâde yardımcıdır, kötü hazırlanmış
standartlar yaratıcılığın ve teknoloji mucitleri için fevkalade bir
engeldirler. "XXX standartını destekliyor" gibi bir etiket, hiçbir
ürün için bir kullanıcıdan gelen istek/gereksinimin cevabı olamaz,
hele ve hele XXXX standartı bir "uzman" komitesi tarafından
hazırlanmış ve bu uzmanlardan hiçbiri kendi pişirdiği tatsız yemeği
yemek zorunda olmamamış insanlardan oluşuyor ise.

En iyi yazılım, deneme, yanılma ve deneyler ile
yazılmaktadır. Çoğunluğun standartı olarak bilinen de facto
standartlar, kullanıcılar için tepeden indirilmiş, apriori
standartlardan daha elverişli ve sağlıklıdır.  10 Dakikada
Hazır..İşliyor...Çalışıyor Müstakbel bir kullanıcının, bir yazılımı
indirir indirmez kurmak, ayarlamak ve bazı kuruluş hatalarıyla
uğraşmak için harcayacağı zaman, yarım saatten az olmalıdır, bundan
fazlası kullanıcıya fazla gelir. Bizim amacımız, yeni
kullanıcılarımızın demo'yu "5 dakikada" çalıştırabilmesidir (JDBC
bilgilerinin olduğunu farzederek), ve "yarım saatte" kendi Merhaba
Dünya programlarını yazabiliyor hâlde olabilmeleridir.

Programcılardan Cevapların Çabuk Gelmesi (Responsiveness)

Kullanıcılar bir problemle karşılaştıkları zaman, ki muhakkak
karşılaşacaklardır, ürünü yazmış olan programcıların cevap vermekte
hazır ve nazır olmaları gerekir. Kullanıcılar size belgelerinizdeki
boşlukları bulmakta yardımcı da olacaklardır (çünkü anlamadıkları bir
özelliği kullanamazlar). Kullanıcılar test havuzunuzun kaçırabileceği
kendini iyi saklamış hataları bulmanıza da yardımcı olurlar. Sonuçta
kullanıcısız bir proje çarçur edilmiş zamandan başka bir şey değildir.
Hatalar hakkındaki komik durum şudur: Aslında hata bulmak
kullanıcıları rahatsız etmiyor eğer bu hatalar çabuk tamir
edilirse. Çabuk hata onarmak (responsiveness), bir hatanın, bir
haftadan önce tamir edilebilmesi demektir. Genel onarma zamanı da
(rapor edilmesi ile tamirin CVS'e koyulması arasındaki geçen zaman) 24
saat olursa en idealidir.Çabuk Güncellenebilen Wiki Sayfaları..

Gavin King





