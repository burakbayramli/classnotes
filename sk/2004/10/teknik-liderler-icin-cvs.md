# Teknik Liderler İçin CVS

CVS adlı kaynak kod idare sisteminin, her bilişim projesinin teknik
lideri tarafından bilinmesi gereken bazı özellikleri vardır. Bunlar
projenin başında yapılan kod ithali (import), proje sırasında kod
bazını etiketleyebilmek, ve lazım olursa kod ihracı (export)
yapabilmektir.

Cvs Import Kod ithali için cvs import komutunu kullanabilirsiniz. Bu
komut, genelde projenin ilk başında gerekli olur. Teknik lider
projenin dizin yapısını, ant betiklerini, birkaç gereken Java
dosyasını, ve jar dosyalarını kendi yerel makinasında hazırladıktan
sonra bu yapının tamamını CVS'e koymak isteyecektir. O anda CVS
bomboştur, ve ilk giren kod bu kodlar olacaktır.  Bu durum tipik bir
cvs import gerektiren bir durumdur. Teknik lider, eğer projeyi
/usr/local/proje1/ altımda kurdu ise, önce o dizine gider, ve şu
komutu işletir.

cvs import -m "Proje Ekleniyor" havuz_ismi vendortag releasetag

Bu komut, havuz_ismi için ne kullanıldıysa, o isimde bir kod havuzu
CVS'te yaratacaktır, ve /usr/local/proje1 altındaki bütün kodları
oraya koyacaktır.  Bu komutu kullanırken, CVSROOT değişkeninin doğru
ayarlanmış olduğunu farzediyoruz. CVS Kurmak adlı yazımızda, havuzun
fiziksel adresini taşımak için -d yaklaşımı yerine CVSROOT'un daha iyi
olacağını belirtmiştik.  Bir önemli not daha: cvs import, (teknik
lider için) /usr/local/proje1 dizinini geliştirme yapmaya hazırlamaz,
sadece o kodu içeri koyar. Yani, o kodu içeri koyan teknik lider cvs
commit, cvs update gibi komutları kullanmak istiyorsa, önce cvs co
havuz_ismi komutunu işletmeli ve kodu aynen öteki programcıların
yapacağı gibi dışarı çekmelidir. Bunu ya aynı dizinde, ya da başka bir
dizinde yapabilir.

Kod Etiketlemek

Teknik lider, ya da onun eğittiği ve görevlendirdiği projede idari
işlere bakan arkadaş, ne zaman test için önemli bir sürüm yapılmışsa o
sürümün referans aldığı o anki kod durumunu "dondurmak" için, CVS'te
bir etiket atmalıdır. Etiket atmak, bir nevi koda işaret
bırakmaktır. Bu işarete sonradan dönülebilir, hattâ sadece işarete
yönelik cvs update işlemleri bile yapabilirsiniz. Fakat geri dönmek,
veya hatırlamak için bu işareti bırakmak daha yaygın bir yaklaşımdır.
Etiket atmak için, cvs tag surum_ismi .. komutunu kullanabilirsiniz.
Teknik lider, ne zaman müşteriye ve ya büyük bir test yönelik bir
sürüm yaparsa, bu etiketleme işlemini gerçekleştirmelidir. Etiket
isimleri, surum_1, surum_2, gibi isimler, ya da sadece bir numara
olabilir. Tarih içeren sürüm isimleri de görmüştük.  Daha büyük
projelerde, etiketle isleminin hata takip programı ile alâkasının
kurulması gerekebilir.

Öyle ya, bir sürüm yapıldı, test makinasına koyuldu, testçi
görevlilere de sürekli hata raporları geliyor. Test edilmeye hazır
hatalar, hangi sürümde test edilmelidir? Belki programcı sürüm 11'i
(örnek) kaçırmıştır, ama yine de hatasını test edilmeye hazır olarak
hata takip programında testçiye göndermek istiyordur.  Bu gibi
durumlarda, ITracker programının da desteklendiği gibi, hataların
içinde hangi sürümde test edileceği bilgisi kaydedilmelidir. Bu sürüm
no'su, teknik liderin CVS'te attığı etiket numaralarını baz alarak
girilen bir numara olacaktır.  Testçilerin de o anda "hangi sürümde
olan bir programa" baktıklarını anlayabilmeleri için, teknik liderin
etiketle işlemini bir betik içine alıp aynı betiğe bir de statik bir
HTML sayfası ürettirmesi uygun olabilir. Bu HTML sayfası, hep aynı
yerde ve hep aynı isimde olacak bir sayfadır, ve web uygulamasının
dizinine koyulabilir. Bu sayfanın nâdide içeriği, test edilen o anki
kodların sürüm numarasından ibâret olacaktır. Böylece hep aynı HTML
sayfası ziyaret edilerek hangi sürümde olunduğu testçiler tarafından
rahatça anlaşılmış olur.

Cvs Export

İhraç komutu, başka birine göndermek için kod ihraç etmeniz
gerektiğinde lâzım olacak. Önce ihracın yapıldığı "anı" hatırlamak
için bir etiket atmanız yerinde olur. Sonra, meselâ etiket1 gibi bir
değerin kullanıldığı kodu ihraç etmek için şu komut yeterlidir.

cvs export -r etiket_ismi havuz_ismi


