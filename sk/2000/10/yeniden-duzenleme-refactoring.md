# Yeniden Düzenleme (Refactoring)

Extreme programming yönteminin diger tasarım/kodlama yöntemlerinden
büyük farkları vardir. Bunlardan en önemlisi, tasarım ne zaman ve ne
kadar yapilacağıdır.

Bildiğiniz gibi eski tasarım yöntemlerinde, başta uzun sureli tasarım
yapmak, ve ciltler dolusu tasarım belgesi ortaya çıkartmak gerekir. Bu
fikre göre, bu devre icin ne kadar uzun zaman harcanırsa, o kadar
iyidir.  Böyle projelerin başarı yüzdesi %50'dir. Sektörde bunun
birçok örneğini gördük ve yaşadık.  Bu yüzden yeni seçenek olarak
gelen her yöntem, ne kadar ve ne zaman tasarım yapılacağını
tanımlamalıdır.

Elimizdeki silahlar

Teknolojide ve diğer birçok konuda, "çapımızı" elimizde olan
yetenekler, bildiğimiz püf nokta gibi yararlı bilgiler belirler. Bu
noktalara dayanarak çözüm üretiriz. O yüzden, XP yönteminin tasarıma
yaklaşımı açısından, yeni bir 'numara' öğrenmek zorundayız. Bu numara
'yeniden düzenleme' denilen refactoring numarasıdır.

Yeniden Düzenleme

İlk once kabul etmemiz bir nokta: Hiç bir programcı bir
defada kafasında kurduğu tasarımı, hiç değisiklik yapmadan kodlayıp
ortaya çıkaramaz. Kafada kurulan tasarım ile, yazılan kod arasinda
'kodu yazarken' muhakkak bazı değişiklikler gerekecektir. Bunun
sebeplerinden biri, kod yazmanın gerçekle yüzyüze gelinen yer
olmasıdır, derleme yaptiğınızda sonuç ya, gecer ya kalır. Ayrıca, kodu
yazarken daha ilginç 'tasarım fikirleri' aklınıza gelecektir. Bu gayet
normal. Zaten tasarım/kodlama surekli bir davridaim icinde gecer, biri
ötekini etkiler.  İşte bu yüzden, XP yöntemi tasarım surada baslar,
surada biter, sonra kodlama baslar demez. XP'ye göre, programcı her
zaman tasarım yapar, her zaman kodlar. XP programcısı için kodlamak
bazen Lego gibi yap-boz ruhu ile uğrasılan, bazen çok ciddi
bağlantıları kurulan bir ortamdır.  Daha uzatmadan, öğretecegimiz
'numaraya' gelelim. Yeniden duzenleme, adindan belli oldugu gibi, kodu
yeniden duzenleyip tasarımını değistirmeye denir. Fakat bu oyle
yapilir ki, programin değisim öncesi ve sonrasındaki özellik
listesinde hiçbir değişiklik olmaz.

'Ozellik listesi niye degismez?'  diye bir soru sorulabilir. Bunun cok
pratik sebepleri var. En onemlisi sudur: Tasarım değişikligi program
yapisini değistirdiği icin, değişimin dogru yapılıp yapilmadığının
kontrolunun en rahat yolu özellik listesininin kullanıcı yüzünü
kontrol etmektir. Hatta ve hatta daha iyisi, sanal bir kullanıcı
sayılabilecek JUnit birim testlerinizi bu iş için kullanmaktır. (Zaten
yazılma amaçları da budur). Yani, test programlarını degisimden önce
ve sonra isletirsinizseniz ve testler, iki sefer de basari ile geçiyor
ise, demek ki tasarım değişikliginiz yeni hatalar yaratmamış.  Boylece
o hepimizin basina gelen, tasarım degistirken programa 'yeni hata
ekleme' olayindan korunuruz.  Tekrar soyleyelim: Yeniden duzenleme
teknigi icin test programlarinin varlığı ZORUNLUDUR.

Bazı programcıların yeniden düzenleme tekniğinden uzak durma ve hatta
secenek olarak bilmemesinin sebebinin altinda, test programı yazma
disiplinindeki eksiklik yatar. Eger elinizde test programı yoksa,
tasarım değişikliğinin doğru olup olmadığını nasıl anlayacağız?  Şimdi
yeniden düzenlemenin ABC'sine, yani alt duzey tekniklerine gelelim: Bu
tekniği uygularken yaparken amacımız şu olmalidir:

* Kod içinde tekrar eden olguların merkezilestirilmesi, yani tek bir
yerde toplanmasıdır.

* İşlemlerin ait olduğu nesneler üzerinde toplanmasıdır.  Bir örnek
yeniden düzenleme şöyle olabilir.

Mesela, Araba adlı nesnemiz var, ve Araba nesnesini kalıtım
(inheritance) ile uzatan Tofas adli nesnemiz. Bildiginiz gibi kalitim
ile kalitim yaptiginiz nesnenin islemleri size gecer. Yani Tofas
nesnesi, Araba nesnesinin butun islemlerini kullanabilir.  Simdi
diyelim Serce adli bir nesne yarattik. Birden farkettik ki, Serce
nesnesi ile Tofas arasinda benzerlikler var. Fakat Serce ile Tofas
arasinda tek baglanti Araba nesnesi! Ortak olmasi gereken islem Araba
uzerinde degil.  Bu gibi durumlarda, hemen acele bir yeniden duzenleme
yapip, ortak olmasi gereken islemi Tofas nesnesinden, Araba nesnesine
'yükseltmemiz' gerekir. Bunu yapinca artik bu isleme Serce'den erismek
mumkundur.  Bu islemi yaptiktan sonra, test programlariniz tekrar
isletmeyi unutmayin..


