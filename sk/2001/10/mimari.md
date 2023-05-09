# Mimari

Mimari kurmak, mimariye karar vermek gibi terimleri bir yazılım
projesi sırasında, ya da yazılım mühendisliği hakkındaki makalelerde
çok görüyoruz. İlk bakışta uyanan intiba, "mimarinin teknik lider
seviyesinde, ya da her programcının yapamayacağı türden birşey"
olduğudur. Bu intiba, bir ölçüde doğru olabilir, çünkü etkili mimari
yapı tecrübe gerektirir. Birçok diğer türden mimariyi başarısı ve
hataları ile görmeden, mimariler arasında seçim yapmak zordur.
Öncelikle mimarinin ne olduğunu tanımlayarak başlayalım. "Mimari, bir
projenin oyunu oynama kuralları + teknoloji seçimi ve kullanacağı tüm
tasarım kalıplarının toplamıdır" diye bir tanım yaratabiliriz.
Aslında ikinci ve üçüncü noktalar, dönüp dolaşıp oyun kurallarına
bağlanır. Yani ikinci nokta olan teknoloji seçimi, oyun kurallarını
değiştirdiği ölçüde mimariye dahildir, mimariyi etkiler.  Daha detaya
inersek, şimdi oyun kurallarının ne olduğunu tanımlamamız
gerekir. Oyun kuralları, bir genel soruna verilmiş olan genel bir
cevaptır.

Mesela, bütün Java nesnelerimiz veri tabanına şu şekilde yazılsın
şeklinde yaptığımız bir beyân, mimari bir seçimdir. Diyelim ki, her
kalıcı Java nesnesi public interface KalıcıNesne {public void
setBaglanti(Connection conn);public Resultset oku();public void
yaz();public void sil();} .. gibi bir arayüzü gerçekleştirmeli
(implement) diye bir kural koyabiliriz. Bunu yaparken bir mimari seçim
yapmış oluruz, eksiği ve fazlası ile bu secimi, bu genelleme ile
tartışabiliriz. Mesela, üstteki mimari seçim yerine, "artık JDO ya da
Hibernate adlı kütüphaneler var, bu oku(), yaz(), sil() gibi gereksiz
kodlamaları yapmamıza gerek yok" gibi bir tartışma başlayabilir. Bütün
bunlar mimari tartışmalardır. Gördüğünüz gibi, belli bir işlevin
"algoritmasını" tartışmıyoruz.

Birçok nesnenin arayüzünü nasıl kurulacağını belirleyecek genel
kurallar saptamaya uğraşıyoruz.  Mimarilere gereksinimimiz, aslında,
insanların belleğinin sınırlı olması sebebiyledir. Eğer beynimizde,
proje sırasında karşımıza çıkan her soruna değişik bir türden
bulduğumuz bir cözümü tutabilecek kadar geniş olsa idi, ve bu çözüm
demetini anında başka bir insana insanüstu bir iletişim yeteneği ile
aktarmamız mümkün olsa idi, mimariye ihtiyacımız olmazdı.  Fakat durum
böyle değildir. Bu yüzden, bütün proje kodunu rahat bir şekilde tasvir
(genellemeler ile) ve kendimiz açısından hatırlayabilmemiz için mimari
kurarız. Ayrıca, kodlama hızı bakımından sürekli tekrarlanan
problemlere sürekli aynı cevabı vermek, kodlama hızımızı arttıracak,
projeye verim artışı sağlayacaktır.  Bir diğer fayda da, yazılım
takımındaki her programcının, yazılımın her tarafı üzerinde rahatça
çalışabilmesine imkan vermesidir. Eğer bütün programcılar, mesela
görsel bir Swing tablosunu veri tabanına bağlarken aynı kaideleri
takip etmişseler, birbirlerinin tablo kodları üzerinde değişik
zamanlarda birbirlerinden habersiz çalışabilirler. Yani, "sandalye
değiştirmek" denilen birbirinin yazdığı kod üzerinde çalışmak ve bakım
yapmak, mimari üzerine kurulmuş olan bir kod bazı üzerinde çok daha
rahat olacaktır. Bunun da verimlilik olarak takıma getirisi çok
önemlidir.

Extreme Programcılık ve Mimari

Şimdi gelelim XP projeleri için mimarinin önemine..  XP tarihi
yazısında Extreme programcılığın aylarca hiç kod yazmadan sürekli
mimari/tasarım kurmaya uğraşan, ve dolu belge sıfır kod üreten
rojelere/tekniklere karşı oluşturulduğunu söylemiştik.  Tabii bu
tavsiyeleri okuyarak, hiç mimari yapmadan direk kodlamaya da atlamak
yanlış olur. "Peki karar noktası nerededir? Ne kadar mimariyi, ne
zaman, ne kadar süre ile tasarlamak gerekir?" gibi bir soru
cevaplamamız gerekiyor.  Teknik liderlere: Aklınızda aşağı yukarı bir
mimari sürekli olsun. Bu mimarinin kağıt üzerinde olması
gerekmez. Eğer müşteri illa ki görmek istiyorsa, ballandırarak ve
teknolojik bilginizi konuşturarar bu aklınızdaki mimariyi "genel
hatları ile, arayüz seviyesine fazla inmeden" anlatabilirsiniz. Fakat
yazılımı geliştirmek için bunun pek bir faydası yoktur, sadece iyi bir
gösteri olur.

Yazılım sırasında aklınızdaki bu genel mimariyi kullanarak,
programcılarınız sizden tavsiye istediğinde, genel hatlı bu mimari
ışığında tavsiyelerde bulunabilirsiniz. Zaten, takım iletişimi iyi ise
ve günlük toplantılarda teknik bilgi alışverişi yapıyorsanız, bilgi ve
yöntemler kendiliğinden takıma yayılacaktır. Takım lideri olarak arada
sırada koda bakarak, istediğiniz kalıpların kullanılıp
kullanılmadığına bakabilirsiniz. Eğer kullanılmamış ise direk baskı
ile programcının değiştirmesini istemeyin; yeniden düzenleme tekniği
ile birim testlerin koruması altında programcılar kodun bu bölümüne
tekrar geldiğinde gerekli değişikliği kendiliğinden yapacaklardır.
Programcılara: Arayüz seviyesindeki (çok önemli) tasarımı tanımlamak
sizin göreviniz. Bunu yaparken, öncelikle elinizdeki özelliği
kodlayacak kadar bir tasarım oluşturun. Kodlamayı tecrübenizin
elverdiğince okunur, ve az satır kod ile yapmaya gayret edin, nesne
tasarımı anlaşılır olsun. Fakat kırk hafta sonrasını görmeye
uğraşmayın. Unutmayın, kırk hafta sonra kodlanacağını zannettiğiniz
bir özellik, müşteri tarafından projeye dahil bile edilmeyebilir.  Bu
yüzden bu gün için, bu güne yetecek kadar tasarım yapın. Bu tasarımı
yaparken tabii ki gerekli takım arkadaşınız ile konuşabilir, eş
programcınız ile tartışabilir, arayüzünü kullandığınız programcılara
soru sorabilirsiniz.

Bu ilk problemi kodlayıp, işi bitirdikten sonra, aynı çözümü
kullanabilecek yeni bir özellik ile karşılaşabilirsiniz. O zaman
mevcut olan kodunuzu bu yeni özellik için kullanıp kullanamacağınıza
bakın. Eğer çok ufak değişiklikler ile çözüm halâ geçerli ise (düşük
bir ihtimal) hemen eski kalıbı yeni özellik için hemen kullanabilir,
ve ufak bazda bu mimari seçimi koda dahil edebilirsiniz.  Eğer önceki
çözümün yeni özelliğe göre değişmesi gerektiriyorsa, bu değişimi
yapın, ve eski/yeni kodu bu yeni ortak koda göre tekrar yazın.  Şimdi
dikkat: Bu yaptığınız seçim, ufak seviyede bir tasarım, ya da geniş
anlamda herkesin kodunu etkileyecek şekilde bir mimari seçim
olabilir. Bunu takıma tanıtın, hatta daha da önce, genel kurallara
yansıyacak türde bir seçimi teknik lider tarafından anlaşılmasına ve
fikrinin alınmasa gayret edin. Eğer bu konu hakkında daha önce kural
konulmamış ise, teknik lideriniz ya bu konuya daha eğilmemiş, ya da
daha öncesi için önemsiz bulmuş olabilir.  Eğer bu doğru ise, düzgün
iletişim ile yeni mimari kuralları tartışın ve uygulamaya koymaya
başlayın.





