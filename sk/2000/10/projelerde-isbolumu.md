# Projelerde İşbölümü

Programcıların arasında işbölümü yaparken gözetilmesi gereken birkaç
önemli noktadan bahsetmek gerekiyor.  Eğer projemizde XP yöntemi
izleniyorsa, işbölümünün yapılması özellik kartlarının programcılar
tarafından paylaşılmasının doğal bir uzantısı olacaktır.  XP yöntemi
kullanılmıyorsa, yapılması mümkün bir hataya karşı şu kıstası sunmak
istiyoruz: İşbölümü takım elemanlarının teknoloji bilgisine göre
değil, kodlayacakları özelliğe göre yapılmalıdır.  Örnek Mesela,
İnternet bazlı bir bilgi işlem projesinde, işbölümü JSP'ciler ve Java
Servlet/Bean'ciler arasında yapmak, akla ilk gelen ve doğru gibi
gözüken bir paylaştırım şeklidir. Fakat bu yöntemin takım için
sakıncalı olacaktır. Bu tip bir iş taksimi, teknoloji bilgi
sınırlarında yapılmış bir işbölümüdür, ve JSP'ciler ile Java'cılar
arasındaki iletişim miktarını gereğinden fazla arttıracaktır.  Bir JSP
sayfasının giriş olarak belli bildirgeçlere ihtiyacı vardır, ve sonuç
olarak bazı çıktılar verir.

Eğer bu verilerin değiş-tokuş yaptığı diğer Java nesnesinin yazarı
başka bir programcı ise, gereken iletişim, iki programcının iş
kısımlarının ilerleyiş/bitiş/devrediş zamanlarını birbirlerine
kitlemiş olur.  Ayrıca, görevin bitip bitmediği özellik bazında daha
rahat anlaşılabilir. Bir JSP sayfasına bakarak sayfanın bitmiş gibi
gözükmesi bizi aldatabilir. Fakat, sayfa daha tam arka plan Java
nesneleri ile tümleştirilmemiş ise, genel-i itabarı ile özellik
bitmemiştir. Fakat, her iki programcı da benim sayfam bitti, arkadaşın
Java nesnesi bitmedi, hata burada değil, orada, gibi bir gereksiz
düşünce egzerzisine girebilirler.  Eğer paylaştırımı özellik bazında
yaparsak, bu özellik için gereken JSP'leri, Java kodunu, veri tabani
SQL komutlarını tasarlayan/kodlayan tek programcı, işini yapması için
gereken bütün katmanlarda gezinerek, işini bitirdiğinde, tümleştirmeyi
bitirdiğinden daha emin olur.  Yani doğru olan, bir projede herkesin
JSP, ve herkesin Java kodları yazabilmesidir. Yazmayı bilmeyenler,
ikili kodlama tekniklerinin yardımı ile diğer teknolojiyi
arkadaşlarından öğrenmeli, ve her iki çeşit kodlamayı yapar hale
gelmelidirler.

Bu, hem kod tecrübesinin proje takımları arasında yayılması açısından,
hem de koda bakım yapabilecek programcı sayısının artması bakımından
zaten yararlı bir sonuçtur.  İletişim, bir yazılım projesinden
muhakkak zâti önem taşır, ve gereken konularda had safhada
arttırılması gerekir. Fakat, eğer bazı yerlerde iletişim kurmadan iş
görebiliyorsak, yani tek bir kişinin, gereken yerlere gereken kodları
koyarak işi bitirmesi mümkün ise, o zaman bu tek kişiye görevi vermek
asli amacımız olmalıdır.  Çok katmanlı bir J2EE projesini çok katmanlı
bir pastaya benzetirsek (her katmanın değişik bir tatlı olduğunu
düşünelim), işbölümü yaparken pastayı dikey olarak kesip, bu dilimleri
misafirlere dağıtmak gerekir. Bu şekilde bir dağılımda herkesin dilimi
içinde her çeşit tatlıdan bir miktar mevcut olacaktır. Tatlılar
teknolojiler, dilimler de özellikler olarak düşünülebilir.  İstisna
Kuralımıza tek istisnai durum, ufak bir alanda tek kişinin uzmanlık
bilgisini kullanıldığı alanlarda olabilir.  Fakat takımın çoğunluğunun
çalıştığı türden görevleri paylaştırma konusunda, önceden açıklanan
kural geçerli olmalıdır.

![](cake.jpg)

