# Akamai Nedir

İnternet programları ile uğraşanlar, herhalde Akamai isimli bir servis
duymuşlardır. Projelerinizde bazen şöyle kelimeler duyabilirsiniz "Web
sayfalarımızı Akamai-ze edelim", "Akamai bağlantısı yapıldı mı?", vs.
Nedir Akamai? Bu çok ünlü olan servis, aslinda bir hızlı erişim
deposundan başka bir şey değildir. Mesela sayfalarınızda grafik
şekiller varsa, ve o sakillere şu şekilde ulaşıyorsanız

`img kaynak="images/mavi_resim.jpg"`

Sayfalar akamaize edildikten sonra,

`img kaynak="http://net.akamai.net/200101102233/www.siteminismi.com/images/mavi_resim.jpg"`

şeklinde bir kod kullanmanız gerekir.  Böylelikle bu resim bizim
siteden değil, Akamai şirketinin sitesinden kullanıcıya
gönderilir. Akamai büyük masraflar yaparak, İnternet'e çok güçlü
servis makinaları kurmuştur, ve ayrıca günün her saatinde İnternet'in
hangi bölümünün ne kadar meşgul olduğunu bilirler. Sizin sayfanız
Akamai servisinden grafik istediği zaman, kullanıcının nerede olduğuna
bağlı olarak, ona 'en yakın' yerden bu grafik ona verilir.  Her
isteyen tabii ki bir Akamai linki ekleyip, sayfalarını hızlandırmayı
beklemesin. Öncelikle Akamai ile bir anlaşma kurulması, ve bu servis
için para ödenmesi gerekir. Para ödendikten sonra Akamai şirketi,
yukarıdaki link için 'izin' verir, yani bilgisayarların da sizin için
yer açar. Grafikler ilk kez istenmeye başlayınca, Akamai grafik sizin
sitenizden alıp, kendi bilgisayarlarına koyacaktır.

Programcılar için bir uyarı: Sayfa kodlarken akamaize edilmiş kodlar
ile beraber kodlamak iyi degil. Yoksa, Internet bağlantısı olmadan
sayfa geliştirmesi yapmak zor, hem de sayfalar çok karışır. En rahat
olanı bizce, normal linkleri, anında akamaı-ze etmek. Bunu Sayfa
Servis Programımiz (Apache gibi), otomatik olarak yapabilir. Bu tür
işleme URL-rewriting denir, yani "o anda link değiştirme".
