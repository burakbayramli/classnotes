# Şirketlerin 'Bulut'una Karşı Diaspora

Gittikçe daha fazla yazılımın buluta (Amazon EC2, Google App Engine)
ve sosyal iletişim bilgisinin sosyal ağ şirketlerine (Facebook,
Twitter) kaymaya başlaması ile birlikte, kışisel verilerin, yazılımın
kime ait, kimin kontrolü altında olduğu konusu tartışılmaya
başlandı. Facebook'un ikidebir değisen gizlilik politikaları bu
tartışmalara yakıt veriyor.Bu konuya yeni bir yaklaşım Diaspora adlı
yazılımı ortaya çıkarttı.

Yazılımın ruhu aslında Bittorrent, Tor mentalitesiyle benzeşiyor
(hatta ileride Diaspora'nın bu hizmetleri kapsayabileceği de
söylenmekte); eğer İnternet'in temeli dağınık yapıda olması ise, o
zaman herkes -kendimize başta olmak üzere- sosyal ağ hizmeti, herkes
web hizmeti, herkes e-mail hizmeti, herkes veri depolama hizmeti
veremez mi?Daha detaylandıracak olursak; Facebook örneğini ele
alalım. Diaspora bu hizmeti şöyle halledecek.

1) Herkes bir Diaspora ağında hem kullanıcı hem servis işini görecek,
yani bilgisayarınızda bir servis, bir kullanıcı programı işliyor
olacak

2) Kişisel veriler kullanıcının kendi bilgisayarında tutulacak,
kullanıcı istediği kadar kışisel bilgiyi istediği şekilde dışarı
açacak.

3) Diaspora protokolü kışisel veriyi yakınındaki diğer makinalara
-şifrelenmiş halde tabii- yedekleme amaçlı kopyalayabilecek

4) İletişim olabildiği ölçüde birebir makinalar arasında
halledilecek.Bu durumda, bir Web sitesi işletmek te kendi
bilgisayarınızda Web sayfaları bir yerlere koymak kadar basit bir
işlem olabilir.

Diaspora her türlü hizmeti dış dünyaya açabileceği için, bu sayfaların
Web üzerinden iletişimi de bu "koca bulutun" içinde
halledilecektir. Peki insanlar bu bulutu performans amaçlı istismar
edebilir mi? Bunun önüne bant genişliği bağlamında "verdiğinden
fazlasını alamayacağını" garanti eden basit kontroller
halledebilir. Donanım zaten o kadar ucuzlasti ki, insanlar kendi satın
aldıkları ufak bilgisayarları evinde Diaspora network'üne dahil ederek
işlem kapasitelerini arttırabilirler. Diğer yandan istismar
edilemeyecek statik sayfalar, kullanım durumuna göre -çok erişilenler
için mesela- diğer Diaspora makinalarına kopyalanarak, satha yayılarak
erişimleri daha da hızlandırılabilir.

Şu anda Akamai, Dropbox gibi şirketlerin verdiği hizmetin bir
benzerinden bahsediyoruz.Email gibi bir servis, zaten noktadan noktaya
olduğu için Diaspora bunu halleder.  Hedef noktadaki makina açık
değilse, coğrafi olarak ona yakın olan Diaspora makinalarında geçici
olarak mesajlar -şifrelenmiş olarak- bekletilebilir. Yani İnternet'in
iletişim paketleri için çok alt seviyede yaptığının benzeri, daha üst
seviyedeki iletişim protokolleri için, benzer bir şekilde
halledilebilir. Bu İnternet'in ruhuna uygundur.Olası bir Diaspora
network'un gücü tabii ki kullanıcı sayısıyla orantılı olacak. Fakat
şimdiden böyle bir ağı kullanmanın avantajı var. Diaspora ilk sürümü
çıkarttı, bu sürümda tüm sosyal ağlardaki hesaplarınıza bağlanıp,
mesajlarınızı toplayıp, eklemenizi sağlayabiliyor. Böyle bir üçüncü
parti entegrasyonu, partiler arası geçişi de kolaylaştırıyor -
Flickr'a bir resim eklemeniz, Twitter hesabınızda bir bağlantı
paylaşılmasını sağlayabiliyor mesela.Yazının başında bahsettiğimiz Tor
servisi, benzer şekilde, Diaspora içinde halledilebilir.

Çin gibi yasakçı ülkelerin erişim kontrolleri Diaspora üzerinden
delinebilir. Şu anda bu Tor ile de yapılıyor, fakat Diaspora daha
fazla ek hizmet vereceği için, daha fazla kullanıcıya sahip olacaktır,
ve daha fazla kullanıcı daha güçlü bir ağ demektir, bu yasakçı
kontrollerin delinmesini kolaylaştırır. Zaten yapılan iş yazılım
seviyesinde benzer olduğu için aynı protokol üzerinden bu hizmetin de
verilmesi mantıklı olur.Yani gelecekte, şu anda bilgisayara bir
işletim sistemi kurduğumuz gibi, bilgisayarı alır almaz bir Diaspora
sistemi de kuruyor olabiliriz - böyle bilgisayarları kurulu halde
alıyor da olabiliriz.

İlginç bir şekilde Diaspora aslında Tim O'Reilly'nin bahsettiği
İnternet İşletim Sistemi'nin ta kendisi. O'Reilly bir PC işletim
sisteminin o makinanın iç kaynaklarını idare etmekte olduğundan
hareketle, şu anda bulutta gözükmeye başlayan hizmetlerin /
kaynakların böyle bir kontrolör sisteme ihtiyacı olduğunu ileri
sürmekteydi. Fakat o, bu işletim sisteminin tek bir şirket tarafından
yukarıdan aşağıya entegre olmuş şekilde ortaya çıkarılabileceğini öne
sürüyordu. Diaspora bu işi dağınık bir şekilde halletmenin yolu
olabilir. Ayrıca Diaspora'nın yaygın olduğu bir dünyada İOS tek bir
şirkete ait olmayacaktır. Bulut hiç kimsedir, aynı anda bulut
herkesdir.Bu çözüm şu anda bulut servislerinin müşterisi olan
şirketler için tercih edilir olabilir; çünkü verilerini kendi
bilgisayarlarında depolamaya devam ediyor olacaklar. "Başkasının"
makinasına kopyalamaları gerekmeyecek.

Peki böyle bir ekosistem içinde inovasyon olabilir mi? Mesela Google
App Engine benzeri bir servis, olası bir Diaspora ağında çıkabilir
miydi?  Buna cevap çok basit. Şu an bulut servisleri veren şirketler
de zaten aynı temel yazılımları kullanıyorlar. App Engine içinde
Python, Django, Linux gibi yazılımlar var. Bu yazılımları herkes kendi
bilgisayarında işletebilir / zaten işletiyor. Diaspora sadece, bu
örnek bağlamında, port 80 erişiminizin güvenli bir şekilde dış dünya
ile paylaşılmasını kolaylaştıracak.Sistemin bir dezavantajı şu
olabilir: verinin aynı yerde olması sayesinde verilebilecek ek
hizmetlerin verilebilmesi zorlaşabilir.

Mesela Linkedİn herkesin iş geçmişiyle alakalı verileri bir yerde
depolayabildiği için bazı ek hizmetleri verebiliyor. O veri üzerinde
veri madenciliği (data mining) algoritmaları işleterek bazı
iyileştirmeler yapabiliyor, bazı ek mesajlar gönderebiliyor, vs. Bu
tür hizmetleri verisi esasen dağınık şekilde olan bir Diaspora ağında
vermek ne kadar kolay olur? İmkansız değil muhakkak, fakat düşünülmesi
gereken konular. Belki de Linkedİn gibi bir şirket Diaspora ağında
insanların iş geçmişi ile alakalı açık verinin 'müşterisi' haline
gelebilir, ve bu veriyi gezerek, kendi üç noktasında toplayarak
istediği ek servisleri verebilir.

Bu durumda isteyen herkes bir LinkedIn olabilir, en azından "iş
geçmişleri hakkında" veri tekeli Linkedİn elinde olmaz.İnternet
reklamcılığı bile Diaspora üzerinden mümkün olabilir. Zaten şu anda
tek bir reklam idarecisi yerine (mesela Adsense) pek çok idareciyi
birleştiren reklam ağları (ad network) daha iyi işlemekte. Tek bir
idareciye bağlı sayfaların reklam içerikleri her zaman dolmayabiliyor
(fill rate denen durum) fakat Mobçlix denen bir şirket pek çok
idareciyi birleştirerek yüzde 100 doluluk oranı garanti edebiliyor. Bu
durumda esas olarak "herkesi" temsil eden bir ağ sistemı olarak
Diaspora aynı şekilde optimal reklam servisini sağlıyor
olabilir. Diaspora'nın içinden çıktığı serbest yazılımın hep para
kazanmaya ve ticarete karşı olduğu düşünülür, fakat Richard
Stallman'ın kendisi dahil böyle bir anti-ticaret duruşun kültürün
çıkış noktasıyla alakası yoktur.





