# XP Tarihi

Extreme programcılık, 'önce uzun uzun tasarlama, sonra kodlarsın'
fikrinin projelerde başarısız olması ile şekillenmeye
başladı. Arkasındaki ilgi, özellikle .com yıllarında daha
artmıştır. Bunda, .com projelerinin zorluk ve zamanlama açısından
yazılım takımlarına daha büyük yük getirmesi muhakkak rol oynadı. .Com
devrinde herkes, piyasaya bir an evvel çıkabilmek için, programcılara
ve danışman şirketlere büyük baskılar uygularken, bu baskıların
sonucunda, bitirilmesi için 10-11 ay gerektiren proje idare yöntemleri
tabii ki 'çatlamaya' başladı.

Eski yöntemler yerine yeni bir yöntem bulmak gerekiyordu, ve XP bu
boşluğu doldurdu.  XP'nin bir ayrı doğuş noktası, Kent Beck'in müşteri
ve programcı gurupları arasında çok katı çizgiler çizmek
istemesidir. Kent, bu iki gurubun birbirinin işlerine karışan ve
sonuçta projenin seyrine köstek olan davranışlarını kaldırıp, yerine
herkesin kendi işini gördüğü, ve kimsenin kimseye stres yaratıp, baskı
uygulayamayacağı bir ortam yaratmak istedi. Çünkü projeyi
bitirip/bitirmemek bir yana, Kent programcıların mutsuz ve motivasyon
eksikliği yaşadığını farketti, ve altta yatan bityeniğini bularak
çözümü sağladı.  Müşteri/programcı çizgileri çizilince, müşteri
projenin nereye gideceğinin yegane sorumlusu oluyor, fakat aynı
zamanda, hiçbir şekilde programcıya 'şu işi daha hızlı yap' deme
hakkına erişemiyordu.

XP, her dönemde işin ne kadar hızlı yapıldığını ölçerek, sonraki
dönemler için ne kadar iş planlanacağını tahmin etmeye uğraştı. Bir
kere tahmin yapıldıktan sonra müşteri, özellik listesini ona göre
çıkarıyor, ve sonra 'müşteriliğini bilerek' daha fazla projeye
karışmıyordu. Çünkü programcılık teknik bir olay idi, ve özelliklerin
ne kadar hızlı yazılıp yazılmayacağı teknik dünyaya ait bir karardı.
Ayrıca, XP'nin teknik kodlama süreci/yöntemleri alanında da hayati
tavsiyeler getirdiğini görüyoruz. Kent, anlattıgı bir hikayede,
rastladığı bir programcının öteki arkadaşlarına sürekli fark attığını
gözlediğini söyler. Bu farkın altında, programcının her yazdığı 3
satıra takabül 1 satır da birim testi yazdığını farkedince Kent, bu
yönteme daha dikkatle bakmaya başladı. Genelde programcıyı
yavaşlatılacağı düşünülen test yazmak eylemi, uzun vadede test eden
programcının lehinde büyük farklar doğuruyordu.

Çünkü test yazan programcı, kodunu daha cesurca değiştirebiliyor, ve
mimari değişiklikleri bile gözünü kırpmadan yapabiliyordu. Öyle ya,
eğer mimari değişikliği yaparken bir hata eklenmiş olsa, derlemeden
sonra sürekli işletilen birim testleri, bu yanlışı anında
yakalayıyordu!  Ve Kent, bu tekniği de XP dağarcığına kattı.  Mekan
Her hikayeye bir mekan gerekir. XP'nin mekanı, C2 projesi denen
General Motors şirketinin bordro kontrol projesidir.  Bu proje, mimari
olarak duvara tosladıktan sonra, projede olan Kent, uzun süredir
planlamakta XP'yi deneme vaktinin geldiğini anladı. General Motor
şirketi yönetiminden izin istedi, ve kolları sıvadı.  Kent ilk önce,
en büyük sorun olarak gördüğü iletişim sorununu çözdü.

Programcılar ayrı odalarda, ya da aynı odada olsalar bile aralarında
plastik duvarlarla ayrıli 'hücrelerde' iş yapmaya uğraşıyorlardı, ve
tabii ki iletişim kurmak zor oluyordu. Yazılım projelerinde herkesin
bildiği bir vaka şudur: Yanınında olmakta olan bir konuşma, sizin için
önemli bilgiler içerir, kulak kabartırsınız, ve gerekli ise konuşmaya
katılırsınız. Bu gibi bilgi paylaşımı için hücre sistemi ölüm
demekti. Ya da, herhangi birine bir soru sormak isteseseniz, ama soru
sormak istediğiniz kişi 50 metre ötede olsa, soruyu sormak için ne
kadar zahmete katlanacaksınız? Kafanızı kaldırıp azıcık sesli olarak
arkadaşınıza bu soruyu sormak daha iyi olmaz mıydı?  Ufak gibi gözüken
bu tip şeyler, çok hızlı gerçekleşen yazılım projeleri için önemli
faktörlerdi. Yazılımda, bilinen türden hammadde ve sanayii yoktur,
herşey düşünce ve iletişim bağlamında gerçekleşir. Bu yüzden tek bir
soru soramamak, hatalı bir kod ile hatasız arasındaki farkı
belirleyebilir.  Bunu farkeden Kent, hemen herkesi aynı odada topladı,
ve hücre duvarlarını kaldırdı.  Bir sonraki değişiklik, sürekli
tümleştirme denen teknik akabinde meydana geldi. Kent, genelde XP
yöntemini şöyle tarif eder. "İyi olduğu bilinen yazılım tekniklerinin
sesini, sonuna kadar açmak".

Gereksiz bir ifade gibi gelebilir, fakat örneklerle daha
berraklaştırmaya çalışalım. Herkesin bildiği programcılık teknikleri
arasında, bütün proje kodunu derleyerek, tümleştirmek (entegrasyon) ve
testlerden geçirmek gelir. Yani, eğer programımızın kullandıği
arayüzleri doğru kullanıp kullanmadığımızı anlamak istiyorsak, bütün
kodu beraber derleriz, ve testlerden geçiririz.  Bu işlemi yapmak
'iyi' bir teknik olarak bilinir. Eğer iyi ise, niye her zaman
yapmıyoruz? Kent, tümleştirme işini bir otomatik programa bağlayarak,
bu programın her yarım saatte bir bütün kodu otomatik derleyerek
sonuçlarını projeye e-posta ile bildirsini sağladı.  Bunun sonuçları
tabii ki devrim oldu. Artık, eğer kaynak kod idare (KKİ) sistemine
yanlış, hatalı, yanlış tümleşen bir kod parçası eklenmiş ise, otomatik
derleyici bunu en az yarım saat içinde yakalayabiliyordu.

Hata e-posta ile alındığında, proje takımı o anda yaptığı işi bırakıp,
tamamen o hatayı düzeltmek ile uğrâşacaklardı.  Böylece, tümleşme
hataları taa sonuç ortamına aktarıldıktan sonra değil, hata olduktan
30 dakika sonra yakalanıyordu.  Bir başka örnek kod kontrol sırasında
oldu. Arada sırada başkasının kodunu kontrolden geçirmemiz, ve stil,
teknik olarak bulduğumuz düzeltmeleri arkadaşımıza bildirmemiz de
'iyi' bir teknik olarak bilinir.  Fakat normal projelerde bu teknik,
genelde takım liderinin insiyatifi ile başlatılır, ve bir odaya
doluşulur, yazıcıdan basılmış koda herkes bakar, ve fikirlerini
söyler, sonra herkes odasına döner düzeltmeler yapılır.  Kent şöyle
dedi: 'Bu iyi bir teknik ise, niye her zaman yapmıyoruz?'.  Sürekli
kod kontrolu yapabilmenin en iyi yolu olarak, tek kişinin kod yazması
yerine bir başkasının sonradan kontrolü yerine, iki kişinin aynı anda
kodlama yapması, ve sürekli birbirlerini kontrol etmesi fikrine
erişti.  Bu yaklaşım da yazılım projelerinde bir devrim oldu.





