# XP Planlaması

XP kullanmaya karar verdiniz, ve proje lideri, programcı olarak nasıl
işleme koyacağızı düşünüyorsunuz. Nereden başlayacağım sorusu aklımıza
geliyor. İlk önce, kavram olarak 'iş süresi' ve 'zaman' terimlerini
yerlerine koyalım.  XP'ye göre, bir proje dönemlere ayrılır
(iteration).

Bu dönemin uzunluğu, bir kere kararlaştırıldıktan sonra sabittir,
değişmez. Mesela, sizin takımınız için 2 haftalık döneme karar
kıldınız, bu şekilde başlayabilirsiniz. Her takım için bu dönem süresi
değişik olabilir. Bu tamamen takımınızın ritmine bağlıdır. Unutmayın,
daha hızlı takımlar daha ufak dönem seçer diye bir şey yoktur. Sadece,
proje hayatınızı ne kadar ufak parçalara bölmek istediğiniz
önemlidir. XP takımlarından alınan istatistiklere göre, genelde iki ya
da üç haftalık dönemlerin tercih edildiğini görüyoruz.  Eğer dönem
süreniz 2 hafta ise, demek ki 3 aylık olarak görülen bir projenin
içine 6 dönem sığacak demektir. Tabii proje eğer 3 ayda bitmemiş ise,
yeni dönemler ekleriz. Hemen belirtelim: XP'de sabit zamanlı, sabit
bitiş çizgili proje yoktur.

Eğer bu tür bir ortamda çalışıyorsanız, arkanıza bakmadan
kaçabilirsiniz. Bu satırların yazarı dahil birçok kişi sabit bitiş
çizgili projelerde yeterince kan ve ter kaybetmiştir. Eğer böyle bir
kültür içindeyseniz, kültürü XP ile değiştirmeye uğraşın. Amerika'da
bu tür götürülen projelerin yarısı başarısız olmuş, başarılı
olanlarında kod kalitesi şüpheli bir seviyede olmuştur. Neyse..XP'ye
dönelim; Dönem süresi karar kılındıkta sonra, müşteriyi temsil edecek
birini seçin. Bu müşteri temsilcisi, projeye hangi özelliklerin
ekleneceğini, hangi özelliğin ötekinden önce yazılacağına karar veren
kimse olacak. Bu tür kararlar, yazılıma talip şirket (yani
programcıların müşterisi) için stratejik türden kararlar olduğu için,
bu şahsın şirketini ve iş alanını çok iyi bilmesi yararlı
olur. Yazının geri kalan kısmında, bu tek kişiyi 'müşteri'
betimleyeceğiz.

Planlama dahilinde, müşterinizin istediği özellikleri bir liste olarak
dökmesini isteyin. Siz olarak hitap ettiğim, XP takımının teknik
lideri olan sizsiniz. Müşteri bu listeyi en önemliden en önemsize
doğru sıraya dizmelidir. Bu özellikleri ya bir Excel hesap tablosu, ya
da her özelliği tek bir kart üzerine yazılmak suretle kartlar kümesi
olarak tutabilirsiniz. Hattâ, şirket içi İntranet üzerinden
kullanılabilen XPlanner gibi bir program da yararlı olabilir.  Kartlar
hazır olduktan sonra, bir takım toplantısı tertip edin. Bu toplantıyı,
her dönem başında yapmanız lazım. Toplantıya müşteriniz ve bütün
programcılar katılacak. Toplantı başında kartları masaya dizin, (ya da
Xplanner'dan ekrana getirin) ve kartları en önemliden en önemsize
doğru tartışmaya başlayın.  Müşterinin yapması gereken, her kartı
alıp, takıma okumaktır. Bu özellik hakkında detaylı bilgiler de
verilebilir.  Özellik tanımı bittikten sonra, programcılar kartı
geliştirme kalemlerine bölmeye başlarlar.  Özellik:Kullanıcı, banka
hesapları arasında para transferi yapmak istiyor.  Programcılar,
geliştirme kalemlerine bölerken, müşteriden daha detaylı bilgi
isteyebilirler. Bu iletişim çok iyidir. Ayrıca, programcılar kendi
aralarında bu bölme işlemi olurken teknik konular hakkında
tartışabilirler. Bu da yararlıdır. Sonuçta, mesela şu şekilde kalemler
ortaya çıkabilir.

Özellik:Kullanıcı, banka hesapları arasında para transferi yapmak
istiyor.

Geliştirme kalemleri:

* Görsel arayüzü JSP/HTML ile kodla.

* Kullanıcı hesap nosuna kullanıcı kimlik nosu ile veri
tabanındaneriş.

* Aynı işlem (transaction) altında bir hesaptan al, öteki hesaba
yaz,işlem bitir.

Bütün kartları bu şekilde işlemden geçirdikten sonra müşterinin
katıldığı kısım bitebilir.  Toplantı bittikten sonra, müşterinin
katılması gerekmeyen kısma geçebilirsiniz. Bu kısımda, kartları kimin
kodlayacağına karar verilir. Her karta iki kişili takımlar talip olur
ve kartlar sahiplerine verilir. Bu safhadan sonra, ikili takımlar
kartlarının geliştirme kalemlerini ne kadar zamanda
kodlayabileceklerine karar verirler. Mesela yukarıdaki örnek:

Özellik:Kullanıcı, banka hesapları arasında para transferi yapmak
istiyor.

Programcılar:Ahmet, Veli

Geliştirme kalemleri:- Görsel arayüzü JSP/HTML ile kodla. (Tahmin: 1
gün).- Kullanıcı hesap nosuna kullanıcı kimlik nosu ile veri
tabanındaneriş. (Tahmin: 0.5 gün)- Aynı işlem (transaction) altında
bir hesaptan al, öteki hesaba yaz,işlem bitir. (Tahmin: 0.5 gün).

Talip olma konusunda bir noktayı vurgulamak gerekir. Takım lideri
olarak kimseye bir özelliği 'vermeyin'. İkili takımların/kişilerin bir
karta 'talip' olmasını bekleyin. Bu nokta çok önemlidir. Eğer birine
bir kartı veriyorsanız, emir/komuta zinciri işletmiş oluyorsunuz. Eğer
programcı kendisi talip oluyor ise, kendi insiyatifi ile bu işe
kalkışmış oluyor. Bu aradaki fark, ufak bir ruhbilimsel fark olarak
gözükse bile, etkisi büyüktür. 'Bu özelliği kim kodlamak istiyor' diye
sorduktan sonra bir 'sessizlik' durumu olabilir. Bekleyin. Mutlaka
talip olan ikili çıkacaktır.  Toplantıdan sonra, geliştirme
kalemlerini birbirine toplayın.

Sonuç olarak, bütün özelliklerin bir döneme sığmayacağını
bulabilirsiniz. O zaman döneme sığmayacak kartları, bir sonraki döneme
bırakmanız gerekir. Hangi kartın sonraki döneme bırakılacağını
müşteriye sorun. Özellik sırası, iş dünyasına ait bir karardır, teknik
bir karar değildir. XP bu konuda çok hassastır, unutmayalım. Müşteri
teknik karar alamaz, programcılar da iş hayatını ilgilendiren kararlar
alamazlar.

En son kart listesi elinizde olduktan sonra, takımınıza hangi
özelliklerin kodlanacağını artık bildirebilirsiniz.  Bir Dönem Ne
Kadar Büyüktür?  Sığmak, sığdırmak terimleri bağlamında bir noktayı
vurgulamak gerekiyor. Bir dönemin ne kadar büyük olduğu nasıl bulunur?
Yani kabımızın büyüklüğü nedir? İlk birkaç dönem için, takım lideri
tahmin bir büyüklük ile başlayabilir.. Bir dönem (iteration) büyüklüğü
olarak 2 hafta farzederek başlayalım. Takımımızda 5 programcımız var
ise de, bu programcıların haftanın 5 çalışma günü içinde 3 ideal
günlük iş yaptığını farzedelim; o zaman kap büyüklüğü 3 ideal gün x 5
programcı x 2 hafta = 30 ideal gün olacaktır. Yani kabımızın büyüklüğü
30 gündür.  Bu yapılan "tahminler", XP'nin gevşek bir yöntem olduğu
intibası vermemelidir. Esasında, bir kere kap büyüklüğü hesaplandıktan
sonra, hangi özellik sığdırma işlemi çok katı bir şekilde takip
edilecektir.

Tahminlerin nasıl yapıldığına gelelim: İlk faraziye, 2 haftalık bir
dönem koyulması, ikincisi ise programcının bir hafta içinde kaç ideal
gün çalışabildiğidir. 2 haftalık dönemi seçerken, diğer XP
projelerinin tecrübelerinden yararlanabilirsiniz. Projenizin ritmini
ne kadar kısa aralıklarla böleceğinize karar verin. 1 hafta çok kısa
olabilir, 4 hafta da çok uzun. Seçim size kalmış, ama dönem uzunluğunu
birinden ötekine sürekli değiştirmeyin.  İkinci tahmin hakkında:
Projede ilerledikçe ve her özelliğin 'gerçek zamanı'ölçüldükçe,
dönemlerin içine sığacak ideal gün sayısını bulabilirsiniz. Böylece
bir sonraki döneme özellik koyarken, önceki dönemin büyüklüğüne göre
listeye almaya başlarız, vs.. Ve böylece projemiz ilerledikçe,
takımınız tahmin yeteneği de ilerleyecek, bu tahmin/gerçek sayıları
birbirine iyice yaklaşacaktır.





