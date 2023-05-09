# XP ve Danışmanlık

Bu yazıda Extreme Programcılığın danışmanlık, teknoloji seçimi, takım
idaresi gibi açılardan ele alacağız.

Gördüğümüz kadarı ile işletimde (execution) çok yerinde tespitler
yapan ve kurallar koyan XP'nin, her seviyede doğru anlaşılması için
böyle bir anlatımın yapılması iyi olacak.  Satırların yazarının dahil
olduğu danışman şirketleri, proje bazında çalışan, sabit zamanlı/sabit
bitiş kodlama süreleri koyan ve sabit ücret alan şirketler idi. Bu tür
şirketler, hem kendi açılarından tahmin edilebilirlik (ne kadar
kazanırız?), personel idaresi (ne kadar kodcu lazım) gibi sorulara
kesin cevaplar bulabildikleri için, herşeyi sabit olarak yapmayı
seçmişlerdi.  Fakat yazılım projelerinin ne kadar tahmin edilmez
olduğu ortaya çıkmaya başlayınca, beton dökerek çizilmiş Microsoft
Proje Gantt grafikleri durumu saptırmaya başladı. Bu duruma çözüm
olarak şirketler değişik teknikler getirmeye uğraştılar.  Bazıları,
programcılara baskı yaparak stres ortamı altında verim almaya
çalıştılar. Ya da, birkaç teknoloji üzerinde yoğunlaşarak tahmin
edilebilirliği geri kazanmaya çalıştılar.

Bu çözümlerden ikisi de başarasızlığa uğradı. Hatta ilk çözüm kısa
vadede bile çökmüştür. Ekonominin iyi olduğu ortamda, programcılar
çabuk yer değiştirebilen insanlar oldukları için, stres yerine daha
iyi idarecilerin olduğu şirketlere geçiş yapmayı tercih ettiler, ya
da, stres altında yaratıcılık yeşermediğinden, kod kalitesinde düşüş
yaşandı. Hâtta baskı ile programcıları geç saatlere kadar çalıştırmaya
çabalayan danışman şirketler, gene aynı şekilde kod kalitesi açısından
kayıp yaşamışlardır.  Geç çalıştırma danışman şirketlerde 80 ve 90
yıllarında öyle yaygındı ki, genelde danışman şirketlerinde akşam 7'da
işte çikmak, "erken" çıkmak olarak adlediliyordu. XP, ismine
"programcılık" kelimesini özellikle katmıştır, çünkü amaçlarından biri
programcının bu şekilde sakız gibi çiğnenip atılmasını engellemektir.

Kural: Programcılar yanlız ve yanlız normal iş saatleri içinde program
yazarlar.

İkinci potansiyel çözüm, şu şekilde duvara tosladı. Moore kanuna göre
mikroişlemci hızları her 19 ayda ikiye katlandığına göre, mesela artık
sürekli derlenmek yerine "yorumlanan" bir dil olarak Java'nın bilişim
sahnesine birden çıkıvermesi mümkün olabiliyordu. Bunun gibi ele geçen
donanım hızını aç bir şekilde hemen kullanabilen, tabii ki
programcıların üretkenliğini arttıran ama arkası kesilmeyen yeni
teknolojiler, yararlı olsalarda, yanlarında şu etkiyi getirmişlerdir:

Artık programcıların kendini sürekli yenilenmesi, yeni şeyler
öğrenmesi gerekiyordu.  Bu yüzden ikinci çözümdeki belirtilen
değişmeyen, tek bir teknolojide uzman şirket/takım tipi de tarihe
karışacaktı. XP, akıllıca bir yöntem ile tahmin edilmezlik sorununu
çözmemiş, sonunu açık bırakarak zamana yaymıştır. XP ile tahmin
edilebilirliği önümüzde gelmekte olan tek bir dönem ile sınırlıyoruz.
Yeni Teknolojiler Sonuç olarak, XP ile yeni teknolojiler ile oynayan,
değişimi seven ve bakleyen bir yapıya kavuşacaksınız. Yeni teknoloji
ile oynamanın kuralları şunlar.

Teknolojiyi deneyecebileceğiniz bir "smaç zamanı" ayırın. (XP
İngilizce belgeleri bu döneme "spike" adını verir). Dönem içinde smaç
yapmak için bile bir özellik (kart) yazın, ve tahmin zamanını üstüne
yazarak döneme katın. Böylelikle müşteri önem sırasına göre bazı
smaçları bazı özellikler için "feda" edebilir. Her dönem içinde ne
kadar smaç, ne kadar gerçek özellik kodlanacağı görülecektir.
Stratejik Boyut Danışman şirketler!

XP'nin yazılımı gerçekleştiren dönemlerde ne kadar yararlı olduğunu
biliyoruz. Fakat bu, stratejik türden danışmanlık yapmadan birkaç
paket programı sunarak hemen koda atlayalım demek değildir.  Stratejik
danışmanlık yapmak için mutlaka zaman ayırın. Mesela birkaç aylık,
proje başlamadan önceki bir dönem içinde, yeni bilgi işlem sisteminin
stratejik vizyonu ortaya konmalıdır. Ana teknolojiler
kararlaştırılmalıdır. Unutmayın, en kötü plan bile, plansızlıktan
iyidir.  Bu seviyede istediğiniz kadar belge/çizelge
yaratabilirsiniz. Zaten müşteriniz de bu tür şeyleri görmek
isteyecektir.

Kodlama başladıktan sonra tabii ki durum değişecek, "programı
kodlarken aynı zamanda belgeleyin" diyen olursa, XP'nin kod
belgelenmesine yasak koyduğunu belirtin. XP, belge değil, kaliteli kod
üreten bir sistemdir. Kodu anlamak isteyen, birim ve kabul testlerine
bakabilir.  Stratejik danışmanlık zamanına dönecek olursak, bu
seviyede üst kademe teknik liderlerinizi toplantılara katmanız uygun
olacabilir. Müşteriniz, teknoloji neleri yapabilir, neleri yapamaz
açısından beklentilerini dengelemek açısından bu davranış yararlı
olacaktır.  Not: "Beklentilerini dengelemek" kelimesi, bir sabit bitiş
çizgili refleks ile çıkmış olsa da, yazıda tutmaya karar verildi,
çünkü eski/yeni düşünce farkları göstermesi açısından yararlı
olabilir. Klasik projelerde, müşteri sabit parayı ödedikten sonra,
sürekli bir danışman/müşteri sürtüşmesi yaşanır. Aynı para ile daha
fazla şey isteyen müşteri, bir yandan teknoloji hakkında hayaller de
kurmaya başlayınca, danışmanlar toplu bir "hayır" korosuna
dönüşebilirler. Yukarıdaki "dengeleme" sözcüğü de bu zamandan miras
kalmıştır, fakat yazıda tutulacaktır.  Yani, stratejik dönemde ve
sonrasında hem dengeleyin, hem de müşterinizin "ufuklarını" açın.

