# IETT Duraklari ve Hatlari


IETT Duraklari ve Hatlari




Istanbul IETT durak ve hat verisini yazinin altindaki kaynaktan indirebilirsiniz. Veri iki cesit, duz hat ve durak verisi, bir de Python pickle halinde islenmis veri yapilarinin oldugu .pkl dosyalari. Duz veriyi kullanmaya gerek yok, pickle dosyalari her seyi iceriyor. Veri yapilari, fonksiyonlari "Path-Planning Algorithms for Public Transportation Systems" adli makalede tarif edilen veri yapilaridir.

CS yapisi ortak duraklar (common stops) veri yapisidir; iki hattin ortak, cakistigi duraklarin ismini listeler. K fonksiyonu (verisi) x hatti uzerindeki y duraginin hattin basindan baslayarak sayi olarak "kacinci" oldugunu rapor eder. SR cagrisi bir duraktan gecen hatlar, routestops bir hattin ziyaret ettigi duraklardir.

Python cagrisi olarak bazi ornekler:

59N ve 59R hattinin ortak duraklari

print CS(cs, '59N', '59R')

59N hattindaki L0168A duragi kacinci duraktir (cevap 4).

print K(k, '59N', 'L0168A')

Durak detaylari

print duraklar['L0139J']

Bir hattin gectigi duraklar

print routestops['59N']

Bir duraktan gecen hatlar

print sr['L0168A']

Ustteki cagrilarin hepsi test.py adli dosya icinde.

Bu veriler, veri yapilari kullanilarak iki nokta arasindaki en kisa seyahati bulan kodlar yazilabilir. Hangi hatlara  binilecegi, inilecegi gibi.. isin bu kismi ilgilenen okuyuculara odev olsun. Ustteki makalede konu hakkinda guzel detaylar var. Matematiksel olarak kombinatoryel matematik, grafik teorisi (graph theory) gibi konular isin icine giriyor. Tek transfer, iki transfer, vs. hesaplari ayri ayri yapiliyor.

Not: Durak kodlari ayni sayi ama sonunda 'A' ve 'B' gibi eklerle  yolun karsilikli taraflarinda, ayni hat uzerinde ama ters yonlere gidecek sekilde konumlandirilmis olabiliyor. Bunu niye soyluyoruz? Eger bir uygulama durak ismini aratarak (mesela "levent" gibi)  durak kodu almayi, ve oradan seyahat planlama yapmayi dusunuyorsa birden fazla durak koduyla is yapmaya hazir olmali.

Kodlar





