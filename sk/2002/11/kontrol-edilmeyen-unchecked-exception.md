# Kontrol Edilmeyen (Unchecked) Exception'lar

Java'nın sözdizim bağlamında çoğu programcıyı gereksiz yere uğraştıran
ve vakit kaybına neden olan özelliklerinden biri, exception atan bir
kod parçasını kullandığınızda kullanan tarafın ya try/catch koymaya,
ya da bulunduğu metotun tanımına "throws xxException" gibi bir ifade
eklemeye mecbur kalmasıdır. Tabii hepimizin bildiği gibi "throws
xxException" ekleme yoluna giderseniz, bu eklemenin etkisi zincirleme
olarak o metotu çağıran metota, ve oradan başka yerlere sıçrar, ve bir
süre sonra bir bakmışız ki metotlarımızın yarısı aynı asalak ifadeyi
içeriyor: method_x() throws Exception!  "En iyi kod, yazmadığım
koddur" desturundan yola çıkarak, bu yazının geri kalanında kontrollü
Exception kullanımını tavsiye etmeyeceğiz.

Kurumsal Java programcılığı esnasında zaten fazla olan teknoloji
çorbası ile uğraşırken "sadece istenilen şeyi söylemek, geri ile
gerekince ilgilenmek" en iyisidir. Daha kısa zamanda, daha
basit/bakımlı kod yazmak bunu gerektirir.  Şimdi, final tavsiyeyi
vermeden önce Java exception mimarisine bir bakalım.  Java ve
Exception'lar Java sözdizim yapısında iki tür exception
vardır. Kontrollü (checked) exception, ve kontrolsüz (unchecked)
exception.  Çoğumuzun bildiği gibi kontrollü exception durumunda, eğer
bir kod parçası içinde 'throw new xxxException' gibi bir ibare
mevcutsa, kullanan kod try/catch "yakalayıcıları" bulundurmaya
mecburdur.

Bu gereklilik bizzat Java derleyicisi üstümüzde zorlanan (enforce) bir
gerekliliktir, kaçış yoktur.  Kontrolsüz exception kullanımında ise
herhangi bir servis kod parçası kontrolsüz exception fırlatabilir,
fakat çağırım yapan taraf bu exception'ı yakalamaya (ya da bulunduğu
metot tanımına "throws xxException" eklemekte) mecburiyeti
yoktur. RuntimeException bunun güzel bir örneğidir.  Java
tasarımcıları baştan beri kontrolsüz yanında kontrollü exception
kullanımını da bir seçenek olarak sunmayı seçmişlerdi. Ayrıca, zamanın
ortak aklı da (conventional wisdom) bir şekilde kontrollü exception
kullanımını ön plana çıkarmıştır (çünkü ilk Java kod örneklerinde
böyleydi).

Fakat aslında, Java tasarımcılarının bu seçiminin arkasında müthiş
zeka aramamak gerekir. İstekleri sadece şuydu: Programcının, çağırdiğı
kodun ne yaptığından "haberdar olması", ve o kodu kullanırken "bunun
sonuçlarına katlanıyorum :)" ibaresini yazılı olarak kodunun içine
koyması.  Fakat bu saf istek, kurumsal Java bağlamında artık bir zul
olmaya başlamıştır. Kurumsal Java kodları bir API "denizi" içinde
yüzer, ve 3-4 dış kütüphane kullanan bir kodda artık çok fazla sayıda
try/catch ibaresi görmek mümkündür, ya da metot tanımı bağlamında
gereksiz bir ekleme enflasyonu ortaya çıkmaktadır.  Bu sebeple
yazımızın tavsiyesi kurumsal uygulamalarda (web/telekom/vs) kontrolsüz
Exception kullanılmasıdır. Bu Exception yöntemi ile, bir Exception'ı,
"sadece istediğinizde" try/catch ile yakalamanız yetiyor, eğer hiçbir
Exception ile ilgilenmiyorsanız, derleyici size bir
hatırlatmada/zorlamada bulunmuyor. Bu sayede kodunuz daha temiz
kalıyor.

Tabii eklemek gerekir ki, içinde Exception atılması muhtemel bir
çağrıyı yapan kodun, o kontrolsüz Exception'ı yakalamaması demek, bu
Exception'ın daha üst seviyelerde yakalanmayacağı demek
değildir. Exception kurallarına göre yakalanmayan bir Exception zaten
çağrı sıralamasında üste doğru çıkacaktır, ve kontrolsüz şartında
sadece ilgilenen bir yakalayıcı tarafından yakalanabilir. Zaten, her
uygulama bir "en üst seviye" yakalayıcı genelde olduğu için bu, o
Exception'ın eninde sonunda bir yerde yakalanacağı anlamına gelir. Ama
bu yakalayıcı bir tane, ve her yerde olmayan bir ekleme olduğu için
kod temizliği için bir tehlike arz etmez.  Bu tavsiye ışığında
Exception muamele sistemimiz daha temiz ve bakımı rahat hâle
gelekcektir.  İlginçtir ki, kontrollü Exception seçeneği, diğer "pür"
nesnesel dillerde mevcut değildir: Ada, Eiffel, Ruby, Phyton
dillerinin hepsi kontrolsüz Exception atarlar. Bu dillerden Ada,
gerçek zamanlı (real-time) ve kritik misyon (mission critical)
sistemlerde kullanıldığına göre (askeri, uzay sistemlerinde),
kontrolllü Exception'ların pek o kadar da hayati olmadığı ortaya
çıkmaktadır.





