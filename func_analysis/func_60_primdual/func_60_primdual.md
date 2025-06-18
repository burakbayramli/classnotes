# Ana-Çift İç Nokta Metotları (Primal-Dual Interior-Point Methods)

Şimdiye kadar gördüğümüz problem tiplerini hatırlayalım şimdi ve çözme
zorluğu açısından sıralayalım. En üstte, en basit olan karesel
problemlerdi, 

$$
\min_x \frac{1}{2}  x^T Q x + c^T x
$$

formunda oluyorlardı. Bu problemler en basiti, eğer $Q$ pozitif yarı-kesin
ise. Basit çünkü gradyanı alıp sıfıra eşitliyorum, pat diye sonucu
buluyorum. 

Sonraki seviye, biraz daha zor, üstteki probleme $Ax=b$ formunda eşitlik
kısıtlaması getirmek. Bu problemi de kapalı / analitik halde (closed-form)
çözebiliriz, KKT koşullarını kullanarak. $Ax=b$ ifadesini ek değişkenler
üzerinden kritere ekleriz, Lagrangian'ı oluştururuz, KKT koşulunda iki tane
öğe olur, durağanlık, ve ana olurluk, bu iki öğeyi eşzamanlı olarak çözerek
sonuca ulaşırız, koca bir lineer sistemdir bu. 

Bir sonraki pürüzsüz minimizasyon, yani üstteki kriterin yerine $f(x)$
kullanmak ki $f(x)$ bir pürüzsüz fonksiyon. Bu durumda Newton metotu
kullanıyoruz, bu metot $Ax=b$ kısıtlamasında $f(x)$ minimizasyonunu birkaç
adımda çözmeye uğraşıyor, bunu her adımda $f(x)$'e bir karesel yaklaşıklama
yaparak başarıyor. 

Sonraki seviye ise iç nokta metotları, eşitlik kısıtlamalarına ek olarak
$h_i(x) \le 0, i=1,..,m$ formunda eşitsizlik kısıtlamaları eklemek.  Bu
derste bu tür problemleri ana-çift yöntemi ile çözeceğiz, daha önceki bir
derste bariyer yöntemi iç nokta metotu ile çözdük. 

Genel olarak yaptığımız herhangi bir seviyedeki problemi çözmeye
uğraştığımızda onu bir önce seviyedeki probleme indirgemek, problemi belli
adımlara bölerek her adımda nasıl işlediğini bildiğimiz önceki seviyedeki
tekniği uygulamak. Bariyer metotunda öyle oldu mesela, eşitsizlik
problemini bariyer terimini kritere ekleyerek bilinen Newton adımlarıyla
onu çözmeye uğraştık. 

Ana-çift metotu biraz daha farklı olacak. Onu öğrendiğimizde göreceksiniz
ki bir problemi açık bir şekilde başka bir probleme indirgemediğini
göreceksiniz [1, 5:55]. Bu tekniği sarsıma uğratılmış KKT koşulları
ışığında ele almak lazım, ana prensibi bu.

[bariyer metot özeti atlandı]

Ana-çift metotu, bariyer metodundan farklı olarak, $t$ parametresinin
güncellemeden önce sadece tek Newton adımı atar. Yani ana-çift metotunda da
yaklaşıksallamanın kuvvetini kontrol eden bir $t$ var, ama o belli bir $t$
üzerinden yakınsama oluncaya kadar Newton adımı atmak yerine her $t$ için
tek Newton adımı atılıyor. Bu demektir ki dış döngü, iç döngü farkı yok,
her şey tek bir döngü içinde.

Bir diğer fark ana-çift döngüsünde giderken üretilen (ziyaret edilen)
noktalar illa olurlu olmayabiliyor. Yapısı itibariyle metot döngüsü
sırasında eşitsizlik kısıtlamalarını tatmin eder, fakat her zaman eşitlik
kısıtlamalarını tatmin etmeyebilir. Hatta bazen ikiz olurlu noktalar bile
mevcut olmayabilir, bu daha ciddi bir durum. Hatırlarsak bariyer metotunda
ikiz olurlu nokta her zaman vardı ve bu noktayı bir ikiz boşluğu hesaplamak
için kullanabiliyorduk. Bu boşluğu hesaplamak kolaydı, her noktada
$m / t < \epsilon$ değerindeydi.

O zaman ana-çift ile bu hesap yoksa, ne zaman duracağımızı tam bilmiyoruz
demektir, demek ki akıllıca uydurma (heuristic) yaparak bir durma şartı
bulmamız lazım.

Pozitif bağlamda ana-çift metotları daha verimli çalışır. İspatına
girmeyeceğiz ama ana-çift yakınsaması lineerden daha iyidir.

Negatif olarak ana-çift metotlarını kabaca, sezgisel kavramak bariyer
metotu kadar direk olmayabilir. Şahsen bu alanda araştırmacı olan ben bile
ana-çift metot adımlarının temelii hatırlamakta bazen zorlanıyorum, bariyer
metotunu hatırlamak basit, kısıtlamanın log'unu alıp kritere ekliyorsunuz,
sonra Newton metotu uyguluyorsunuz [1, 13:35].

Detaylara gelelim. KKT koşul sarsımını hatırlarsak, bariyer metotunu KKT
koşullarında bir sarsım olarak görebiliyorduk, şu ifadelerde

$$
\nabla f(x) + \sum_{i=1}^{m} u_i\nabla h_i + A^T v = 0
\qquad (1)
$$

$$
u_i \cdot h_i(x) = (-1/t) 1, \quad i=1,..,m
$$

$$
h_i(x) \le 0, \quad i=1,..,m, \quad Ax = b
$$

$$
u_i \ge 0
$$

Normal sartlarda bloktaki ikinci ifade yerine 

$$
u_i \cdot h_i(x) = 0, \quad i=1,..,m
\qquad (2)
$$

olacaktı. Değişen tamamlayıcı gevşeklik yani.

Ana problem neydi?

$$
\min_x f(x) \quad \textrm{öyle ki}
$$
$$
Ax = b
$$
$$
h_i(x) \le 0, \quad i=1,..,m
$$

Bu problemin KKT şartları görülen blokta, durağanlık için gradyan alıp
sıfıra eşitlenir, (1) elde edilir, tabii $f,h_i$'in pürüzsüz ve dışbükey
olduğu farz edilir, o sebeple gradyan yeterli, altgradyana gerek yok, vs.
Tek değiştirdiğimiz tamamlayıcı gevşeklik ve onun artık sıfıra eşit
olmasını şart koşmuyorum, ufak başka bir değere, ve doğru işarete sahip
olan başka bir değere eşit olmasını zorluyorum, $u_i \cdot h_i(x) = (-1/t)$
şartı bu. $1/t$ gibi bir değerin sebebi aslında $\log(x)$'in türevinin
$1/\log(x)$ olmasıyla alakalı, çünkü log bariyerleştirilmiş kriterin
türevini alıp sıfıra eşitleyince ve ikiz değişkenleri uygun şekilde
tanımlayınca log bariyer metotunun orijinal KKT koşulları yerine üstteki
şekilde bir problemi çözülebildiğini görmüştük [1, 16:19], ve $t$
büyütüldükçe görülen değiştirilmiş tamamlayıcı gevşeklik esas versiyonuna
daha da yaklaşıyordu.

Ana-çift metotlarına erişmenin bir diğer yolu sarsımın ortaya çıkarttığı
denklemleri birarada çözmek ve Newton adımını ona göre atmak [1, 22:55]. 

Denklemler ayrı ayrı olarak 

$$
r_{dual}= \nabla f(x) + Dh(x)^T u + A^T v 
\qquad (6)
$$

$$
r_{cent} = -\mathrm{diag}(u)h(x) - 1/t 
$$

$$
r_{prim} = Ax - b
$$

Sarsım denklem sistemini sıfıra eşitlemek amacıyla matris formunda
düzenlersek, 

$$
r(x,u,v) = 
\left[\begin{array}{c}
\nabla f(x) + Dh(x)^T u + A^T v \\
-\mathrm{diag}(u)h(x) - 1/t \\
Ax - b
\end{array}\right]
\qquad (3)
$$

ki

$$
h(x) = \left[\begin{array}{c}
h_1(x) \\
\dots \\
h_m(x)
\end{array}\right]
\quad
D h(x) = \left[\begin{array}{c}
D h_1(x)^T \\
\dots \\
D h_m(x)^T
\end{array}\right]
\qquad (4)
$$

$r(x,u,v)$'yu sıfıra eşitliyoruz, yani bir anlamda 

$$
0 = r (x+\Delta x, u + \Delta u, v + \Delta v)
$$

çözülecek, bunu 1. derece Taylor açılımı ile yaklaşıklarım,

$$
\approx r(x,u,v) + D r(x,u,v) \left[\begin{array}{c}
\Delta x \\ \Delta u \\ \Delta v
\end{array}\right]
$$

Üstteki denklemde (3) ve (4) öğelerini kullanarak özyineli şekilde dönersem
gayrı-lineer denklemi çözmüş olurum. Notasyonu biraz degistirirsek,
$y = (x,u,v)$ ile,

$$
0 = r(y + \Delta y) \approx r(y) + D r(y) \Delta y
$$

ve $\Delta y$ için çözmek istiyoruz. 

Ya da, genel bir $F$ için $F(y) = 0$ çözümü, yani "kök bulmak'' amacıyla
her döngü adımında bir $\Delta y$ hesaplayabilmek istiyoruz. Şu şekilde

$$
F(y + \Delta y) \approx F(y) + D F(y) \Delta y
$$

yaklaşıklarsak, ve kök amaçlı $F(y)=0$ olmalı ama $F(y + \Delta y) = 0$ da
denebilir,

$$
0 \approx F(y) + D F(y) \Delta y
$$

$$
-F(y) =  D F(y) \Delta y
$$

$$
\Delta y = -(DF(y))^{-1} F(y) 
$$

Ya da

$$
DF(y) \Delta y = -F(y) 
$$

Bu problemde $F$ yerine $r$ var. 

$$
D r(y) \Delta y = -r(y) 
$$

O zaman (3)'teki $r(y)$'nin türevi, yani Jacobian'ı gerekiyor. Üsttekini
şöyle yazıyoruz, 

$$
\left[\begin{array}{ccc}
\nabla^2 f(x) + \sum_{i=1}^{m} u_i \nabla^2 h_i(x) & D h(x)^T & A^T \\
-\mathrm{diag}(u) D h(x) & -\mathrm{diag}(h(x)) & 0 \\
A & 0 & 0
\end{array}\right]
\left[\begin{array}{c}
\Delta x \\ \Delta y \\ \Delta v
\end{array}\right] = 
\left[\begin{array}{c}
r_{dual} \\ r_{cent} \\ r_{prim}
\end{array}\right]  
\qquad (5)
$$

Büyük Jacobian'ı nasıl elde ettik? Mesela (3)'ün ilk satırına bakalım, 

$$
\nabla f(x) + Dh(x)^T u + A^T v 
$$

var, onun $x,u,v$'ye göre türevlerini almak bize iki üstteki matrisin
1. satır 1. 2. ve 3. kolonunu veriyor, mesela $x$'e göre türev alınca bir
üstteki ifadede 1. ve 2. terimin türevi alınır, $A^T v $ yokolur, bu bize
$\nabla^2 f(x) + \sum_{i=1}^{m} u_i \nabla^2 h_i(x)$ verir [1, 28:43].
Aynı şekilde devam edersek görülen matrisi elde ederiz. Tüm sistemi
$\Delta y$ için çözünce de istediğimiz Newton yönünü elde ederiz.

Bu yönteme ana-çift denmesinin sebebi üstte görülüyor aslında, çünkü dikkat
edersek hem ana hem ikiz değişkenleri aynı sistemde, aynı anda
çözüyoruz. Değil mi? Denklem sistemi KKT koşularının formülize edilmesinden
geldi, ve bu koşullarda ana ve ikiz değişkenler aynı yerde mevcuttur, ve
çözerken tüm $x,u,v$ için çözüyoruz. 

Not: Bu yaklaşımla bariyer metotuna erişmek mümkün, o durumda sistemden $u$
çıkartılır, ve geri kalanlar çözülür. 

Metotu algoritmik olarak görmeden önce bir konudan daha bahsetmek
istiyorum; alternatif ikizlik boşluğu. Bu gerçek ikizlik boşluğu değil,
çünkü daha önce belirttiğimiz gibi bu metotta ikiz değişkenler her zaman
olurlu olmayabiliyor.

Bariyer metotu için ikizlik boşluğu basitti, $m/t$ çünkü
$u_i = -1 / (t h_i(x))$, $i=1,..,m$ tanımlamıştık ve bu ikiz olurlu idi.
Alternatif boşluk için sanki ikiz olurluk varmış gibi yapıyoruz, ve 

$$
\eta = -h(x)^T u = - \sum_{i=1}^{m} u_i h_i(x)
$$

hesabını yapıyoruz. Eğer üstteki hesabı bariyer problemi için yapıyor
olsaydık, $u_i = -1/t$ tanımlamış olacaktık ve o zaman bariyer metotu için
olan boşluğu elde edecektik. Ana-çift yönteminde böyle değil tabii, sistemi
çözerken $u_i$ için de çözüm yapıyoruz, onu önceden tanımlamıyoruz, fakat
üstteki formu kullanarak alternatif ikizlik boşluğunu elde edebiliriz.  
$\eta$ her zaman pozitif olacak, çünkü kendimizi her zaman $h_i(x) \le 0$
olacak şekilde kısıtlayacağız, ve $u_i \ge 0$ zaten, o zaman çarpımlarının
ekşi ile çarpılması pozitif sonuç verir.

Tüm bunları durma şartı için nasıl kullanırız? Her ne kadar $u_i$'lar
olurlu olmayabilse bile yine de boşluğu hesaplıyoruz, ardından ikiz
değişkenlerin olurluğa ne kadar yakın olduğunu ayrı bir yerde
hesaplıyoruz. Yani eğer alternatif boşluk az, ve olurluğa yakınlık varsa,
akıllıca bir uydurma ile kullanarak durma / durmama kararı verebiliriz.
Gerçi bu teknik uydurmadan biraz daha iyi aslında, ana-çift metotunun
yakınsadığına dair matematiksel ispatlar var, fakat terminolojik olarak bu
boşluk hesabı gerçek bir boşluk hesabı değil.

Artık metotu tanımlayabiliriz. Bir harfiyen olurlu $x^{(0)}$ ile başla,
yani bu nokta $h_i(x^{(0)}) < 0 $, ve $A x^{(0)}= b$. Ayrıca $u^{(0)} > 0$,
$v^{(0)}$ herhangi bir değer. Alternatif ikizlik boşluğu
$\eta^{(0)} = -h(x^{(0)})^T u^{(0)}$ olarak tanımla [1, 45:21].

$t$'yi büyütmek için $\mu > 1$ kullanıyoruz. Her döngü sonunda eski $t$'yi
$\mu$ ile çarpıp yeni $t$ elde edeceğiz.  

Adımlar

1) $t = \mu m / \eta^{(k-1)}$ tanımla. 

2) Ana-çift güncelleme yönü $\Delta y$'yi hesapla (nasıl yapılacağını
gördük, (5)'teki lineer sistemi çözerek).

3) Geri iz sürme (backtracking) tekniği ile adım büyüklüğü $s$'yi hesapla
(birazdan nasıl yapılacağını göreceğiz)

4) $y^{(k)} = y^{(k-1)} + s \cdot \Delta y$ ile $y$'yi güncelle. Yani bu
hesapla tüm ana, ikiz değişkenleri güncellemiş oluyoruz, $x,u,v$. 

5) Alternatif ikizlik boşluğunu hesapla $\eta^{(k)} = -h(x^{(k)})^T
u^{(k)}$. 

6) Ana ve ikiz artıklar ufak ise, yani eğer $\eta^{(k)} < \epsilon$ ise ve 
$(|| r_{prim} ||_2^2 + ||r_{dual}||_2^2)^{1/2} < \epsilon$ ise dur. 

$r_{prim}$ hatırlarsak eşitlik sınırlamasından ne kadar uzak
olduğumüz. $r_{dual}$ ise durağanlık şartıydı, onun sıfırdan ne kadar uzak
olduğuydu. Niye ona "ikiz (dual)'' etiketi verdik? Bunun ikiz olurluk ile
ne alakası var? Burada biraz nüans var.. 

Not: artık kelimesini kullandık daha önce  $r_{dual}$ ikiz artık,
$r_{prim}$ ana artık. 

Hatırlarsak $u,v$ üzerindeki kısıtlamalar nelerdi? $u \ge 0$, ve $v$
herhangi bir şey olabilir. Ama dolaylı bir kısıtlama daha var aslında, o da
$u,v$'nin Lagrange iki fonksiyonunun tanım alanında olma zorunluluğu.. Ve
bu kısıtlamalar işte (6)'dan başlayan üç denklemde aslında belirtiliyor. 
Yani, çünkü eğer alttaki sıfır ise

$$
\nabla f(x) + D h(x)^T u + A^T v = 0
$$

bu sadece ve sadece doğru olabilir $x$ eğer $x$ üzerinden $L(x,u,v)$'i
minimize ediyorsa. Ki bu durumda 

$$
g(u,v) = L(x,u,v)
$$

doğru olur. Yani $x$ Lagrangian'ı minimize ediyorsa, tanım itibariyle
$L(x,u,v)$ eksi sonsuzluk değildir. Değil mi? Çünkü eksi sonsuzluğa gidiş
olmasın diye ikizde spesifik kısıtlamalar getirdik. Ve bu da demektir ki
$u,v$ Lagrangian'ın tanım alanında olmalı.

Geriye İz Sürme

Üstteki algoritmada \#4 adımında bir adım atıldığını gördük, fakat bu adım
atılırken $s$'nin nasıl bulunacağını anlatmadık. Adım atılırken
$y^+ = y + s \Delta y$ ile, $h_i(x) \le 0$, ve $u_i(x) > 0$ şartlarının
hala geçerli olmasını garantilemek istiyoruz, ve $s$'yi bu olacak şekilde
seçeceğiz. Tabii $y^+ = y + s \Delta y$ derken

$$
x^+ = x + s \Delta y
$$

$$
u^+ = u + s \Delta y
$$

$$
v^+ = v + s \Delta y
$$

demek istiyoruz. Bu seçim şöyle yapılabilir, önce $s$'yi her öge için
$u_i > 0, i=1,..,m$ olacak sekide mümkün en büyük adımdan başlarız. Bu
çözüm kolaydır, çünkü her $u_i$ için her $\Delta u_i$ bizi sıfıra
yaklaştırıyor mu, eğer yaklaştırıyorsa sıfıra gelmeden ne kadar uzağa
gidebiliriz sorusunuz sorabiliriz, ve tüm bu uzaklıklar arasından en ufak
olanı $s$ seçimi için başlayacağımız en büyük uzaklık
olacaktır. Matematiksel olarak 

$$
s_{max} = \min \bigg\{
  1, \min \{ -u_i / \Delta u_i : \Delta u_i < 0  \}
\bigg\}
$$

Tabii harfiyen olurluk istiyoruz, yani $u > 0$ o zaman bulunan büyüklüğün
0.999'ü kadarını alırız. Bu değeri alınca oradan "geriye iz sürmeye''
başlarız, yani küçülte küçülte bu sefer $h$ şartlarını da tatmin eden bir
$s$ aramaya başlayabiliriz. Bu aramayı yaparken $u$ işaretini tatmin
edeceğimizden eminizdir çünkü en büyük $s$'yi özellikle $u$ için ayarladık.

Döngünün bu aşamasında her küçültme sonrası alttaki şartları da kontrol
edeceğiz, şu şekilde;

$s = \beta s$ yap, ta ki 

1 - $h_i(x^+) < 0, i=1,..,m$. 

2 - $|| r(x^+, u^+, v^+) ||_2 \le (1-\alpha s) || r(x,u,v) ||_2$

olana kadar. 2. şartta eşitsizliğin sol tarafı sarsıma uğratılmış KKT
koşulları, onu $1-\alpha s$ oranında azaltıyorum.

Ya da şu şekilde bakabiliriz, 1. kontrolda $s$'i ana olurluk tatmin
oluncaya kadar azalt. Ondan sonra 2. adım üzerinden normal geriye iz sürme
gerçekleştir. 

Artik elimizde ana-cift metotunu kodlamak icin gereken her sey var. Bir
ornek uzerinde gorelim, standart form LP. 

$$
\min_x c^T x, \quad \textrm{öyle ki}
$$
$$
Ax = b
$$
$$
x \ge 0
$$

ki $c \in \mathbb{R}^n$, $A \in \mathbb{R}^{m \times n}$, $b \in \mathbb{R}^m$.

Ikiz

$$
\max_{u,v} \quad \textrm{öyle ki}
$$
$$
A^T u + u = c
$$
$$
u \ge 0
$$

Bu formu ezberlemek aslında faydalı olabilir çünkü optimizasyonda başka
yerlerde bu formu görebilmek faydalı olabiliyor. 

Ana-çift metotu bu problem üzerinde işleyecek, ve bize olurluğa çok yakın
olan hem ana hem de ikiz problem için bir çözüm verecek. $Ax=b$, ya da
$A^T u + u = c$ şartını tam olarak tatmin etmiyor olabilirim ama onlara
yakın bir yerde olacağım. Tabii ana-çift metotu işleyişi sırasında doğal
olarak bu şartlara yakın durmayabilir, o sebeple artıkları kontrol
ediyoruz. 

Şimdi bu konunun tarihi hakkında biraz konuşalım. Lineer programları ilk
çözen araştırmacı Dantzig, simplex adlı bir metodu keşfetti. Hala bu yöntem
LP çözmek için en yaygın metotdur. Ne yazık ki onu LP'ler ötesine
genelleştirmek mümkün değil, o sebeple bu derste onu işlemedik. İç nokta
metotları, kıyasla, çok daha geniş bir problem sınıfında geçerlidir, Newton
metotu, gradyan inişi, hep bu bağlamda devreye girer, vs. 

Tabii simplex değişik bir mahluktur, "direk yöntem'' denen bir metot
sınıfındadır, şimdiye kadar gördüğümüz metotlarda olduğu gibi döngü içinde
daha iyi, daha iyi çözüme gitmiyor, en iyi, kesin çözümü bulmaya uğraşıyor
[tabii lineerlik burada faydalı herhalde, ayrıksal şekilde seçenek arama
açısından, ama diğer yandan lineerlik ötesine geçilemiyor].

Simplex iyi isler fakat bir sure sonra anlasildi ki en-kotu durum
cetrefilligi oldukca kotu. 

İç-nokta metotları simplex'den sonra geliştirildi, burada 70'ler, 80'lerde
müthiş bir aktivite oldu. Khachiyan ve Karmarkar burada önemli isimler,
LP'ler için ilk ispatlanabilir polinom zamanlı çözümü
geliştirdiler. Khachiyan'ın metotu elipsoid yakasımını kullanıyordu, teorik
olarak çok kuvvetliydi ama pratikte ne yazık ki böyle olmadığı görüldü,
fakat en azından alternatif bir şekilde LP çözülebileceğini
gösterdi. Karmarkar'ın buluşu en önemlisi, buluşu bugün gördüğümüz ana-çift
iç nokta yöntemine benziyordu, ispatlanabilir polinom hızdaydı, ve pratikte
oldukca verimliydi. Karmarkar'ın yaklaşımı iç-nokta alanında bir
araştırma patlamasına sebep oldu.  

Örnekle devam edelim. Bir LP'yi ana-çift yöntemi ile çözeceğiz ve
farklarına bakacağız. Standart form LP'nin KKT koşulları,

$$
A^T v + u = c
$$
$$
x_i u_i = 0, i=1,..,n
$$
$$
Ax = b
$$
$$
x,u \ge 0
$$

İç-nokta yöntemleri ilk ve son iki şarta uyacak şekilde ayarlanır ve döngü
içinde yavaş yavaş 2. şartı yerine getirmeye uğraşır [2, 13:47]. 

Sarsıma uğratılmış KKT şartları, üstteki formülde tamamlayıcı gevşeklik için
eşitlikte $1/t$ kullanarak  elde edilir,

$$
A^T v + u = c
$$
$$
x_i u_i = 1/t, \quad i=1,..,n
$$
$$
Ax = b
$$
$$
x,u \ge 0
$$

Sonra üstteki tüm eşitlik sınırlamalarını alıyorum (eşitsizlikleri döngü
sırasında tatmin etmeye uğraşacağım) ve onları bir matriste istifliyorum,

$$
0 = r_{pd}(x,u,v)
$$

$$
= \left[\begin{array}{c}
A^T v + u - c \\
\mathrm{diag}(x) u - (1/t) \\
Ax - b
\end{array}\right]
$$

Sonra matrisin türevini alıyorum, bir lineer yaklaşıklama yaratıyorum,
güncelleme yönünü buluyorum, geriye iz sürme yapıyorum, vs [2, 15:42]. 


Genel olarak ana-çift yönteminin (log bariyere nazaran) daha büyük bir
sistemi çözdüğünü söyleyebiliriz, $0 = r_{pd}(y + \Delta y) \approx
r_{pd}(y) + D r_{pd}(y) + D r_{pd}(y) \Delta y$ diyoruz, ve alttaki sistemi
çözüyoruz, 

$$
\left[\begin{array}{ccc}
0 & I & A^T \\
\mathrm{diag}(u) & \mathrm{diag}(x) & 0 \\
A & 0 & 0 
\end{array}\right]
\left[\begin{array}{c}
\Delta x \\ \Delta u \\ \Delta v  
\end{array}\right] = -r_{pd}(x,u,v)
$$

Güncelleme için her $t$ ile tek bir adım atıyorum,m adım $y^+ = y + \Delta
s$ (tabii $s > 0$ için çizgi araması yaparak) ama tek bir kez. Sonra 
$t = \mu t$ ile $t$'yi güncelliyorum [1, 18:13]. 

Ekler

LP Kodu

Altta şimdiye kadar anlatılan metotlar ile çözüm yapan ve sonucu
`linprog` çağrısı ile karşılaştıran bir kod [5] görüyoruz. Çözülen
problem [4, sf. 209]

$$
\min_x -x_1 - 5x_2 \quad \textrm{öyle ki}
$$
$$
x_1 + x_2 + x_3  = 5 
$$
$$
x_1 + 3 x_2 + x_4 = 7
$$
$$
x_1,x_2,x_3,x_4 \ge 0
$$


```python
import numpy as np
from scipy.optimize import linprog
from numpy.linalg import matrix_rank

def solve(c, A, b, epsilon=0.0001):
    if matrix_rank(A) < min(A.shape[0], A.shape[1]):
        print('A is not full rank, dropping redundant rows')
        _, pivots = sympy.Matrix(A).T.rref()
        A = A[list(pivots)]
        print('Shape of A after dropping redundant rows is {}'.format(A.shape))

    m = A.shape[0]
    n = A.shape[1]

    x = np.ones(shape=(n, ))
    l = np.ones(shape=(m, ))
    s = np.ones(shape=(n, ))

    k = 0

    while abs(np.dot(x, s)) > epsilon:        
        k += 1
        primal_obj = np.dot(c, x)
        dual_obj = np.dot(b, l)
        print('iteration #{}; primal_obj = {:.5f}, dual_obj = {:.5f}; duality_gap = {:.5f}'.format
              (k, primal_obj, dual_obj, primal_obj - dual_obj)) 
        sigma_k = 0.4
        mu_k = np.dot(x, s) / n

        A_ = np.zeros(shape=(m + n + n, n + m + n))
        A_[0:m, 0:n] = np.copy(A)
        A_[m:m + n, n:n + m] = np.copy(A.T)
        A_[m:m + n, n + m:n + m + n] = np.eye(n)
        A_[m + n:m + n + n, 0:n] = np.copy(np.diag(s))
        A_[m + n:m + n + n, n + m:n + m + n] = np.copy(np.diag(x))

        b_ = np.zeros(shape=(n + m + n, ))
        b_[0:m] = np.copy(b - np.dot(A, x))
        b_[m:m + n] = np.copy(c - np.dot(A.T, l) - s)
        tmp = np.dot(np.dot(np.diag(x), np.diag(s)), np.ones(shape=(n, )))
        b_[m + n:m + n + n] = np.copy( sigma_k * mu_k * np.ones(shape=(n, )) - tmp )

        delta = np.linalg.solve(A_, b_)
        delta_x = delta[0:n]
        delta_l = delta[n:n + m]
        delta_s = delta[n + m:n + m + n]

        alpha_max = 1.0
        for i in range(n):
            if delta_x[i] < 0:
                alpha_max = min(alpha_max, -x[i]/delta_x[i])
            if delta_s[i] < 0:
                alpha_max = min(alpha_max, -s[i]/delta_s[i])
        eta_k = 0.99
        alpha_k = min(1.0, eta_k * alpha_max)

        x = x + alpha_k * delta_x
        l = l + alpha_k * delta_l
        s = s + alpha_k * delta_s

    diff = np.dot(A, x) - b
    print('Ax - b = {}; ideally it should have been zero vector'.format(diff))
    print('norm of Ax - b is = {}; ideally it should have been zero'.format
          (np.linalg.norm(diff)))

    return x

A = np.array([[1,  1, 1, 0],
              [1,  3, 0, 1]])

b = np.array([5,7])

c = np.array([-1, -5, 0, 0 ])
            
res = solve(c,A,b)
print (res)

res = linprog(c, A_eq=A, b_eq=b, options={"disp": True})

print (res)

```

```
iteration #1; primal_obj = -6.00000, dual_obj = 12.00000; duality_gap = -18.00000
iteration #2; primal_obj = -9.21750, dual_obj = -1.11750; duality_gap = -8.10000
iteration #3; primal_obj = -11.15521, dual_obj = -9.33695; duality_gap = -1.81826
iteration #4; primal_obj = -11.60327, dual_obj = -11.70816; duality_gap = 0.10489
iteration #5; primal_obj = -11.64091, dual_obj = -11.69203; duality_gap = 0.05113
iteration #6; primal_obj = -11.65622, dual_obj = -11.67707; duality_gap = 0.02084
iteration #7; primal_obj = -11.66243, dual_obj = -11.67089; duality_gap = 0.00846
iteration #8; primal_obj = -11.66495, dual_obj = -11.66838; duality_gap = 0.00344
iteration #9; primal_obj = -11.66597, dual_obj = -11.66736; duality_gap = 0.00140
iteration #10; primal_obj = -11.66638, dual_obj = -11.66695; duality_gap = 0.00057
iteration #11; primal_obj = -11.66655, dual_obj = -11.66678; duality_gap = 0.00023
Ax - b = [0. 0.]; ideally it should have been zero vector
norm of Ax - b is = 0.0; ideally it should have been zero
[3.50107272e-05 2.33331700e+00 2.66664799e+00 1.40040490e-05]
Primal Feasibility  Dual Feasibility    Duality Gap         Step             Path Parameter      Objective          
1.0                 1.0                 1.0                 -                1.0                 -6.0                
0.1105388427842     0.1105388427842     0.1105388427842     0.8919387648961  0.1105388427842     -10.34625028215     
0.001400532337055   0.00140053233704    0.00140053233704    0.9918943193656  0.00140053233704    -11.65623916548     
7.115191880125e-08  7.11519194345e-08   7.115191920093e-08  0.9999491966235  7.115192025851e-08  -11.66666613752     
3.556266503391e-12  3.557079864332e-12  3.557332206583e-12  0.9999500067836  3.557595982315e-12  -11.66666666664     
Optimization terminated successfully.
         Current function value: -11.666667  
         Iterations: 4
     con: array([1.18571819e-11, 1.18527410e-11])
     fun: -11.66666666664022
 message: 'Optimization terminated successfully.'
     nit: 4
   slack: array([], dtype=float64)
  status: 0
 success: True
       x: array([1.15454732e-13, 2.33333333e+00, 2.66666667e+00, 3.96953400e-12])
```

QP Kodu

[3]'den odev sorusu 11.24 cozumu olarak altta ana-çift yöntemi ile bir QP
nasıl çözülür görüyoruz. QP şu formda,

$$
\min_x (1/2) x^T P x + q^T x \quad \textrm{öyle ki}
$$
$$
Ax \le b
$$

```python
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import numpy.linalg as lin
import scipy.linalg as slin

MAXITERS = 200;
TOL = 1e-6;
m=3;n = 3
RESTOL = 1e-8;
MU = 10;
ALPHA = 0.01;
BETA = 0.5;
x = np.zeros((n,1));

b = np.ones((n,1))*10.
q = np.ones((n,1))*3.

A = np.array( [[2, 3, 5],
               [3, 4, 5],
               [4, 5, 3]] )

P = np.array( [[1, 2, 4],
               [2, 4, 4],
               [1, 1, 1]] )

s = b-np.dot(A,x);
z = 1./s;

for iters in (range(MAXITERS)):
  gap = np.dot(s.T,z)
  res = np.dot(P,x) + q + np.dot(A.T,z)
  if (gap < TOL) & (lin.norm(res) < RESTOL):
      break
  tinv = gap/(m*MU)

  tmp1 = -np.vstack((np.hstack((P, A.T)),
                     np.hstack((A, np.diag(  (-s/z).T[0]  )))))
  tmp2 = np.vstack(( np.dot(P,x)+q+np.dot(A.T,z), -s+tinv*(1.0/z) )) 
  sol = lin.solve(tmp1, tmp2)
  dx = sol[0:n]
  dz = sol[n:n+m]
  ds = -np.dot(A,dx)
  r = np.vstack((np.dot(P,x)+q+np.dot(A.T,z),
                 z*s-tinv))
  step = np.min([1.0, 0.99/np.max(-dz/z)]);
  while (np.min(s+step*ds) <= 0):
    step = BETA*step
    print (step)
    
  newz = z+step*dz
  newx = x+step*dx
  news = s+step*ds

  tmp1 = np.dot(P,newx)+q+np.dot(A.T,newz)
  tmp2 = newz*news-tinv
  newr = np.vstack((tmp1,tmp2))
  while (lin.norm(newr) > (1-ALPHA*step)*lin.norm(r)):
    step = BETA*step;
    newz = z+step*dz
    newx = x+step*dx
    news = s+step*ds
    newr = np.vstack((np.dot(P,newx)+q+np.dot(A.T,newz),
                      newz*news-tinv))
    
  x = x+step*dx
  z = z +step*dz
  s = b-np.dot(A,x)

print (x)
  
```

```
[[-4.50000029]
 [ 2.25000012]
 [-0.75000002]]
```


Kaynaklar

[1] Tibshirani, *Convex Optimization, Lecture Video 16 (Part 1)*, 
[https://www.youtube.com/channel/UCIvaLZcfz3ikJ1cD-zMpIXg](https://www.youtube.com/channel/UCIvaLZcfz3ikJ1cD-zMpIXg)   

[2] Tibshirani, *Convex Optimization, Lecture Video 16 (Part 2)*, 
[https://www.youtube.com/channel/UCIvaLZcfz3ikJ1cD-zMpIXg](https://www.youtube.com/channel/UCIvaLZcfz3ikJ1cD-zMpIXg)   

[3] Boyd, *Convex Optimization I*, 
    [http://web.stanford.edu/class/ee364a/](http://web.stanford.edu/class/ee364a/)

[4] Wright, *Linear Programming with MATLAB*

[5] Kamal, {Linear Program Solvers},
    [https://github.com/hasan-kamal/Linear-Program-Solvers](https://github.com/hasan-kamal/Linear-Program-Solvers)







