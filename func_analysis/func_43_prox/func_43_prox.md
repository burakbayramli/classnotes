# Proksimal / Yakınsal Gradyan Metotu (Proximal Gradient Method) 

Bu metot, herhangi bir pürüzsüz olmayan fonksiyonu optimize etmeye uğraşmak
yerine belli bir yapıya uyan çetrefil fonksiyonları optimize etmeye
uğraşır. Bu yapı

$$
f(x) = g(x) + h(x) 
\qquad (1)
$$

formundadır [1, 45:59]. $g,h$'in ikisi de dışbükey. $g$'nin pürüzsüz,
türevi alınabilir olduğu farz edilir (çoğunlukla oldukca çetrefil olabilir)
ve $\mathrm{dom}(g) = \mathbb{R}^n$, $h$ ise pürüzsüz olmayabilir. Tabii pürüzsüz
artı pürüzsüz olmayan iki fonksısyon toplamı kriterin tamamını pürüzsüz
olmayan hale çevirir. Ama bu toplam daha basittir denebilir, onun üzerinde
proksimal operatörü uygulanabilir.

Hatırlarsak eğer $f$ türevi alınabilir olsaydı gradyan iniş güncellemesi 

$$
x^{+} = x - t \cdot \nabla f(x)
$$

Bu formüle erişmenin bir yöntemi karesel yaklaşıklama üzerinden idi,
$f$'nin $x$ etrafındaki yaklaşıklamasında $\nabla^2f(x)$ yerine
$\frac{1}{t} I$ koyunca,

$$
x^{+} = \arg\min_x f(x) + 
\underbrace{
  \nabla f(x)^T (z-x) + \frac{1}{2t} ||z-x||_2^2 
}_{f_t(z)}
$$

elde ediliyordu. Yani gradyan inişi sanki ardı ardına geldiği her noktada
bir karesel yaklaşıklama yapıyor, ve onu adım atarak minimize etmeye
uğraşıyor.

Ama (1)'deki $f$ pürüzsüz değil, o sebeple üstteki mantık ise
yaramayacak. Fakat, belki de karesel yaklaşıklamanın bir kısmını hala
kullanabiliriz, ve minimizasyonu sadece $g$'ye uygularız çünkü pürüzsüz
olan kısım o. Yani niye $g$'nin yerine karesel yaklaşıklama koymayalım?
Şimdi bunu yapacağız [1, 50:10]. 

$$
x^{+} = \arg\min_z \tilde{g}_t(z) + h(z)
$$

ki $\tilde{g}_t(z)$ $g$'nin $x$ noktası etrafındaki karesel yaklaşıklaması
oluyor.  Eğer elimizde $h$ olmasaydı üstteki sadece bir gradyan
güncellemesine indirgenebilirdi. Neyse açılımı yaparsak

$$
= \arg\min_z g(x) + \nabla g(x)^T (z-x) + \frac{1}{2t} ||z-x||_2^2 + h(z)
$$

Daha uygun bir formda yazabiliriz, $z$'nin $g$ üzerindeki değişikliğe
karesel uzaklığı olarak,

$$
= \arg\min_z \frac{1}{2t} 
\big|\big| z - \big( x-t\nabla g(x) \big) \big|\big|_2^2 + 
h(z)
$$

Yani söylenmek istenen, hem sadece $g$ olsaydı atacağımız gradyan adımına
yakın durmaya çalışmak, hem de $h$'nin kendisini ufak tutmak. Dışarıdan
ayarlanan $t$ parametresi $g$ gradyan adımı ile $h$ arasında bir denge
kurmak gibi görülebilir, eğer $t$ ufak ise o zaman gradyan adımına yakın
durmaya daha fazla önem atfetmiş olacağız, $h$'nin ufak tutulmasına daha
az. $t$ çok büyük ise tam tersi olacak. 

Bu aslında kabaca Proksimal Gradyan İnişi yöntemini tarif etmiş
oluyor. Şimdi proksimal eşlemesi operatörünü göstereceğiz, ki bu
algoritmaları daha temiz olarak yazmamıza yardımcı olacak. 

Bir $h$ fonksiyonu için bir $\mathrm{prox}$ operatörü tanımlıyoruz, 

$$
\mathrm{prox}_{t} (x) = \arg\min_z \frac{1}{2t} ||x-z||_2^2 + h(z)
\qquad (3)
$$

Üstteki $x$'in bir fonksiyonu olarak $\arg\min$'de gösterilen kriteri
minimize eden $z$'yi buluyor. Operatör $t$'ye bağlı, dışarıdan verilen
parametre. Tabii ki $h$'ye de bağlı, ki bazen üstteki operatörü
$\mathrm{prox}_{h,t}$ olarak gösteren de oluyor.

Üstteki ifadenin eşleme olduğunu kontrol edelim. Ciddi tanımlı bir
fonksiyon üstteki değil mi? Ona bir $x$ veriyorsunuz, o da size tek bir
sonuç döndürüyor. Ayrıca bu eşleme / fonksiyonun kendisi bir
minimizasyon. Peki bu minimizasyon problemi dışbükey mi? Evet. Peki bu
problemin özgün bir sonucu var mıdır? Vardır çünkü üstteki problem harfiyen
dışbükey. Değil mi? Eğer $h$ harfiyen dışbükey olmasa bile ifadenin tümü
harfiyen dışbükey olurdu çünkü $\frac{1}{2t} ||x-z||_2^2$ harfiyen
dışbükey [1, 55:09], $z-x$'in karesi var [ayrıca $x$ değişkeni $h$'ye
geçilmiyor].

Yani harfiyen dışbükey, o zaman özgün sonuç var. Biz de bu özgün sonucu
alarak bir iniş algoritması yazıyoruz [1, 56:28], 

$$
x^{(k)} = \mathrm{prox}_{t_k} \big( 
x^{(k-1)} - t_k \nabla g(x^{(k-1)})
\big), \quad k=1,2,...
\qquad (2)
$$

Güncelleme adımını tanıdık şekilde yazmak için 

$$
x^{(k)} = x^{(k-1)} - t_k \cdot G_{t_k} (x^{(k-1)})
$$

ki $G_t$'ye $f$'nin genelleştirilmiş gradyanı denebilir,

$$
G_t(x) = \frac{x - \mathrm{prox}_{t}(x - t \nabla g(x))}{t}
$$

$G$'nin dışbükey fonksiyonların alışılageldik gradyanlarına benzer pek çok
özelliği vardır, ki bu özellikler proksimal metotların yakınsadığıyla
alakalı ispatlarda kullanılabilir.

Şimdiye kadar anlattıklarımıza bakanlara bu komik bir hikaye gibi
gelebilir. Bir $g + h$ toplamını minimize etmek istiyordum, bunu yapabilmek
için (2) formunda adımlar atacağım, bir prox operatörüm var, ama bu
operatör bir sonuç döndürüyor aslında, ve bu sonuç bir başka
minimizasyondan geliyor. Yani $g + h$ türü bir toplam minimizasyonu yerine
her adımda, bir sürü minimizasyonları koymuş oldum. Bu nasıl daha iyi bir
sonuç verecek ki?

Şunu belirtmek lazım, sadece eğer proksimal gradyanları analitik, ya da
hızlı bir şekilde hesaplayabiliyorsak onları çözüm için düşünürüz. Yani her
ne kadar her adımda (3) turu optimizasiyonlar yapıyorsak ta, bunu pürüzsüz
olan kısım $h$ yeterince basit olduğu zaman yapıyoruz ki tüm (3) için
analitik  ya da hızlı hesapsal çözüm olsun [1, 58:54]. 

Diğer noktalar, dikkat edersek proksimal operatör $g$'ye bağlı değil,
tamamen $h$ bazlı. Eğer $h$ basit ise ama $g$ müthiş çetrefil ise bu
proksimal hesaplarını çok zorlaştırmıyor. Eğer o çok çetrefil (ve pürüzsüz)
$g$ için gradyan hesaplanabiliyorsa, durumu kurtardık demektir. 

Tekrar bir Lasso problemi göreceğiz. Bu Lasso için gördüğümüz ikinci
algoritma, ve belirtmek gerekir ki Proksimal metot altgradyan metotuna göre
çok daha verimlidir, hızlıdır.

Verili bir $y \in \mathbb{R}^n$, $X \in \mathbb{R}^{n \times p}$ için Lasso
kriterini hatırlarsak, 

$$
f(\beta) = \frac{1}{2} || y - X \beta ||_2^2 + \lambda ||\beta||_1 
$$

Kriterdeki ilk terim en az kareler kayıp fonksiyonu, ikinci terim bir ayar
parametresi üzerinden katsayıların 1. normu. Bu kriteri pürüzsüz, ve
pürüzsüz olmayan ama basitçe olan iki kısma ayıracağız, yani zaten oldukca
bariz, pürüzsüz kısım 1. terim, $g(\beta)$ diyelim, olmayan 2. terim,
$h(\beta)$ diyelim. Proksimal gradyan inişi için bize iki şey gerekiyor,
birincisi $g$'nin gradyanı, ikincisi $h$ için prox operatörünü
hesaplayabilmek.

$g$'nin gradyanı oldukca basit, onu bu noktada uykumuzda bile bulabiliyor
olmamız lazım. $h$'nin prox operatörü,

$$
\mathrm{prox}_t(\beta) = \arg\min_z \frac{1}{2t} ||\beta-z||_2^2 + \lambda ||z||_1
$$

Her şeyi $t$ ile çarparsam,

$$
\mathrm{prox}_t(\beta) = \arg\min_z \frac{1}{2} ||\beta-z||_2^2 + \lambda t ||z||_1
$$

Üstteki minimizasyonun çözümünü daha önce altgradyanlar üzerinden
görmüştük, 

$$
= S_{\lambda t}(\beta)
$$

Yine yumuşak eşikleme (soft-threshold) operatörüne gelmiş olduk, 

$$
[ S_{\lambda} (\beta) ]_i = 
\left\{ \begin{array}{ll}
\beta_i - \lambda & \textrm{eğer } \beta_i > \lambda_i \\
0 & \textrm{eğer } -\lambda \ge \beta_i \ge \lambda, \quad i=1,..,n \\
\beta_i + \lambda & \textrm{eğer } \beta_i < -\lambda_i 
\end{array} \right.
$$

Tüm algoritma neye benziyor? Önce $g$'ye göre bir graydan güncellemesi
yaparım, gradyan

$$
\nabla g(\beta) = -X^T (y-X\beta)
$$

O zaman güncelleme 

$$\beta + t X^T (y-X\beta)$$ 

olur. Buna prox uygularsak,

$$
\beta^+ = S_{\lambda t}(\beta + t X^T (y-X\beta))
$$

Yumuşak eşikleme ne yapar? Her ögeye teker teker bakar, eğer mutlak değeri
çok ufaksa onu ya sıfıra eşitler, ya da onu $\lambda \cdot t$ kadar sıfıra
yaklaştırır. 

Üstteki Lasso algoritmasina ISTA adı da verilir. 

Geriye Çizgisel İz Sürme  (Backtracking line search)

Graydan inişinde görmüştük ki adım büyüklüklerini dinamik olarak
seçebiliyorduk, her adımdaki duruma adapte olabiliyorduk, tipik olarak
Lipschitz sabitini bilmiyoruz çünkü. O sebeple geriye iz sürme pratikte
iyi işlemesiyle beraber, teorik olarak yakınsamayı da garantiliyordu. 

Proksimal gradyanları için benzer bir kavram geçerli. Ayrıca proksimal
durumda geriye doğru iz sürmenin birden fazla yolu var [1, 1:10:23]. Ben
sadece $g$ üzerinde işlem yapan bir yöntem seçtim, çünkü bu metotu
hatırlaması daha kolay. Gradyan inişi için iz sürmeyi hatırlarsak, $x$
noktasındayız diyelim ve $x-t \nabla g$ yönünde gitmek istiyoruz, o zaman
alttakinin doğru olup olmadığını kontrol ediyorduk, 

$$
f(x - t\nabla f(x) ) > f(x) - \frac{t}{2} ||\nabla f||_2^2 
$$

Proksimal gradyan için benzer bir yöntem [1:11:57], ve $G$ üzerinde işlem
yapıyoruz dikkat, $h$ değil, yani $G$ üzerinden yeterince "iniş'' yapmaya
uğraşacağız, $g+h$ değil. Normal iz sürmede üsttekinin doğruluğunu kontrol
ediyoruz, doğru ise $t$'yi belli bir ölçüde ufaltıyoruz. Doğru değilse yani
yeterince iniş yaptıysak, o zaman eldeki değerlerle güncellemeyi yapıyoruz.

$$
g(x - t G_t (x) ) > g(x) - t \nabla g(x)^T G_x(x) + \frac{t}{2} ||G_x(x)||_2^2
$$

Yani gradyan güncellemesi 1. derece Taylor güncellemesinden geldi. Üstteki
ifadede de eğime ek olarak 1. derece Taylor güncellemesine göre de
yeterince iniş yapmış olmak istiyorum. Dikkat edersek eğer $G$ yerine
$g$'nin gradyanını koyarsam, iki üstteki formüle benzer bir formül elde
ederim.

Geriye iz sürme genel algoritmasi şöyle, bir $0 < \beta < 1$ paremetresi
var (dışarıdan ayarlanan bir parametre). $t=1$ ile başlıyoruz, ve üstteki
formülü işletiyoruz, eğer gerekiyorsa $t = \beta t$ ile küçültme
yapıyoruz. İz sürme bitince bulduğumuz $t$ ile güncelleme yapıyoruz [3,
07:35].

[bazi ek detaylar atlandi]

Matris Tamamlamasi

Şimdi prox operatörü sofistike olan bir örnek görelim [3, 11:38]. Buradan
çıkan algoritma ilginç olacak. 

Bize bir $Y \in \mathbb{R}^{m \times n}$ matrisi veriliyor ama biz sadece
bu matrisin bazı öğelerini görebiliyoruz, bu öğeler
$Y_{i,j}, (i,j) \in \omega$ ile belirtiliyor ki $\omega$ belli bir indis
kümesidir. Bu problem bir tavsiye sistemi olabilir, matrisin tamamı bir
ideal müşteri / ürün eşlemesidir, biz sadece bu matrisin belli bir kısmını
görüyoruz (tipik olarak mevcut müşterilerin tarihi veride yaptığı alımlar,
ürünler üzerindeki beğendi/beğenmedi yorumları matrisin "görünen'' kısmını
temsil edebilir). 

Bu tür problemleri iz norm regülarizasyonu (trace norm regularization)
problemi olarak görmenin iyi işlediği görülmüştür. Bu problem aslında daha
önce gördüğümüz Lasso problemine benzer, onun matrisler için olan formudur
bir açıdan. Problem,

$$
\min_B \frac{1}{2} \sum_{(i,j) \in \omega} (Y_{ij} - B_{ij})^2 + \lambda ||B||_{tr}
\qquad (4)
$$

ki $||B||_{tr}$ iz (ya da nükleer) normudur, 

$$
||B||_{tr} = \sum_{i=1}^{r} \sigma_i(B)
$$

ile gösterilir, $r$ kertedir, $r = \mathrm{rank}(B)$, ve herhangi bir $X$ matrisi
için $\sigma_1(X) \ge .. \ge \sigma_r(X) \ge 0$, $X$'in eşsiz (singular)
değerleridir.

Minimizasyon ifadesindeki ilk toplam, $B$'deki değerleri zaten görülen,
bildiğimiz değerlere $Y$'ye karesel kayıp yakın tut diyor, ve ona bir ayar
parametresi üzerinden $B$'nin iz normunu ekliyoruz, bu istatistiki bağlamda
bir tür regülarizasyon yapmış oluyor. Eğer bu ek olmsaydı problem kötü
konumlanmış (ill-posed) olurdu [3, 15:06]. Eğer o terim olmasaydı ve
$\lambda$ sıfır olsaydı o zaman optimizasyon $B$'yi $Y$'ye eşitlerdik, oldu
bitti derdik, ama o zaman hiçbir iş yapmamış olurduk. Sağdaki terim ekiyle
yapmaya uğraştığımız $Y$'ye olabildiğince yakın bir $B$ seçmek ve bu
$B$'nin düşük kerte olmasını zorlamak. Bu bir regülarizasyon yöntemi,
diyelim $B$ şu şekilde 

$$
\underbrace{B}_{m \times n} = 
\underbrace{U}_{m \times k}
\underbrace{V^T}_{k \times n}
$$

yani $B$'yi güya oluşturan birer $U,V$'nin boyutlarındaki $k$'nin
olabildiğince düşük olmasını istiyoruz. 

Bir diğer bakış açısı, daha önceki Lasso'yla ilintilendirmek bağlamında, iz
normu L1-normun matrisler için olan versiyonu olarak görmek. Eğer elimde
bir köşegen matris olsaydı iz köşegendeki öğelerin (tek öğeler onlar)
toplamı olurdu, ki bu bir vektörün 1-norm'unu almak gibi değil mi? Eğer
köşegeni bir vektör gibi görürsem, ve matriste bu vektörden başka bir şey
yoksa.. bağlantıyı görüyoruz herhalde. 

Bir anlamda $||B||_{tr}$ $B$ matrisinin kertesini yaklaşıksal olarak temsil
ediyor çünkü kerte bir matrisin sıfır olmayan eşsiz değerlerinin
sayısıdır. $||B||_{tr}$ tabii ki eşsiz değerlerinin sayısı değil onların
kendilerinin toplamı, ama yaklaşıksal olarak kullanabileceğimiz bir şey bu
y [3, 17:49] , çünkü değer toplamı dışbükey [altgradyan, türevi
alınabildiği için bu yaklaşıklık seçilmiş herhalde].  (4) problemi tamamen
dışbükey bu arada, ilk terim dışbükey, ve ikinci terim düzgün bir norm'dur
o zaman dışbükeydir.

Bilimciler proksimal gradyan kullanmadan önce bu problem zor bir
problemdi. Optimizasiyon problemi bir yarı kesin (semidefinite) program
olarak tanımlanır. Eskiden bu tür problemleri iç nokta teknikleri ile
çözüyorduk, ve bu teknikler üstteki gibi problemler üzerinde oldukca
yavaştır. 

Devam edelim, problemi şöyle hazırlarız, bir yansıtma operatörü $P_\Omega$
tanımlayalım, gözlenen kümeye yansıtma yapacağız, 

$$
[P_\Omega (B) ]_{ij} = 
\left\{ \begin{array}{ll}
B_{ij} & (i,j) \in \Omega \\
0 & (i,j) \notin \Omega 
\end{array} \right.
$$

Bu operatörün yaptığı verilen bir matrisin gözlem olmayan her öge için
değeri sıfır yapmak, yoksa olduğu gibi bırakmak. Simdi kriteri yazabiliriz, 

$$
f(B) = 
\underbrace{\frac{1}{2} || P_\Omega (Y) - P_\Omega(B) ||_F^2}_{g(B)} + 
\underbrace{\lambda ||B||_{tr}}_{h(B)}
$$

Problemi proksimalin beklediği $g + h$ formuna soktuk. Hatırlatalım, $g$
dışbükey ama pürüzsüz değil. Bu yaklaşım için gerekenler neydi? Pürüzsüz
kısım için gradyan hesaplayabilmek, ve pürüzsüz olmayan kısım için prox
operatörünü hesaplayabilmek. Gradyan oldukca basit,

$$
\nabla g(B) = -(  P_\Omega (Y) - P_\Omega(B) ) 
\qquad (8)
$$

Sadece işaretleri biraz değiştirdim çünkü onu sonra gradyandan
çıkartacağız, o sebeple parantez dışında bir eksi işareti olması
faydalı. Şimdi ikinci terim için prox operatörünü görelim. 

$$
\mathrm{prox}_t(B) = \arg\min_Z \frac{1}{2t} || B - Z ||_F^2 + \lambda ||Z||_{tr}
\qquad (5)
$$

Tüm mümkün $Z$ matrisleri üzerinden minimizasyon yapıyoruz, ve $B_i$ ile
$Z_i$ arasındaki tüm farkların kare toplamlarını alıyoruz, artı bir sabit
çarpı $Z$'nin iz normu. Yani optimizasyon hedefi $Z$, ve bu minimizasyonu
prox operatörünü $B$ üzerinde uygulayarak elde ediyoruz [3, 22:15]. 

Üstteki ifadeyi hesaplamak için altgradyan matematiğine inmek gerekiyor
biraz. 

$$
\mathrm{prox}_t(B) = S_{\lambda t} (B)
$$

Yani prox'u $\lambda$ seviyesinde bir matriste yumuşak eşikleme olarak
görmek mümkün. Yani aynen 1-norm'un prox operatörünün vektörsel yumuşak
eşikleme olduğu gibi burada matris seviyesinde bir eşikleme var [3, 22:46].
Bu demektir ki herhangi bir matris $B$'nin prox operatörü için $B$'nin
SVD'sini alıyoruz, yani $B = U\Sigma V^T$ diyoruz, ve köşegen matris
$\Sigma$ üzerinde yumuşak eşikleme işletiyoruz, bunu yapmak için ya
köşegendeki her ögeden $\lambda_i$ çıkartıyoruz ve eğer pozitifse o değeri
kullanıyoruz, ya da sıfır alıyoruz. 

$$
S_\lambda (B) = U \Sigma_\lambda V^T
$$
 
$$
(\Sigma_\lambda)_{ii}  = \max \{ \Sigma_{ii} - \lambda_i 0 \}
$$

$S_\lambda (B)$ normal $B$'den daha düşük kerteli olacaktır, çünkü üstteki
işlem ile $\Sigma_\lambda$'nin köşegenindeki bazı değerler sıfırlanır, ve
bu matris ile hesaplanan $U \Sigma_\lambda V^T$, çarpım sırasında
$U,V$'deki bazı satırlar, kolonları da sıfırlar [3, 26:00].

Peki $\mathrm{prox}_t(B) = S_{\lambda t} (B)$ nereden geliyor? Prox operatörünü
oluşturan kriterin (5) altgradyanını alırız, sıfıra eşitleriz, ve ortaya
çıkan formülü doğrulayan $Z$'yi buluruz. Cevabın ne olacağını biliyoruz,
sadece tarif edilen altgradyan vs sonrası aynı formülü bulup
bulmayacağımızı kontrol edeceğiz. 

(5)'teki ilk terim puruzsuz, direk gradyan alinir, 

$$
0 \in Z - B + \lambda t \cdot \partial ||Z||_{tr} 
\qquad (7)
$$

İz normunun altgradyanını gösterelim, 

$$
\partial ||Z||_{tr} = \{  UV^T + W : ||W||_{op} < 1, U^TW = 0, WV = 0 \}
\qquad (6)
$$

Üstteki notasyon diyor ki aradığımız $W$'ler en büyük eşsiz değeri 1'den
küçük olan ve $U$'nun kolonlarına ve $W$'nin satırlarına dikgen
olmalı. Yıne sonradan doğrulama ile ispat yapılabilir, üstteki tanımı baz
alarak $Z = S_{\lambda t}(B)$ olarak formüle sokarsak sıfır sonucu
aldığımızı göreceğiz. Ek bazı bilgiler, iz normu ve op normu birbirinin
ikizidir. Yani

$$
||Z||_{tr} = \max_{||Y||_{op} \le 1}  Z \cdot Y
$$

Üstteki $\cdot$ işareti öğesel çarpım ve bu çarpımların toplamı
anlamında. $||Y||_{op} \le 1$ şartına uyan tüm $Y$'lere bakıyorum, ve bu
$Y$'lerle $Z$ arasında ögesel çarpım yapıyorum topluyorum bu bana $Z$'nin
iz normunu veriyor. 
 
Ya da

$$
= \max_{||Y||_{op} \le 1}  \mathrm{tr} (Z^TY)
$$

işlemi de aynı kapıya çıkar, bahsettiğimiz ikizlik sayesinde bu mümkün
oluyor. Devam edersek, üstteki bir $\max$ ifadesi ve bu ifadeleri
altgradyanını almayı gördük, üstteki ifade üzerinden, $||Y||_{op} \le 1$
şartına uyan tüm $Y$'ler içinden $\mathrm{tr}{Z^TY}$ maksimumunu gerçekleştiren,
eşsiz değerlerin toplamını / iz normunu veren $Y$'leri istiyoruz. Ve (6)
içindeki tüm $U,V$'lerin bunu yapabileceğini kontrol edebilirsiniz [3,
31:56], $Z$ SVD'sinden gelen $U,V$'lerin herhangi bir öğesel çarpımı size
$Z$'nin eşsiz değerlerinin toplamını verecektir, ve tasarım itibariyle
onların operatör normu en fazla 1'dır. $ UV^T$'un operatör normu en fazla
1'dır, $W$ için aynı şekilde, ve her iki terim birbirine dikgen olacak
şekilde tasarlanmıştır. Tüm bu matrislerin $||Z||_{tr} $'in altgradyani
olduğunu ispatlamanın yolu budur. 

Neyse üsttekilerin bize altgradyan verdiğini ispatladıktan sonra
$Z = S_{\lambda t}(B)$'i (7)'ye sokup ispatı tamamlıyoruz.  

Bu oldukca zor bir prox operatörüydü tabii. Hesaplamanabilir olduğu
anlaşıldığında pek çok araştırmacı sevinmişti, pek çok yerde kullanıldı,
vs. İspatı size ödev olarak veriyorum ama rutin bir ödev sorusu olmadığını
belirtmek isterim. 

Artık algoritmayı oluşturabiliriz, $g$ gradyanının negatif yönünde adım
atıyoruz. Gradyanı (8)'de görmüştük. 

$$
B^+ = S_{\lambda t} \bigg( 
B + t (P_\Omega(Y) - P_\Omega(B) )
\bigg)
$$

Büyük parantez içini gradyan ile hesaplıyoruz sonra elde edileni yumuşak
eşikten geçiriyoruz. Bu ne demekti? Parantez içindekilerin SVD'sini al,
eşsiz değeri $\lambda t$'den küçük olan tüm değerleri sıfırla, ve
kalanlardan $\lambda t$ çıkar, ve bu yeni değerlerle yeni bir matris
yarat. Prox operatörünü uygulamak bu demek.

Bu arada pürüzsüz kısmımızdaki gradyan $\nabla g(B)$, $L=1$ ile Lipschitz
süreklidir. (8)'deki ifade lineer, sabit 1 üzerinden tabii ki
Lipschitz. Yakınsama analiz bilgimiz bize diyor ki proksimal gradyan
inişinde adım büyüklüğü en fazla $1/L$ olabilir, ki burada 1 sadece. Bu
problemin güzel taraflarından biri bu, olabilecek en büyük adım büyüklüğünü
kullanabiliyoruz ve onu hesaplaması kolay, sadece 1.

Ve $t=1$'i üstteki formüle sokuyoruz, 

$$
B^+ = S_{\lambda t} \bigg( B + P_\Omega(Y) - P_\Omega(B)  \bigg)
$$

elde ediyoruz. Formüldeki $B - P_\Omega(B)$  bize görülmeyen kümedeki
herşeyi veriyor değil mi? $B$'de herşey var, $P_\Omega(B)$'de görülen
kümedeki $B$ öğeleri var (geri kalanlar sıfır), o zaman bu çıkartmayı
yapınca görülmeyen kümedeki herşeyi elde ederiz. 

$$
= S_{\lambda t} \bigg( P_\Omega(Y) + P_{\Omega^C}(B) \bigg)
$$
  
olarak yazabiliriz, $P_{\Omega^C}(B)$, $P_{\Omega}(Y)$'in tamamlayıcısı
(complement), görülmüş öğeler için sıfır, görülmemiş $ij$ öğeleri için
$B_{ij}$.  

Algoritmanın mantığı çok doğal, her adımda tahminim $P_{\Omega^C}(B)$'a
bakıyorum, burada olanların görülmemiş kümede ne yapacağımı kontrol
etmesine izin veriyorum, artı $P_\Omega(Y)$ ile orijinal veride görülen
öğeleri alıyorum.

Yani bir matris oluşturuyorum, görülen yerlerde $Y_{ij}$ var görülmeyen
yerlerde $B_{ij}$ var. Sonra bu matrisi düşük kerteli hale getiriyorum,
SVD'sini alıyorum, eşsiz değerlerini işleyerek bazılarını sıfırlayıp tekrar
matrisi oluşturuyorum yani. Ve bu tarif edilenleri ardı ardına
yapıyorum. Elde edileni yeni $B$ yap, mevcut olmayan ogeler icin bu matrisi
kullan, vs. 

Bu algoritmaya yumuşak atfetme (soft-impute) algoritmasi da deniyor, çünkü
her adımda kayıp değerleri "yüklüyoruz'',  yaklaşık olarak atıyoruz, ve
bunu yumuşak bağlamda yapıyoruz, eşikleme ile yavaş yavaş kerteye
düşürüyoruz. 

Bu arada üstteki yaklaşım ilk bulunduğunda proksimal gradyan olduğu
bilinmiyordu. Bilimci yaklaşımı buluyor, hakkında makale yazıyor, sonra
sonra makaleyi revize ederken birdenbire anlıyor ki bu metot proksimal
gradyan.  Daha önce gördüğümüz İSTA da biraz böyle aslında, değil mi? Kendi
başına durabilecek, gayet doğal bir mantığı olan bir algoritma, sanki
proksimal yapısı dışında da bulunabilecek bir şey.

Devam edelim, algoritmadaki prox operatörü ne kadar pahalı? Her adımda SVD
işletmemiz lazım, bu İSTA'dan farklı. SVD oldukca pahalı bir işlemdir,
özellikle büyük matrisler için. 

[atlandi]

Matris tamamlama böyle. Şimdi proksimal gradyan inişine kategorik olarak
bakalım, PGİ gradyan inişinin genelleştirilmiş halidir dedik. $f = g + h$
kriterini çözüyorsak, 

$h=0$ ise normal gradyan inişini elde ederiz. 

$h = I_C$ ise yansıtılan gradyan inişini elde ederiz. 

$g=0$ ise proksimal minimizasyon algoritmasini elde ediyoruz. Bu durumda
kriterde sadece pürüzsüz olmayan bir fonksiyon var, bu algoritma altgradyan
inişine bir alternatif olarak kullanılabiliyor. 

Tabii üstteki tüm özel durumların da yakınsama oranı $O(1/\epsilon)$. Bunu
biliyoruz çünkü proksimal gradyanin teorik yakınsama oranı öyle [3,
48:30]. 

Ekler

Örnek kod, Lasso problem çözümü [2], pür proksimal gradyan inişi, iz sürme yok

```python
import pandas as pd
diabetes = pd.read_csv("../../stat/stat_120_regular/diabetes.csv",sep=';')
y = np.array(diabetes['response'].astype(float)).reshape(442,1)
X = np.array(diabetes.drop("response",axis=1))
N,dim = X.shape
print (N,dim)

lam = 1/np.sqrt(N);
w = np.matrix(np.random.multivariate_normal([0.0]*dim, np.eye(dim))).T

L = (np.linalg.svd(X)[1][0])**2
print(L)
max_iter = 500

def obj(w):
    r = X*w-y;
    return np.sum(np.multiply(r,r))/2 +  lam * np.sum(np.abs(w))

def f_grad(w):
    return  X.T*(X*w-y) 

def soft_threshod(w,mu):
    return np.multiply(np.sign(w), np.maximum(np.abs(w)-mu,0))  

w = np.matrix([0.0]*dim).T
for t in range(0, max_iter):
    obj_val = obj(w)
    w = w - (1/L)* f_grad(w)
    w= soft_threshod(w,lam/L)    
    if (t % 50==0):
        print('iter= {},\tobjective= {:3f}'.format(t, obj_val.item()))

print (w)
```

```
442 10
4.0242141761466925
iter= 0,	objective= 6425460.500000
iter= 50,	objective= 5751070.568959
iter= 100,	objective= 5750285.357193
iter= 150,	objective= 5749670.506866
iter= 200,	objective= 5749177.635558
iter= 250,	objective= 5748779.527464
iter= 300,	objective= 5748457.810485
iter= 350,	objective= 5748197.804952
iter= 400,	objective= 5747987.670443
iter= 450,	objective= 5747817.840900
[[  -8.71913404]
 [-238.35531517]
 [ 522.93302022]
 [ 323.11825944]
 [-526.09642955]
 [ 265.58097894]
 [ -17.84381222]
 [ 143.15165377]
 [ 652.14114865]
 [  68.55685031]]
```

Kaynaklar

[1] Tibshirani, *Convex Optimization, Lecture Video 8*, 
[https://www.youtube.com/channel/UCIvaLZcfz3ikJ1cD-zMpIXg](https://www.youtube.com/channel/UCIvaLZcfz3ikJ1cD-zMpIXg)

[2] He, *IE 598 - Big Data Optimization*,  
    [http://niaohe.ise.illinois.edu/IE598_2016/](http://niaohe.ise.illinois.edu/IE598_2016/)

[3] Tibshirani, *Convex Optimization, Lecture Video 9*, 
[https://www.youtube.com/channel/UCIvaLZcfz3ikJ1cD-zMpIXg](https://www.youtube.com/channel/UCIvaLZcfz3ikJ1cD-zMpIXg)







