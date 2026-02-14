# Olasılıksal Matris Ayrıştırması (Probabilistic Matrix Factorization) ve Film Tavsiyeleri 

Tavsiye sistemleri film, şarkı gibi ürünler üzerinde yapılan beğeni
notları üzerinden o kullanıcının not vermediği filmler, şarkılar
üzerinde bir tahmin üretmeye uğraşır. Diyelim ki $N$ tane kullanıcı ve
$M$ tane film var, ve $i$ kullanıcısının $j$ filmine verdiği not
$R_{ij}$ üzerinde. Eğer kullanıcı ve film özelliklerini azaltılmış,
sıkıştırılmış, "gizil" bir uzay üzerinden temsil etmek istersek, ki bu
$N,M$'ye kıyasla boyutu daha küçük bir uzay olacaktır, şu şekilde
olasılıksal bir tanım yapabilirdik [1],

* $N$: kullanıcı sayısı
* $M$: film sayısı
* $K$: gizil (latent) boyut, yani sıkıştırılmış, $U,V$'nin daraltılmış uzayı
* $U_i \in \mathbb{R}^K$, $i = 1, \dots, N$ için
* $V_j \in \mathbb{R}^K$, $j = 1, \dots, M$ için
* $\mu \in \mathbb{R}$ (global ortalama)
* $R_{ij} \in \mathbb{R}$ ($I_{ij}=1$ ise bu kullanıcı o filme not vermiştir)

$$
R_{ij}
=
\mu + U_i^\top V_j + \epsilon_{ij},
\quad
\epsilon_{ij} \sim \mathcal{N}(0, \sigma^2)
\qquad (3)
$$

Bir tavsiye sisteminin eğitmek demek üstteki daha az boyutlu $U,V$
matrislerinin optimal değerlerini bulmak demektir. Bu değerleri
bulunca bir tür özet temsil elde etmiş olacağız, bu temsil bize
kullanıcının seyretmediği filmlere verebileceği notu tahmin etmemizi
sağlayacak.

### Hesap Yöntemi

Hesapsal baglamda amacımız alttaki dağılıma erişmek, yani bu "sonsal
(posterior)" dağılımdan örneklem alabilmek istiyoruz:

$$
p(U, V \mid R)
$$

Bu dağılımdan örneklem alabilmek için Gibbs tekniği kullanarak, yani
koşullu dağılımlardan tekrarlı örnekleme yaparak oraya
erisebiliriz. Mesela bir adımda $p(U_i \mid V, R)$ örneklemesi alırız,
o değerleri kullanarak sonrakinde $p(V_i \mid U, R)$ alırız, bunu ardı
ardına yapınca üstteki nihai dağılıma erisebileceğimizi
biliyoruz. Tabii $R$ verisine uyan $U,V$ değerlerine erişmek demek,
kavramsal olarak bir dağılım elde etmektir, uygulama bağlamında mesela
bir kullanıcının daha not vermediği filme not vermek için (1)
formülünü direk kullanmak mümkündür, $U,V$ ile düz matris çarpımı
yaparak bir tahmin hesaplayabiliriz.

Yani bize, kullanıcılar için, $p(U_i \mid V, R)$ formülasyonu lazım,
bunu cebirsel olarak elde etmek için tam genişletilmiş birleşik
dağılımdan başlamalıyız.

$$
p(U)
=
\prod_{i=1}^{N}
\mathcal{N}(U_i \mid 0, \lambda_U^{-1} I)
$$

Film gizil vektörleri

$$
p(V)
=
\prod_{j=1}^{M}
\mathcal{N}(V_j \mid 0, \lambda_V^{-1} I)
$$

Olurluk

Gösterge $I_{ij} = 1$ eğer $R_{ij}$ gözlemlenmis ise, yani not mevcut
ise, aksi halde 0 olarak tanımlanır.

$$
p(R \mid U, V)
=
\prod_{i=1}^{N}
\prod_{j=1}^{M}
\mathcal{N}
\left(
R_{ij}
\mid
\mu + U_i^\top V_j,
\sigma^2
\right)^{I_{ij}}
$$

Tam birleşik dağılım

$$
\begin{aligned}
p(U, V, R)
=
p(R \mid U, V) \cdot p(U) \cdot p(V)
=
\left[
\prod_{i=1}^{N}
\prod_{j=1}^{M}
\mathcal{N}
\left(
R_{ij}
\mid
\mu + U_i^\top V_j,
\sigma^2
\right)^{I_{ij}}
\right] \\
\quad \times
\left[
\prod_{i=1}^{N}
\mathcal{N}(U_i \mid 0, \lambda_U^{-1} I)
\right] \\
\quad \times
\left[
\prod_{j=1}^{M}
\mathcal{N}(V_j \mid 0, \lambda_V^{-1} I)
\right]
\end{aligned}
$$

Amaç: $p(U_i \mid V, R)$ türetmek

Bayes kuralına göre (koşullu olasılığın tanımı):

$$
p(U_i \mid V, R)
=
\frac{p(U_i, V, R)}{p(V, R)}
$$

Not: Tek bir kullanıcı vektörü $U_i$ üzerine koşullandırdığımızı vurgulamak için $p(U, V, R)$ yerine $p(U_i, V, R)$ yazıyoruz. Daha kesin olarak:

$$
p(U_i \mid U_{-i}, V, R)
=
\frac{p(U_i, U_{-i}, V, R)}{p(U_{-i}, V, R)}
$$

burada $U_{-i} = \{U_1, \dots, U_{i-1}, U_{i+1}, \dots, U_N\}$, $U_i$
hariç tüm kullanıcı vektörlerini gösterir.

Payda $U_i$'ye bağlı değildir, bu nedenle onu bir normalizasyon sabiti
olarak ele alabiliriz.

$U_i$ içeren tüm terimleri izole et

Adım 1: Birleşik dağılımı genişlet

Tam birleşik dağılımdan başlayarak:

$$
p(U_i, U_{-i}, V, R)
=
p(R \mid U_i, U_{-i}, V) \cdot p(U_i) \cdot p(U_{-i}) \cdot p(V) 
$$

$U_i$ üzerindeki önseli geri kalanından ayır:

$$
p(U_i, U_{-i}, V, R)
=
\mathcal{N}(U_i \mid 0, \lambda_U^{-1} I)
\times
\left[
\prod_{k \neq i} \mathcal{N}(U_k \mid 0, \lambda_U^{-1} I)
\right]
\times
p(V) 
\times
p(R \mid U_i, U_{-i}, V)
\qquad (1)
$$

Adım 2: Şimdi (1) içindeki $p(R \mid U_i, U_{-i}, V)$ terimine dikkat et.
Bunu nasıl genişletebiliriz? İşte nasıl.

$U_i$ içeren olabilirlik terimlerini izole et

Olabilirlik tüm gözlemlenen derecelendirmeler üzerinden çarpanlara ayrılır:

$$
p(R \mid U_i, U_{-i}, V)
=
\prod_{i=1}^{N}
\prod_{j=1}^{M}
\mathcal{N}
\left(
R_{ij}
\mid
\mu + U_i^\top V_j,
\sigma^2
\right)^{I_{ij}}
$$

Sadece kullanıcı $i$'den gelen derecelendirmeler $U_i$'ye bağlıdır.

Tanım:
$$
\Omega_i = \{ j \mid I_{ij} = 1 \}
$$
(kullanıcı $i$ tarafından derecelendirilen filmlerin kümesi)

O zaman ayırabiliriz:

$$
p(R \mid U_i, U_{-i}, V)
=
\left[
\prod_{j \in \Omega_i}
\mathcal{N}
\left(
R_{ij}
\mid
\mu + U_i^\top V_j,
\sigma^2
\right)
\right]
\times
\left[
\prod_{k \neq i} \prod_{j=1}^M
\mathcal{N}
\left(
R_{kj}
\mid
\mu + U_k^\top V_j,
\sigma^2
\right)^{I_{kj}}
\right]
\qquad{(2)}
$$

İkinci çarpım $U_i$'ye bağlı değildir.

Bayes'i tekrar uygulayalım

$$
p(U_i \mid U_{-i}, V, R) = \frac{p(U_i, U_{-i}, V, R)}{p(U_{-i}, V, R)}
$$

Payda $U_i$'ye bağlı olmadığından, sadece bir normalizasyon sabitidir, dolayısıyla:

$$
p(U_i \mid U_{-i}, V, R) \propto p(U_i, U_{-i}, V, R)
$$

Sağ taraf (1)'dir,

$$
p(U_i, U_{-i}, V, R)
=
\mathcal{N}(U_i \mid 0, \lambda_U^{-1} I)
\times
\left[
\prod_{k \neq i} \mathcal{N}(U_k \mid 0, \lambda_U^{-1} I)
\right]
\times
p(V) 
\times
p(R \mid U_i, U_{-i}, V)
$$

Ve onu (2) ile genişletiyoruz

$$
= \mathcal{N}(U_i \mid 0, \lambda_U^{-1} I)
\times
\left[
\prod_{k \neq i} \mathcal{N}(U_k \mid 0, \lambda_U^{-1} I)
\right]
\times
p(V) 
\times
\left[
\prod_{j \in \Omega_i}
\mathcal{N}(R_{ij} \mid \mu + U_i^\top V_j, \sigma^2)
\right]
\times
\left[
\prod_{k \neq i} \prod_{j=1}^M
\mathcal{N}(R_{kj} \mid \mu + U_k^\top V_j, \sigma^2)^{I_{kj}}
\right]
$$

Ve şimdi bundan sadece $U_i$'ye bağlı terimleri topluyoruz,

$$
\begin{aligned}
p(U_i \mid U_{-i}, V, R)
&\propto
\mathcal{N}(U_i \mid 0, \lambda_U^{-1} I)
\times
\prod_{j \in \Omega_i}
\mathcal{N}
\left(
R_{ij}
\mid
\mu + U_i^\top V_j,
\sigma^2
\right)
\end{aligned}
$$

Bu doğru olur, değil mi? Çünkü (1)'de $U_i$'ye bağlı olmayan terimler

- $\prod_{k \neq i} \mathcal{N}(U_k \mid 0, \lambda_U^{-1} I)$ — sadece diğer kullanıcılara bağlı
- $p(V)$ — sadece filmlere bağlı
- $p(R \mid U_i, U_{-i}, V, R)$'nin çoğu — sadece kullanıcı $i$'den gelen derecelendirmeler $U_i$'ye bağlı

$U_i$'ye bağlı olan terimler

- $\mathcal{N}(U_i \mid 0, \lambda_U^{-1} I)$ — $U_i$ üzerindeki önsel
- $\prod_{j \in \Omega_i} \mathcal{N}(R_{ij} \mid \mu + U_i^\top V_j, \sigma^2)$ — kullanıcı $i$'nin derecelendirmelerinin olabilirliği

$p(V)$ gibi terimler $U_i$'ye bağlı değildir, $U_i$ ne olursa olsun aynıdırlar, bu nedenle sadece normalizasyon sabitine katkıda bulunabilirler (eşitlikten $\propto$'ya geçişe dikkat edin), dolayısıyla atılırlar.

Devam edelim. $U_{-i}$ üzerine koşullandırdığımız için, şunu yazabiliriz:

$$
p(U_i \mid V, R)
\propto
\mathcal{N}(U_i \mid 0, \lambda_U^{-1} I)
\prod_{j \in \Omega_i}
\mathcal{N}
\left(
R_{ij}
\mid
\mu + U_i^\top V_j,
\sigma^2
\right)
$$

Not: Gibbs örneklemesinde, $U_i$'yi örneklediğimizde, diğer tüm
değişkenler ($U_{-i}, V$) sabit tutulur, bu nedenle gösterim basitliği
için $U_{-i}$ üzerine koşullandırmayı atıyoruz.

Log-yoğunluğa dönüştür

Normalize edilmemiş yoğunluğun logaritmasını alarak:

$$
\begin{aligned}
\log p(U_i \mid V, R)
&=
\log \mathcal{N}(U_i \mid 0, \lambda_U^{-1} I)
+
\sum_{j \in \Omega_i}
\log \mathcal{N}
\left(
R_{ij}
\mid
\mu + U_i^\top V_j,
\sigma^2
\right)
+ \text{const}
\end{aligned}
$$

Gauss log-yoğunluklarını genişlet:

$$
\log \mathcal{N}(U_i \mid 0, \lambda_U^{-1} I)
=
-\frac{1}{2} U_i^\top (\lambda_U I) U_i + \text{const}
=
-\frac{\lambda_U}{2} U_i^\top U_i + \text{const}
$$

$$
\log \mathcal{N}
\left(
R_{ij}
\mid
\mu + U_i^\top V_j,
\sigma^2
\right)
=
-\frac{1}{2\sigma^2}
\left(
R_{ij} - \mu - U_i^\top V_j
\right)^2
+ \text{const}
$$

Birleştir:

$$
\begin{aligned}
\log p(U_i \mid \cdot)
&=
-\frac{\lambda_U}{2} U_i^\top U_i
\\
&\quad
-\frac{1}{2\sigma^2}
\sum_{j \in \Omega_i}
\left(
R_{ij} - \mu - U_i^\top V_j
\right)^2
+ \text{const}
\end{aligned}
$$

Karesel terimi genişlet

Artığı tanımla:

$$
\tilde{R}_{ij} = R_{ij} - \mu 
$$

O zaman:

$$
\left(\tilde{R}_{ij} - U_i^\top V_j\right)^2
=
\tilde{R}_{ij}^2
- 2 \tilde{R}_{ij} U_i^\top V_j
+ (U_i^\top V_j)^2
$$

Not: $(U_i^\top V_j)^2 = (U_i^\top V_j)(V_j^\top U_i) = U_i^\top V_j V_j^\top U_i$

Yani:

$$
\left(\tilde{R}_{ij} - U_i^\top V_j\right)^2
=
\tilde{R}_{ij}^2
- 2 \tilde{R}_{ij} U_i^\top V_j
+ U_i^\top V_j V_j^\top U_i
$$

$j \in \Omega_i$ üzerinden toplayarak:

$$
\sum_{j \in \Omega_i}
\left(\tilde{R}_{ij} - U_i^\top V_j\right)^2
=
\sum_{j \in \Omega_i} \tilde{R}_{ij}^2
- 2 U_i^\top \sum_{j \in \Omega_i} V_j \tilde{R}_{ij}
+ U_i^\top \left(\sum_{j \in \Omega_i} V_j V_j^\top\right) U_i
$$

$U_i$ içindeki kuadratik ve lineer terimleri topla

Log-yoğunluğa geri koyarak:

$$
\begin{aligned}
\log p(U_i \mid \cdot)
&=
-\frac{\lambda_U}{2} U_i^\top U_i
-\frac{1}{2\sigma^2}
\left[
U_i^\top \left(\sum_{j \in \Omega_i} V_j V_j^\top\right) U_i
- 2 U_i^\top \sum_{j \in \Omega_i} V_j \tilde{R}_{ij}
+ \sum_{j \in \Omega_i} \tilde{R}_{ij}^2
\right]
+ \text{const}
\end{aligned}
$$

Kuadratik terimleri grupla:

$$
-\frac{\lambda_U}{2} U_i^\top U_i
-\frac{1}{2\sigma^2} U_i^\top \left(\sum_{j \in \Omega_i} V_j V_j^\top\right) U_i
=
-\frac{1}{2} U_i^\top
\left(
\lambda_U I + \frac{1}{\sigma^2} \sum_{j \in \Omega_i} V_j V_j^\top
\right)
U_i
$$

Lineer terimleri grupla:

$$
\frac{1}{\sigma^2} U_i^\top \sum_{j \in \Omega_i} V_j \tilde{R}_{ij}
$$

Sabit terim $\sum_{j \in \Omega_i} \tilde{R}_{ij}^2$, $U_i$'ye bağlı değildir.

Gauss sonsalı tanımlayalım. Şimdi elimizde:

$$
\log p(U_i \mid \cdot)
=
-\frac{1}{2} U_i^\top \Sigma_i^{-1} U_i
+
U_i^\top \Sigma_i^{-1} \mu_i
+ \text{const}
$$

Bu, çok değişkenli bir Gauss'un kanonik formudur:

$$
\log \mathcal{N}(x \mid \mu, \Sigma)
=
-\frac{1}{2}(x - \mu)^\top \Sigma^{-1} (x - \mu) + \text{const}
=
-\frac{1}{2} x^\top \Sigma^{-1} x + x^\top \Sigma^{-1} \mu + \text{const}
$$

Kesinliği (kovaryansın tersini) tanımla:

$$
\Sigma_i^{-1}
=
\lambda_U I + \frac{1}{\sigma^2} \sum_{j \in \Omega_i} V_j V_j^\top
$$

Ortalamayı tanımla (lineer terimden), $U_i^\top \Sigma_i^{-1} \mu_i =
\frac{1}{\sigma^2} U_i^\top \sum_{j \in \Omega_i} V_j
\tilde{R}_{ij}$'den:

$$
\Sigma_i^{-1} \mu_i
=
\frac{1}{\sigma^2} \sum_{j \in \Omega_i} V_j \tilde{R}_{ij}
$$

Dolayısıyla:

$$
\mu_i
=
\Sigma_i
\left(
\frac{1}{\sigma^2}
\sum_{j \in \Omega_i}
V_j
\left(
R_{ij} - \mu 
\right)
\right)
$$

Nihai sonuç:

$$
p(U_i \mid V, R)
=
\mathcal{N}(U_i \mid \mu_i, \Sigma_i)
$$


Özet: $U_i$ için koşullu sonsal

$$
\begin{aligned}
\Sigma_i^{-1}
&=
\lambda_U I + \frac{1}{\sigma^2} \sum_{j \in \Omega_i} V_j V_j^\top
\\[1em]
\mu_i
&=
\Sigma_i
\left(
\frac{1}{\sigma^2}
\sum_{j \in \Omega_i}
V_j
(R_{ij} - \mu )
\right)
\end{aligned}
$$

burada $\Omega_i = \{j : I_{ij} = 1\}$, kullanıcı $i$ tarafından
derecelendirilen filmlerin kümesidir.

Simetri: $V_j$ için koşullu sonsal

Simetri ile (kullanıcı/film indekslerini değiştirerek):

$$
p(V_j \mid U, R) = \mathcal{N}(V_j \mid \hat{\mu}_j, \hat{\Sigma}_j)
$$

burada:

$$
\begin{aligned}
\hat{\Sigma}_j^{-1}
&=
\lambda_V I + \frac{1}{\sigma^2} \sum_{i \in \Omega_j} U_i U_i^\top
\\[1em]
\hat{\mu}_j
&=
\hat{\Sigma}_j
\left(
\frac{1}{\sigma^2}
\sum_{i \in \Omega_j}
U_i
(R_{ij} - \mu )
\right)
\end{aligned}
$$

burada $\Omega_j = \{i : I_{ij} = 1\}$, film $j$'yi derecelendiren kullanıcıların kümesidir.

Gibbs örneklemesi neden işe yarar

* Tüm koşullu sonsallar Gauss'tur (kapalı form)
* Örnekleme, normalizasyon sabitlerini hesaplamayı gerektirmez
* Her güncelleme sadece yerel veriyi kullanır (o kullanıcı/filmi içeren derecelendirmeler)
* Tekrarlı taramalar bilgiyi gizil uzayda global olarak yayar
* Hafif koşullar altında gerçek sonsala yakınsama garantilidir

Gibbs Örnekleme Algoritması

Başlat: $U^{(0)}, V^{(0)}$'ı rastgele değerlere

$t = 1, 2, \dots, T$ için:

1. Her kullanıcı $i = 1, \dots, N$ için:
   - $U_i^{(t)} \sim p(U_i \mid V^{(t-1)}, R)$ örnekle

2. Her film $j = 1, \dots, M$ için:
   - $V_j^{(t)} \sim p(V_j \mid U^{(t)}, R)$ örnekle

Isınma (burn-in) sonrasında, tahmin için örnekleri kullan.

$R_{ij}$ hem $\epsilon_{ij}$ nedeniyle hem de $U_i, V_j$'nin
kendilerinin önsel dağılımlara sahip rastgele değişkenler olması
nedeniyle bir rastgele değişkendir.

### Genel Sorular

Sistemdeki rasgelelik nereden geliyor? $U,V$ rasgele değişken
midirler? Burada aslında iki seviye rastgelelik var:

1] Gözlem seviyesi rastgelelik ($\epsilon_{ij}$'den)

$U_i, V_j$'nin sabit değerleri verildiğinde, derecelendirme $R_{ij}$
hala rastgeledir çünkü:

$$
R_{ij} \mid U_i, V_j \sim \mathcal{N}(\mu + U_i^\top V_j, \sigma^2)
$$

Bu, ölçüm gürültüsünü veya derecelendirme sürecindeki doğal
stokastikliği temsil eder. Gerçek gizil özellikleri bilsek bile, bir
kullanıcı aynı filmi farklı günlerde farklı derecelendirebilir.

2] Parametre seviyesi rastgelelik ($U, V$ üzerindeki önsellerden)

Bayesçi çerçevede, $U_i, V_j$ sabit parametreler değil, önsel
dağılımlara sahip rastgele değişkenlerdir:

$$
\begin{aligned}
U_i &\sim \mathcal{N}(0, \lambda_U^{-1} I) \\
V_j &\sim \mathcal{N}(0, \lambda_V^{-1} I) 
\end{aligned}
$$

Dolayısıyla herhangi bir veri gözlemlemeden önce, $R_{ij}$ rastgeledir
çünkü parametrelerin kendileri rastgeledir.

Bayesçi perspektiften, $R_{ij}$'nin koşulsuz dağılımı (hiçbir şey
gözlemlemeden önce):

$$
p(R_{ij}) = \int p(R_{ij} \mid U_i, V_j) \cdot p(U_i, V_j) \, dU_i \, dV_j 
$$

Bu, her iki rastgelelik kaynağı üzerinden integral alır:
- Olabilirlik $p(R_{ij} \mid U_i, V_j)$, $\epsilon_{ij}$ gürültüsünü yakalar
- Önsel $p(U_i, V_j)$, parametreler hakkındaki belirsizliğimizi yakalar

Pratikte:

Gibbs örneklemesi yaptığımızda:

- Bazı derecelendirmeleri $R_{ij}$ gözlemliyoruz (onları sabit veri olarak ele alıyoruz)
- Bu gözlemler verildiğinde $U, V$'nin sonsal dağılımını çıkarıyoruz
- $\epsilon_{ij}$ terimleri olabilirlik yoluyla örtük olarak "integral alınarak çıkarılır"

Yani model şunu söyler:

- Veri görmeden önce: Her şey ($U, V, R$) rastgeledir
- Veri gördükten sonra: Gözlemlenen $R_{ij}$ üzerine koşullandırır ve $U, V$ için dağılımları çıkarırız
- Tahmin için: Yeni $R_{ij}^*$'yi tahmin etmek için sonsal örneklerini kullanırız, bu hem parametre belirsizliğini hem de $\epsilon$ gürültüsünü içerir

### Yeni Kullanıcı

Sistemi eğitip $U,V$ elde ettikten sonra mesela mevcut 100'uncu
kullanıcı için tavsiye üretmek basit olurdu. $U$ matrisinin 100'uncu
satırına gideriz, bu satır $1 \times K$ boyutundadır, sonra o satır
ile $V$ matrisini çarparız, $1 \times K$ çarpı $K \times M$ bize $1
\times M$ boyutunda bir vektör veriyor, $\mu$ ile toplayınca tüm
filmlere verilmiş tahmini notlar böylece hesaplanmış olur.

Fakat ya eğer tavsiye üretmek istediğimiz kullanıcı yeni bir kullanıcı
ise, yani verisi eğitim fazına dahil edilmemiş bir kişi ise? Bu
durumda bu kişinin $U$ matrisinde satırı yoktur. O zaman bu kişi $U$
matrisinde olsaydı nasıl bir satır verisi, diyelim $u_{ben}$'e, sahip
olurdu sorusunu cevaplamak gerekir, yani bu amaç için bir hesap
yöntemi bulmak gerekir.

Şöyle bir yaklaşım olabilir, öyle bir $u_{\textrm{ben}}$ bul ki onun
$V$ ile çarpımı artı global ortalama kullanıcının oylamış olduğu
filmler için verilen nota yakın bir değer versin. Bu bir tür regresyon
hesabı olabilir aslında. Herhangi bir $j$ filmi için

$$
\hat{r}_j = \mu + u_{ben}^T v_j
$$

diyebiliriz. En iyi $u_{ben}$ vektörünü bulmak için karesi alınmış
hataların toplamını minimize edebiliriz, bu hatalar benim gerçekten
verdiğim notlar $r_j$ ile tahmin edilen notlar $\hat{r}_j$ arasında
olur, bir de regülarize edici terim ekleriz ki aşırı öğrenme
(overfitting) engellenmiş olsun. Yani amac $J(u_{ben})$'nin
minizasyonu.

$$
J(u_{ben}) = \sum_{j \in \Omega_j} (r_j - (\mu + u_{ben}^T
v_j))^2 + \lambda_U \|u_{ben}\|^2
$$

Basitleştirmek için global ortalamayı notlardan çıkartabiliriz, $y_j =
r_j - \mu$. Formül şöyle olur,

$$
J(u_{ben}) = \sum_{j \in \text{Rated}} (y_j - u_{ben}^T v_j)^2 +
\lambda_U \|u_{ben}\|^2
$$

Formülü matrisler kullanacak şekilde adapte edebiliriz, diyelim ki
$V$'nin alt kümesi olan bir $V_{rated}$ yarattım bu matris $V$'nin
benim oy verdiğim satırlarını içeriyor, eğer $B$ tane film oylamışsam,
$V_{rated}$ matrisi $B \times K$ olur.

$$
J(u_{ben}) = \|y - V_{rated}u_{ben}\|^2 + \lambda_U u_{ben}^T u_{ben}
$$

$$
J(u_{ben}) = (y - V_{rated}u_{ben})^T (y - V_{rated}u_{ben}) + \lambda_U u_{ben}^T u_{ben}
$$

Şu terimi açalım, $(y - V_{rated}u_{ben})^T (y - V_{rated}u_{ben})$,

$$
J(u_{ben}) = (y^T - u_{ben}^T V_{rated}^T) (y - V_{rated}u_{ben})
+ \lambda_U u_{ben}^T u_{ben}
$$

Çarpımı yapalım,

$$
J(u_{ben}) = y^T y - y^T V_{rated} u_{ben} - u_{ben}^T
V_{rated}^T y + u_{ben}^T V_{rated}^T V_{rated} u_{ben} + \lambda_U
u_{ben}^T u_{ben}
$$

Üstte görülen $y^T V_{rated} u_{ben}$ bir tek sayı olduğu için kendi
devriğine eşittir, o yüzden ortadaki terimleri birleştirebiliriz,

$$
J(u_{ben}) = y^T y - 2u_{ben}^T V_{rated}^T y + u_{ben}^T
V_{rated}^T V_{rated} u_{ben} + \lambda_U u_{ben}^T u_{ben}
$$

Şimdi $J(u_{ben})$'in $u_{ben}$'a göre türevini alalım, ve minizasyon
için sıfıra eşitleyelim. Matris Calculus kurallarından biliyoruz ki

$\frac{\partial}{\partial u} (u^T A u) = 2Au$ (eger $A$ simetrik ise)

$\frac{\partial}{\partial u} (u^T b) = b$

O zaman 

$$
\frac{\partial J}{\partial u_{ben}} =
0 - 2V_{rated}^T y + 2V_{rated}^T V_{rated} u_{ben} + 2\lambda_U u_{ben} = 0
$$

2 ile bölelim ve $u_{ben}$ sol tarafta kalacak şekilde tekrar düzenleyelim,

$$
V_{rated}^T V_{rated} u_{ben} + \lambda_U u_{ben} = V_{rated}^T
y$$

$u_{ben}$'i dışarı çekelim,

$$
(V_{rated}^T V_{rated} + \lambda_U I) u_{ben} = V_{rated}^T y
$$

$$
u_{ben} = (V_{rated}^T V_{rated} + \lambda_U I)^{-1} V_{rated}^T
y$$

$$(V_{rated}^T V_{rated} + \lambda_U I)u_{ben}  = V_{rated}^T y$$

$u_{ben}$ için çözelim,

$$
u_{ben} = (V_{rated}^T V_{rated} + \lambda_U I)^{-1} V_{rated}^T y
$$

Üstteki bir Sırt (Ridge) Regresyon tanımıdır [5]. 

### Kodlama

Ekteki kodlar arasında `sng_bpmf.py` dosyası tek islemci ile Gibbs
örneklemesi yapar, `par_bpmf.py` ise aynı işlemi paralel şekilde
yapar. Detaylar için kodlara bakılabilir.  Veri olarak [2],[3]
kullanmak mümkün, bu verilerin dizin olarak `/opt/Downloads` altında
olduğunu farz ediyoruz. Verinin hazırlanması için `pmf/prep1.py`,
`pmf/prep2.py`, `pmf/prep3.py`, `pmf/prep4.py` script'leri o sırada
işletilmeli. Bu scriptler ile kullanıcı ve film kimlikleri tekrar
üretiliyor, tüm film ve kullanıcılar sıfırdan başlayıp birer birer
artacak şekilde tekrar kimlikleniyor. Bunun yapılmasının sebebi bu
kimlik değerlerinin $U,V$ üzerinde direk satır erişimi için
kullanılabilmesi.. Daha sonra `user_movie.txt` ve `movie_user.txt`
dosyaları yaratılıyor, bu dosyalarda her satır, mesela
`user_movie.txt` için diyelim, satır başında kullanıcı kimliği
ardından aynı satırda o kullanıcının verdiği film notlarını içerir. Bu
şekilde tek bir satır okuması ile o kullanıcı hakkında tüm bilgileri
alabilmiş oluyoruz (filmler için benzer şekilde). Movielens verisine
bakanlar farketmiş olabilir, oradaki `ratings.csv` içinde bu tür bir
satırsal temsil yoktur.

Gibbs işlemi bitince sonuçlar, sonsal dağılımlar, bir `.npz` dosyasına
yazılır, ve `recom.py` koduyla bu çıktılar kullanılarak taviyeler
üretilebilir. Tavsiye kodu kullanıcının beğendiği filmleri, notlar
okumak için su anda `~/Documents/kod/movpicks.csv` dosyasını
kullanıyor, burada benim kendi seçimlerim var, mesela

```
movie,rating
..
Swordfish (2001),5
Dunkirk (2017),2
Tombstone (1993),5
..
```

diye notlar vermişim. Bu notlar kullanılarak ve üstteki matematik
kullanılarak tavsiyeler üretiliyor.

Paralellik

Üstte görülen Gibbs örnekleme algoritmasında dikkat edersek
kullanıcılar için bir döngü var, onun içinde kullanıcı 1,2,3.. diye
giden ve o kullanıcıların $U$ satırlarını örnekleyen (yani mevcut
satırın üzerine yazan) bir yaklaşım var. Bu yaklaşıma göre
kullanıcıların satırlarının örneklenmesi birbirinden bağımsız, yani
kullanıcı 100 örneklenmesi için kullanıcı 99'un işinin bitmiş olmasını
beklemiyoruz. Bu demektir ki örneklem işlemi kullanıcı ve film bazında
paralel şekilde işletilebilir.

Kullanıcılar için 10 tane paralel süreç başlatırız, bu süreçler
kullanıcıları ve onun verdiği notları 10 parçaya böler, her biri kendi
içinde örneklem işini yapar, kendi $U$ parçasını üretir, tümü bitince
$U$ parçaları birleştirilip yeni $U$ oluşturulur ve filmler için aynı
işlem yapılır, 10 parçaya bölünür, orneklenip birlestirilir vs. Bu bir
dongu (iteration) olur.

Bölme işlemini `user_movie.txt` ve `movie_user.txt` üzerinden basit
bir şekilde yapabiliriz, kullanıcı ve film kimlik değerleri artık
direk $U,V$ satırlarına tekabül ettiği için erişimde zorluk
çıkmaz. Her süreç kendi verisine odaklanır, ama $U,V$ verisinin her
süreç içinde kopyalanması problem değil, çünkü bunlar nispeten ufak
matrislerdir, bellek için yük oluşturmazlar.

Kodlar

[sng_bpmf.py](sng_bpmf.py),
[par_bpmf.py](par_bpmf.py),
[util.py](util.py),
[prep1.py](prep1.py),
[prep2.py](prep2.py),
[prep3.py](prep3.py),
[prep4.py](prep4.py),
[recom.py](recom.py),
[rmse_mypicks.py](rmse_mypicks.py)

Kaynaklar

[1] Salakhudtinov,  <a href="https://www.cs.toronto.edu/~amnih/papers/bpmf.pdf">Bayesian Probabilistic Matrix Factorization using Markov Chain Monte Carlo</a>

[2] Netflix, <a href="https://grouplens.org/datasets/movielens/latest/">MovieLens Small (ml-latest-small)</a>

[3] Netflix, <a href="https://grouplens.org/datasets/movielens/32m/">MovieLens 32M, (ml-32m)</a>

[4] Anton Gerber Sort, <a href="https://research-api.cbs.dk/ws/portalfiles/portal/98731723/1641765_Thesis_Anton_Sort.pdf">Probabilistic Matrix Factorisation in Collaborative Filtering, Thesis</a>

[5] Bayramli, <a href="../stat_120_regular/stat_120_regular.html">Regresyon, Ridge, Lasso, Çapraz Sağlama, Regülarize Etmek</a>

