# Olasılıksal Matris Ayrıştırması (Probabilistic Matrix Factorization) ve Film Tavsiyeleri 

Sonsal dağılımdan örneklem almak istiyoruz:

$$
p(U, V \mid R)
$$

Bunu Gibbs örneklemesi kullanarak, yani koşullu dağılımlardan tekrarlı
örnekleme yaparak başarmayı umuyoruz,

$$
p(U_i \mid V, R)
$$

Bunu doğru yapmak için, tam genişletilmiş birleşik dağılımdan başlamalıyız.

Rastgele değişkenler ve boyutlar

* $N$: kullanıcı sayısı
* $M$: film sayısı
* $K$: gizil (latent) boyut, yani sıkıştırılmış, $U,V$'nin daraltılmış uzayı

Değişkenler:

* $U_i \in \mathbb{R}^K$, $i = 1, \dots, N$ için
* $V_j \in \mathbb{R}^K$, $j = 1, \dots, M$ için
* $\mu \in \mathbb{R}$ (global ortalama)
* $R_{ij} \in \mathbb{R}$ ($I_{ij}=1$ ise bu kullanıcı o filme not vermiştir)

Üretici model

Her gözlemlenen derecelendirme $R_{ij}$ için:

$$
R_{ij}
=
\mu + U_i^\top V_j + \epsilon_{ij},
\quad
\epsilon_{ij} \sim \mathcal{N}(0, \sigma^2)
$$

Önsel dağılımlar

Kullanıcı gizil vektörleri

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

Ortalamayı tanımla (lineer terimden):

$U_i^\top \Sigma_i^{-1} \mu_i = \frac{1}{\sigma^2} U_i^\top \sum_{j \in \Omega_i} V_j \tilde{R}_{ij}$'den:

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


Özet: $U_i$ için koşullu posterior

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

Simetri: $V_j$ için koşullu posterior

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

* Tüm koşullu posterior'lar Gauss'tur (kapalı form)
* Örnekleme, normalizasyon sabitlerini hesaplamayı gerektirmez
* Her güncelleme sadece yerel veriyi kullanır (o kullanıcı/filmi içeren derecelendirmeler)
* Tekrarlı taramalar bilgiyi gizil uzayda global olarak yayar
* Hafif koşullar altında gerçek posterior'a yakınsama garantilidir

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

Burada aslında iki seviye rastgelelik var:

1 - Gözlem seviyesi rastgelelik ($\epsilon_{ij}$'den)

$U_i, V_j$'nin sabit değerleri verildiğinde, derecelendirme $R_{ij}$
hala rastgeledir çünkü:

$$
R_{ij} \mid U_i, V_j \sim \mathcal{N}(\mu + U_i^\top V_j, \sigma^2)
$$

Bu, ölçüm gürültüsünü veya derecelendirme sürecindeki doğal
stokastikliği temsil eder. Gerçek gizil özellikleri bilsek bile, bir
kullanıcı aynı filmi farklı günlerde farklı derecelendirebilir.

2 - Parametre seviyesi rastgelelik ($U, V$ üzerindeki önsellerden)

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

1. Bazı derecelendirmeleri $R_{ij}$ gözlemliyoruz (onları sabit veri olarak ele alıyoruz)
2. Bu gözlemler verildiğinde $U, V$'nin posterior dağılımını çıkarıyoruz
3. $\epsilon_{ij}$ terimleri olabilirlik yoluyla örtük olarak "integral alınarak çıkarılır"

Yani model şunu söyler:

- Veri görmeden önce: Her şey ($U, V, R$) rastgeledir
- Veri gördükten sonra: Gözlemlenen $R_{ij}$ üzerine koşullandırır ve $U, V$ için dağılımları çıkarırız
- Tahmin için: Yeni $R_{ij}^*$'yi tahmin etmek için posterior örneklerini kullanırız, bu hem parametre belirsizliğini hem de $\epsilon$ gürültüsünü içerir

```python
import numpy as np, sys, csv, json, os
import pandas as pd

csv.field_size_limit(sys.maxsize)

K = 25
N_ITERS = 8
BURN_IN = 1
THIN = 2

lambda_U = 5.0
lambda_V = 5.0
lambda_b = 2.0
lambda_c = 2.0
sigma2 = 1

#d = "/opt/Downloads/ml-32m"
d = "/opt/Downloads/ml-latest-small"
USER_MOVIE_FILE = d + "/user_movie.txt"
MOVIE_USER_FILE = d + "/movie_user.txt"

n_users, n_movies, global_mu = 611, 9742, 3.5
#n_users, n_movies, mu = 200948, 87584, 3.54

rng = np.random.default_rng(42)

U = 0.1 * rng.standard_normal((n_users, K))
V = 0.1 * rng.standard_normal((n_movies, K))
b = np.zeros(n_users)
c = np.zeros(n_movies)

U_mean = np.zeros_like(U)
V_mean = np.zeros_like(V)
b_mean = np.zeros_like(b)
c_mean = np.zeros_like(c)
n_kept = 0

eyeK = np.eye(K)

for it in range(1, N_ITERS + 1):
    print ('iteration', it)

    with open(USER_MOVIE_FILE, newline="") as csvfile:
        rd = csv.reader(csvfile, delimiter="|")
        for row in rd:
            i = int(row[0])
            ratings = json.loads(row[1])
            tsum = np.zeros((K,K))
            for j,rating in ratings.items(): tsum += np.dot(V[int(j)].reshape(K,1), \
                                                            V[int(j)].reshape(1,K) )
            
            invsigma = lambda_U*eyeK + (1.0 / sigma2) * tsum
            sigma = np.linalg.inv(invsigma)
            mutmp = np.array([ V[int(j)]*(rating-global_mu) for j,rating in ratings.items()]).sum(axis=0)
            mu = np.dot(sigma, 1.0/sigma2 * mutmp)
            U[i, :] = rng.multivariate_normal(mu, sigma)

            
    with open(MOVIE_USER_FILE, newline="") as csvfile:
        rd = csv.reader(csvfile, delimiter="|")
        for row in rd:
            j = int(row[0])
            ratings = json.loads(row[1])
            tsum = np.zeros((K,K))            
            for i,rating in ratings.items(): tsum += np.dot(U[int(i)].reshape(K,1), \
                                                            U[int(i)].reshape(1,K) )
            
            invsigma = lambda_V*eyeK + (1.0 / sigma2) * tsum
            sigma = np.linalg.inv(invsigma)
            mutmp = np.array([ U[int(i)]*(rating-global_mu) for i,rating in ratings.items()]).sum(axis=0)
            mu = np.dot(sigma, 1.0/sigma2 * mutmp)
            V[j, :] = rng.multivariate_normal(mu, sigma)

            
    if it >= BURN_IN and it % THIN == 0:
        n_kept += 1
        alpha = 1.0 / n_kept
        U_mean += alpha * (U - U_mean)
        V_mean += alpha * (V - V_mean)        

np.savez(
    "bpmf_posterior.npz",
    U=U_mean,
    V=V_mean,
    mu=mu,
    user_ids=np.arange(n_users),
    movie_ids=np.arange(n_movies),
)

print("Saved bpmf_posterior.npz")
```


[devam edecek]

Kodlar

[par_bpmf.py](par_bpmf.py),
[prep1.py](prep1.py),
[prep2.py](prep2.py),
[prep3.py](prep3.py),
[prep4.py](prep4.py),
[recom.py](recom.py),
[rmse_mypicks.py](rmse_mypicks.py)

Kaynaklar

[1] Netflix, <a href="https://grouplens.org/datasets/movielens/latest/">MovieLens Small (ml-latest-small)</a>

[2] Netflix, <a href="https://grouplens.org/datasets/movielens/32m/">MovieLens 32M, (ml-32m)</a>

[3] Salakhudtinov,  <a href="https://www.cs.toronto.edu/~amnih/papers/bpmf.pdf">Bayesian Probabilistic Matrix Factorization using Markov Chain Monte Carlo</a>

[4] Anton Gerber Sort, <a href="https://research-api.cbs.dk/ws/portalfiles/portal/98731723/1641765_Thesis_Anton_Sort.pdf">Probabilistic Matrix Factorisation in Collaborative Filtering, Thesis</a>

