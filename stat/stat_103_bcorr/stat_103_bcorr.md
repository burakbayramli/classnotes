# Korelasyon, P-Değerleri, Bayes Yaklaşımı

Bir korelasyon katsayı hesabının p-değerini, yani onun istatistiki
olarak ne kadar anlamlı (significant) olduğunu hesaplamak mümkündür.

### Frekansçı Yaklaşım

Korelasyon $\rho$'yu daha önce gördük, tahmin edicisi $r$'dir, 

$$ 
\hat{\rho} = r = \frac{S_{xy}}{\sqrt{S_{xx}S_{yy}}} 
\tag{1} 
$$

ki örneklem hesapları $S_{xx},S_{xy},S_{yy}$ 

$$ S_{xx} = \sum_{i=1}^{n} (x_i-\bar{x})^2 $$

$$ S_{xy} = \sum_{i=1}^{n} (x_i-\bar{x})(y_i-\bar{y}) $$

$$ S_{yy} = \sum_{i=1}^{n} (y_i-\bar{y})^2 $$

olsun; bu hesapların teorik varyans ile olan bağlantısı görülebilir. Eğer
$X,Y$ iki değişkenli (bivariate) bir normal dağılımından geliyorsa, o zaman
ortada bir regresyon varmış gibi gösterebiliriz

$$ E(Y|X=x) = \beta_0 + \beta_1 x + \epsilon $$

ki $\beta_1 = \sigma_Y / \sigma_X \cdot \rho$ olur. Detaylar için
[1]. Soru şu, $r$ için bir istatistiksel önemlilik (significance)
hesabı nasıl yapardık? Yani, eğer $-1 \le r \le 1$ işe, ve $r=0$ hiç
korelasyon olmama durumu ise, acaba bu "sıfır olmama'' durumunu test
edebilir miydim? Evet. Yukarıdaki normallik faraziyesi doğru ise
$\beta_1 = 0$ olmama durumunu test etmek $\rho = 0$ olmama testi ile
aynı, bu durumda

$$ t_0 = \frac{\hat{\beta_1}}{\sqrt{ Var(\hat{\beta_1} ) }} $$

gibi bir test istatistiği yaratırız, ki bu istatistik Öğrenci t dağılımına
sahip olurdu çünkü sıfır hipotezi $\hat{\beta_1} = 0$, ve üstteki
istatistik sıfır hipotezi altında ile Öğrenci t dağılımına sahip olmak
zorundadır, çünkü bölünen normal dağılmış, bölen chi karenin karekökü
olarak dağılmış. Eğer $t_o$ hesabı veriye uygulandıktan sonra hipotezin
öngördüğü dağılıma uymaz ise, sıfır hipotezini reddederiz. 

Bu noktada lineer regresyon ile alakalı bilgiler devreye sokulabilir,
[1]'den biliyoruz ki,

$$ Var(\hat{\beta_1}) = \frac{\sigma^2}{S_{xx}} $$

$\sigma$ yerine örneklemden gelen $S$ kullanırsak ve üstteki formüle koyarsak,

$$ t_0 = \frac{\hat{\beta_1}}{\sqrt{S / S_{xx}}} $$

Bu ifadeyi $r$ bazında ifade edebilir miyiz?  Deneyelim, $\hat{\beta_1} =
S_{xy} / S_{xx}$ ve $r = \hat{\beta_1}\sqrt{\frac{S_{xx}}{S_{yy}}}$ olduğunu
biliyoruz [1], ayrıca

$$ S = \frac{SSE}{n-2}, \qquad SSE = S_{yy} - \hat{\beta_1} S_{xy} $$

ki $SSE$ hata karelerinin toplamıdır (sum of squared errors),

$$ t_0 = \frac{  \sqrt{S_{xx}} \hat{\beta_1} \sqrt{n-2} }{\sqrt{SSE}} $$

$$ = \frac{  \sqrt{S_{xx}} \hat{\beta_1} \sqrt{n-2} }{\sqrt{S_{yy} - \hat{\beta_1} S_{xy}}} $$

Bölümün iki kısmını $\sqrt{S_y}$ ile bölelim, 

$$ = 
\frac{ \sqrt{S_{xx}/S_{yy}} \hat{\beta_1} \sqrt{n-2} }
{\sqrt{1 - \hat{\beta_1} S_{xy}/S_{yy}}} 
$$

Bölünen kısmında bir $r$ ortaya çıktı,

$$ = 
\frac{ r\sqrt{n-2} }
{\sqrt{1 - \hat{\beta_1} S_{xy}/S_{yy}}} 
$$

Bölen kısmındaki $\hat{\beta_1}$ yerine $\hat{\beta_1} = S_{xy} / S_{xx}$
koyarsak yine (1)'deki $r$ tanımına geliriz, ve alttaki basitleştirilmiş
ifade ortaya çıkar,

$$ t_o = \sqrt{\frac{(n-2)r^2}{(1-r^2)}} $$

Bu istatistik $n-2$ derece serbestliğe sahip bir Öğrenci t dağılımıdır. 

Örnek

Possum adı verilen bir tür hayvanın dişilerinin tüm uzunluğu ve kafa ölçümü
`totlngth,hdlngth` değişkenleri arasında korelasyon olup olmadığı
merak edilmektedir.

```python
import pandas as pd
import scipy.stats

def p_corr(df1, df2):
    corr = df1.corr(df2)
    N = np.sum(df1.notnull())
    t = corr*np.sqrt((N-2)/(1-corr**2))
    p = 1-scipy.stats.t.cdf(abs(t),N-2)  # one-tailed
    return corr, t, p

df = pd.read_csv('fossum.csv')
c,tval, pval = p_corr(df.totlngth,df.hdlngth)
print (c, pval)
```

```
0.779239322172 3.75045772216e-10
```

p-değeri çok küçük, demek ki korelason olmadığı tezi reddedildi. Korelasyon
var.

### Bayes Usulü Korelasyon Hesabı

Frekansçı yaklaşımla korelasyon hesabı yaptığımızda verinin dağılımı
hakkında bir faraziye yapmığımızı söylemiştik, bu faraziye dağılımın
iki değişkenli (bivariate) Gaussian olduğu idi. O zaman Bayes
yaklaşımı ile korelasyon tahmini yapmak için bu başlangıç noktasını
kullanabiliriz. 

Önce Gaussian dağılımının formülünü hatırlayalım, genel $d$ boyutlu
Gaussian dağılımı şöyledir,

$$ f(x | \mu,\Sigma) = \frac{ 1}{(2\pi)^{k/2} \det(\Sigma)^{1/2}} \exp
\bigg\{ -\frac{ 1}{2}(x-\mu)^T\Sigma^{-1}(x-\mu) \bigg\} $$

Bizim için iki değişkenli Gaussian lazım, yani $k=2$ olacak, o zaman
bir veri noktası $x_i$'nin olasılığı,

$$
f(x_i \mid \theta) = \frac{1}{2\pi
\sqrt{\det(\Sigma)}} \exp \left(
-\frac{1}{2} (x_i - \mu)^T \Sigma^{-1} (x_i - \mu) \right)
\tag{2}
$$

ki

$$ \mu = \begin{pmatrix} \mu_1 \\ \mu_2 \end{pmatrix}, \quad \Sigma =
\begin{pmatrix} \sigma_1^2 & \sigma_{12} \\ \sigma_{21} &
\sigma_2^2 \end{pmatrix} $$

$\theta = (\mu_1, \mu_2, \sigma_1, \sigma_2, \sigma_{12},
\sigma_{21})^T$ şeklinde de tanımlanabilirdi. Bu dağılımı bir
korelasyon hesabı $\rho$ için nasıl kullanabiliriz?  Düşünelim,
korelasyon katsayısı nedir? Bu katsayı eğer pür dağılım üzerinden
düşünürsek $\rho = \frac{\sigma_{12}}{\sigma_1 \sigma_2}$ değil midir?
Evet, o zaman üstteki dağılımdaki varyans ile ilgili değişkenleri
saptayabilirsek bize gereken sonucu elde etmiş olacağız. Bu
değişkenleri nasıl buluruz? Bayes yaklaşımında $\theta$ içeriği
rasgele değişken olarak kabul edilir, onların bir dağılımı
vardır. Bayes terminolojisinde (yine rasgele değişken kabul edilen)
veri ile koşullandırılmış (conditioned) $\theta$ olasılık (sonsal)
yoğunluğu fonksiyonunu bulmamız gerekiyor.

İşin cebirsel / mekaniksel / hesapsal kısmına gelirsek, bazen bu
sonsal (posterior) dağılımın ne olduğunu analitik olarak bulunabilir,
eğer bu mümkün değil ise sayısal yöntemlerle sonsaldan örneklem
toplamak mümkündür. Demek ki $\sigma_1, \sigma_2, \sigma_{12},
\sigma_{21}$ rasgele değişkenlerinin dağılımından örneklem
toplayabilirsek bu simülasyon sonuçlarından $\rho$ hesaplanabilir.

Daha ilerlemeden bir numara daha tanıştıralım. Birazdan göreceğimiz
Metropolis yönteminde $\theta$'daki değişkenlerden örneklem topluyor
olacağız, fakat bu sırada $\sigma_{12}$, $\sigma_{21}$ değişkenlerinin
$\sigma_1, \sigma_2$ değişkenlerinden alakasız yönlere evrilmesi
sayısal problem çıkartabilir. Ayrıca hesaplamaya uğraştığımız nihai
değişken $\rho$'yu direk simülasyon içine dahil edebilirsek bizim için
daha iyi olacak. O zaman

$$\rho = \frac{\sigma_{12}}{\sigma_1 \sigma_2}$$

olduğundan hareketle $\rho \sigma_1 \sigma_2 = \sigma_{12}$ ifadesi de
doğrudur. Ayrıca $\sigma_{12} = \sigma_{21}$ da olduğu için $\rho
\sigma_1 \sigma_2 = \sigma_{21}$ da diyebiliriz. Bu iki eşitliği
Gaussian formülünde $\sigma_{12}$ ve $\sigma_{21}$ yerine kullanırsak
simülasyon kodu daha basitleşecek. O zaman iki değişkenli
Gaussian'ımız için $\mu$ ve $\Sigma$ şu hale gelecek,

$$
\mu = \begin{pmatrix} \mu_1 \\ \mu_2
\end{pmatrix}, \quad \Sigma =
\begin{pmatrix} \sigma_1^2 & \rho \sigma_1 \sigma_2 \\ \rho \sigma_1
\sigma_2 & \sigma_2^2 \end{pmatrix}
\tag{3}
$$

O zaman ilgilendiğimiz parametreler $\theta = (\mu_1, \mu_2, \sigma_1,
\sigma_2, \rho)^T$ olacak. Veri $D = \{x_1, x_2, \dots, x_N\}$ ise, ki
her veri noktası iki değişkenli $x_i = (x_{i,1}, x_{i,2})^T$ olur,
Bayes Teorisi şöyle der,

$$
p(\theta \mid D) = \frac{p(D \mid \theta) \, p(\theta)}{p(D)}
\propto p(D \mid \theta) \, p(\theta)
$$

Her iki tarafın doğal logiratmasını alalım, bu işlem çarpımları
toplama dönüştürür,

$$
\ln p(\theta \mid D) = \ln p(D \mid \theta) + \ln p(\theta) +
\text{sabit}
$$

Demek ki üstteki hesabı yapabilmek için eşitliğin sağındaki verinin log
olurluğu (log likelihood) bize lazım, tüm veri için

$$
L(\theta \mid D) = \prod_{i=1}^N f(x_i
\mid \theta)
$$

$$
\ln L(\theta \mid D) = \sum_{i=1}^N \ln f(x_i \mid \theta)
\tag{4}
$$

Eriştiğimiz son ifade için (2) formülündeki determinantlar, onun
tersi, formülde görülen karesel ifadenin açılımı ve tüm bunların
log'unun alınması gerekecek. (3) üzerinde determinant alalım,

$$
\det(\Sigma) = (\sigma_1^2)(\sigma_2^2)
- (\rho \sigma_1 \sigma_2)^2
= \sigma_1^2 \sigma_2^2 (1 - \rho^2)
$$

Karekök alınca

$$
\sqrt{\det(\Sigma)} = \sigma_1 \sigma_2
\sqrt{1 - \rho^2}
$$

Kovaryans matrisinin tersi

$$
\Sigma^{-1} = \frac{1}{\sigma_1^2
\sigma_2^2 (1 - \rho^2)}
\begin{pmatrix} \sigma_2^2 & -\rho \sigma_1 \sigma_2 \\ -\rho
\sigma_1
\sigma_2 & \sigma_1^2 \end{pmatrix} = \frac{1}{1 - \rho^2}
\begin{pmatrix} \frac{1}{\sigma_1^2} & -\frac{\rho}{\sigma_1
\sigma_2}
\\ -\frac{\rho}{\sigma_1 \sigma_2} & \frac{1}{\sigma_2^2}
\end{pmatrix}
$$

$z_{i,1} = \frac{x_{i,1} - \mu_1}{\sigma_1}$ ve $z_{i,2} =
\frac{x_{i,2} - \mu_2}{\sigma_2}$ olsun (bir tür "standardizasyon"
işlemi yapıyoruz). Üstel içindeki terimler şu hale gelir,

$$(x_i - \mu)^T \Sigma^{-1} (x_i - \mu) = \begin{pmatrix} x_{i,1} -
\mu_1 & x_{i,2} - \mu_2 \end{pmatrix} \left[ \frac{1}{1 - \rho^2}
\begin{pmatrix} \frac{1}{\sigma_1^2} & -\frac{\rho}{\sigma_1 \sigma_2}
\\ -\frac{\rho}{\sigma_1 \sigma_2} & \frac{1}{\sigma_2^2}
\end{pmatrix} \right] \begin{pmatrix} x_{i,1} - \mu_1 \\ x_{i,2} -
\mu_2 \end{pmatrix}$$

$$
= \frac{1}{1 - \rho^2} \left[ \left(\frac{x_{i,1} -
\mu_1}{\sigma_1}\right)^2 - 2\rho \left(\frac{x_{i,1} -
\mu_1}{\sigma_1}\right)\left(\frac{x_{i,2} - \mu_2}{\sigma_2}\right) +
\left(\frac{x_{i,2} - \mu_2}{\sigma_2}\right)^2 \right]
$$

$$
= \frac{1}{1 - \rho^2} \left( z_{i,1}^2 - 2\rho z_{i,1} z_{i,2} +
z_{i,2}^2 \right)
$$

O zaman (4) hesabı şu şekilde yapılabilir,

$$\ln L(\theta \mid D) = \sum_{i=1}^N \left[ -\ln(2\pi) -
\ln\left(\sigma_1 \sigma_2 \sqrt{1 - \rho^2}\right) - \frac{1}{2(1 -
\rho^2)} \left( z_{i,1}^2 - 2\rho z_{i,1} z_{i,2} + z_{i,2}^2 \right)
\right]$$

$$
\ln L(\theta \mid D) = -N \ln\left(2\pi \sigma_1 \sigma_2 \sqrt{1 -
\rho^2}\right) - \frac{1}{2(1 - \rho^2)} \sum_{i=1}^N \left( z_{i,1}^2
- 2\rho z_{i,1} z_{i,2} + z_{i,2}^2 \right)
\tag{5}
$$

Kodlar

Alttaki kod parçasında gördüğümüz formülleri kodlayacağız. İşlenecek
veri ABD altın ve reel faiz seviyeleri. Finans verileri çoğunlukla
Gaussian faraziyesine uymaz, durağan değildirler, ortalamaları ve
standart sapmaları zaman geçtikçe değişim gösterir. Durağanlığı geri
getirmek için veri üzerinde fark alma işlemi yapabiliriz, yani serinin
kendisi yerine iki öğe arasındaki farkından yaratılmış yeni bir seri
ile işlem yaparız. Bu niye işliyor? Çoğu finans fiyat seviyeleri
rasgele yürüyüş (random walk) olarak davranır, bugünün fiyat seviyesi
dünün seviyesi artı gürültüdür,

$$
Y_t = Y_{t-1} + \epsilon_t
$$

Fark alma işlemi yapınca

$$
\Delta Y_t = Y_t - Y_{t-1} = \epsilon_t
$$

$\Delta Y_t$ serisi durağan hale geldi, sadece gürültüye bağlı.


```python
import pandas as pd
import scipy.stats as st

# 1. Kurulum ve veri temizleme
np.random.seed(42)

df = pd.read_csv('../stat_102_regchpt/gold_ir.csv', index_col=0)
df_clean = df.dropna().copy()

raw_gold = df_clean.Gold_Price
raw_rate = df_clean.Calculated_Real_IR

# Durağanlık (stabilite) için farkını alma
diff_gold = np.diff(raw_gold)
diff_rate = np.diff(raw_rate)
data = np.column_stack((diff_gold, diff_rate))
N = len(data)

# --- FREKANSÇI (FREQUENTIST) YAKLAŞIM ---
r_freq, p_freq = st.pearsonr(diff_gold, diff_rate)


# --- MODÜLER BAYESÇİ KURULUM (Regresyon Tarzı) ---

def log_prior(params):
    """
    Parametreler için kesin sınırlar belirler.
    Önerilen parametreler fiziksel olarak mümkünse 0.0 (ln(1)) döndürür,
    kurallarımızı ihlal ediyorsa -inf (ln(0)) döndürür.
    """
    mu1, mu2, sigma1, sigma2, rho = params
    
    # Standart sapmalar pozitif olmalı, korelasyon [-1, 1] aralığında olmalı
    if sigma1 <= 0 or sigma2 <= 0 or not (-1 < rho < 1):
        return -np.inf
        
    return 0.0


def log_likelihood(params, data):
    """
    İki değişkenli Normal Olurluk'u hesaplar. Önsel kontrolü zaten
    çalıştığı için parametrelerin geçerli olduğunu varsayar.
    """
    mu1, mu2, sigma1, sigma2, rho = params
    
    # Standart skorları hesapla
    z1 = (data[:, 0] - mu1) / sigma1
    z2 = (data[:, 1] - mu2) / sigma2
    
    # İki değişkenli normal log-yoğunluk fonksiyonu toplamı
    term1 = -N * np.log(2 * np.pi * sigma1 * sigma2 * np.sqrt(1 - rho**2))
    term2 = -1 / (2 * (1 - rho**2)) * np.sum(z1**2 - 2 * rho * z1 * z2 + z2**2)
    
    return term1 + term2


def log_posterior(params, data):
    """
    Geçersiz matematiksel durumlara karşı korumak amacıyla kısa devre
    değerlendirmesi kullanarak Önsel ve Olurluk'u birleştirir.
    """
    lp = log_prior(params)
    
    # Eğer önsel "hayır" diyorsa, sıfıra bölme veya NaN durumlarını
      önlemek için hemen dur    
    if np.isneginf(lp):
        return -np.inf
        
    return lp + log_likelihood(params, data)


# --- MCMC METROPOLIS ÖRNEKLEYİCİ ---

n_samples = 50000
burn_in = 10000

# Parametre vektörü formatı: [mu1, mu2, sigma1, sigma2, rho]
initial_params = [np.mean(diff_gold), np.mean(diff_rate), np.std(diff_gold), np.std(diff_rate), 0.0]

proposal_sigmas = np.array([0.001, 0.001, 0.001, 0.001, 0.005])

# İz (trace) depolama
trace = np.zeros((n_samples, 5))
trace[0] = initial_params
current_log_post = log_posterior(trace[0], data)

accepted = 0

for i in range(1, n_samples):
    # Gauss rastgele yürüyüşü kullanarak yeni parametreler öner
    proposal = trace[i-1] + np.random.normal(0, proposal_sigmas)
    
    # Önerilen log-sonsal değerini hesapla
    proposal_log_post = log_posterior(proposal, data)
    
    # Metropolis kabul kriteri
    log_acceptance_ratio = proposal_log_post - current_log_post
    
    if np.log(np.random.uniform(0, 1)) < log_acceptance_ratio:
        trace[i] = proposal
        current_log_post = proposal_log_post
        accepted += 1
    else:
        trace[i] = trace[i-1]

# Yanma süresi (burn-in) sonrası korelasyon katsayısı için örneklemler (rho 4. indekste)
rho_samples = trace[burn_in:, 4]
acceptance_rate = accepted / n_samples

# Bayesçi "p-değeri" karşılığı (Ters işaret olasılığı)
mean_rho = np.mean(rho_samples)
if mean_rho > 0:
    p_bayes = np.mean(rho_samples <= 0)
else:
    p_bayes = np.mean(rho_samples >= 0)

# %95 Güvenilir Aralığı (Trace'ten yüzdelik yöntemiyle) hesapla
hdi = np.percentile(rho_samples, [2.5, 97.5])

# Frekansçı için Güven Aralığı (Fisher Dönüşümü ile)
r_z = np.arctanh(r_freq)
se = 1 / np.sqrt(N - 3)
z_crit = st.norm.ppf(0.975)
ci_freq = np.tanh([r_z - z_crit * se, r_z + z_crit * se])


# --- SONUÇLARI YAN YANA GÖSTER ---
print("=" * 60)
print(f"MCMC Raporu: Kabul Oranı = {acceptance_rate:.2%}")
print("=" * 60)
print(f"{'Metrik':<25} | {'Frekansçı (Pearson)':<20} | {'Bayesçi MCMC (El Yapımı)':<20}")
print("-" * 60)
print(f"{'Korelasyon (r / rho)':<25} | {r_freq:>20.4f} | {mean_rho:>20.4f}")
print(f"{'p-değeri (işaret olasılık)':<25} | {p_freq:>20.4e} | {p_bayes:>20.4f}")
print(f"{'%95 Aralık Alt Sınır':<25} | {ci_freq[0]:>20.4f} | {hdi[0]:>20.4f}")
print(f"{'%95 Aralık Üst Sınır':<25} | {ci_freq[1]:>20.4f} | {hdi[1]:>20.4f}")
print("=" * 60)
```

```text
============================================================
MCMC Raporu: Kabul Oranı = 57.14%
============================================================
Metrik                     | Frekansçı (Pearson) | Bayes MCMC
------------------------------------------------------------
Korelasyon (r / rho)       |             -0.0042 |   -0.0045
p-değeri (işaret olasılık) |          7.9035e-01 |    0.3901
%95 Aralık Alt Sınır       |             -0.0353 |   -0.0353
%95 Aralık Üst Sınır       |              0.0269 |    0.0251
============================================================
```

Kodda görülen `term1` ve `term2` hesapları (5) formülündeki birinci ve
ikinci terime tekabül ediyor.

Fonksiyon `log_prior` içinde simülasyonun istenen sınırlar içinde
kalmasının nasıl kontrol edildiğini görüyoruz. Önsel tanımımız içinde
birörnek dağılımlar vardı hatırlarsak, ve bu dağılımlar belli
aralıklar içinde kalmalıdır. `sigma1`, `sigma2`, `rho` eğer istenmeyen
değerlere gelirlerse fonksiyondan `np.inf` yani sonsuz değer
döndürüyoruz (oradaki olasılığı sıfırlamış oluyoruz, log olasılıkta
sıfırın log değeri sonsuz). Metropolis algoritması muhtemel değerleri
gezerken sınır dışı değerler için sıfır olasılık alınca o bölgelerden
uzak durmak isteyecektir.

Sonuçlara bakalım: Korelasyon katsayısının frekansçı ve Bayes ile
birbirine yakın olduğunu olduğunu görüyoruz. P-değeri hesabı birinde
0.79 diğerinde 0.39 çıktı (ikinci hesap tek taraflı test, ikiyle
çarpınca birinci değere yaklaşırız), 0.05 altında olmadıkları için,
bulunan $\rho$'nun istatistiki olarak önemli (significant) olmadığı
sonucuna varabiliriz.

Bayes yaklaşımında elde edilen sonuç sonsal dağılımdır demiştik, daha
detaylandırmak gerekirse sonsal dağılımdan toplanmış
örneklemlerdir. Bu örneklem vektörü üzerinde sayısal güven aralıkları
hesaplanabilir, ve frekansçı dünyadaki güven aralıklarının (confidence
interval) Bayes dünyasındaki karşılığı budur, Bayes literatürü onlara
"inandırıcı aralık (credible interval)" ismini veriyor, Bayes güven
aralığı olarak ta tanımlanabilirler.

Bayes p-değerini hesaplamak için elde edilen sonucun (bu örnekte
$\rho$) işaretine bakılır, ve "sonsal örneklem içinde kaç tane değer
bu işaretin tersi işaretine sahiptir" sorusu sorulur, cevap bir oran
olarak raporlanır. Burada yapılmaya uğraşılan kritik eşik olan sıfırı
merkez alan bir özet hesabı yaratmak. Bir sonuç elde ettik, şunu
soruyoruz, "acaba ters işaretli olan sonuçların yüzde kaçı sıfırın
diğer tarafında?"

Bayes güven aralığı şöyle kullanılabilir; Üstte görülen sonuçlarda
aralık -0.0353 ve 0.0251 olarak hesaplanmış. Sıfır değeri bu aralığın
içinde (hatta neredeyse tam ortasında) olduğu için korelasyon olmadığı
sonucu daha kuvvetlenmiş demektir, ayrıca güven aralığının ne kadar
sıkı (tight) olduğuna da bakabiliriz. Gördüğümüz raporda aralık uçları
birbirine oldukca yakın, demek ki aralık dar, ve demek ki sıfırın bu
aralıkta olması çok çok muhtemel, böylece korelasyon olmadığı tezi
daha da kuvvetlenmiş oluyor.

Student-t Bazlı Getiriler

Finansal getiriler Gaussian dağılımı faraziyesine uymayabilirler,
araştırmalara göre bu tür verileri modellemek için Student-t dağılımı
daha uygundur, çünkü finansal şok olayları, yani ekstrem değerler bir
Gaussian dağılımının izin vereceğinden daha yaygındır. Bayes yaklaşımı
sayesinde bu yeni dağılımı kullanmak pek zor olmayacak.

Alternatif bir MCMC kodlaması şöyle olabilir, zaman serisi $x_t$ ve
$y_t$'yi iki boyutlu Student-t dağılımı üzerinden analiz ederiz, ki bu
dağılımın $\nu$ serbestlik derecesi, ortalama vektörü $\mu = [\mu_x,
\mu_y]^T$, ve ölçeklemesi $\Sigma$ olacaktır (üstteki Gaussian
örneğinde olduğu gibi). N bağımsız örneklem $D = \{(x_1, y_1), \dots,
(x_N, y_N)\}$ için birleşik olasılık yoğunluğu:

$$
p(D \mid \mu, \Sigma, \nu) = \prod_{i=1}^N
\frac{\Gamma(\frac{\nu+2}{2})}{\Gamma(\frac{\nu}{2})\pi\nu
|\Sigma|^{1/2}} \left(1 + \frac{1}{\nu} (d_i - \mu)^T \Sigma^{-1} (d_i
- \mu)\right)^{-\frac{\nu+2}{2}}
$$

ki $d_i = [x_i, y_i]^T$ olacak şekilde. Yine $\rho$ hesaplanacaktır,
$\nu=3$'yu sabitleyebiliriz ("kalın etekleri" garantilemek için),
ve Metropolis yöntemi ile $\theta = (\mu_x, \mu_y, \sigma_x, \sigma_y, \rho)$
değişkenlerinden örneklem toplarız. Bu kodlar `corr_studentt.py` içinde bulunabilir.

Kodlar

[corr_studentt.py](corr_studentt.py)

Kaynaklar

[1] Wackerly, *Mathematical Statistics, 7th Edition*
