# Coğrafi İstatistiki Hesaplar, Oranlar

### Savaş Sırasında Londra

Daha önce istatistik testler konusunda gördük, bir dağılım
varlığlığını test için o dağılımın analitik yoğunluk fonksiyonunu
veriden gelen tahmin ediciler üzerinden tanımlayıp, veriyi bu
fonksiyon ile üretmeyi deneyebiliriz, ve bu sonuç ile veri arasında
uyumluluğa bakabiliriz.

Mesela olayların coğrafi olarak dağılımına bakalım..  Bu tür olayları
nasıl modelleriz? Olaylar depremler, yangınlar, ya da bir savaşta bir
alana atılan bombalar olabilir, ve bu tür sayılar Poisson dağılımı ile
modellenir. Bu dağılım ilk bölümde gördüğümüz gibi,

$$ f(x) = P(X=x) = e^{-\lambda}\frac{\lambda^{x}}{x!} $$

olay sayısı $x=1$, $x=2$, vs.. olacak şekilde, ki önceden tanımlı
belli bir zaman aralığında $x$ tane olayın olma olasılığını bu
yoğunluk veriyor. Coğrafi olay sayılarını ölçmek için biraz farklı
düşünmek gerekiyor, mesela 2'inci Dünya Savaşı sırasında Almanların
Londra'ya attıkları bombaları düşünelim, analizi [2]'de var; Merak
edilen şuydu, acaba bombalar belli bir yerde kümeleniyor muydu
(clustering)? Cevap önemli olabilirdi, belki özel bir yer vurulmak
isteniyordu? Analizde olayların doğal oluş sayısını modelleyen Poisson
varlığı ispatlanırsa, kümelenme hipotezi reddedilmiş
olacaktı. İstatistikçi Clarke Londra'yı 536 tane ızgaraya böldü, ve
her öğe içine düşen bombaları saydı. Bu bittikten sonra 1 tane bomba,
2 tane bomba, vs.. şeklinde olan hücrelerin sayısını aldı, ki
yoğunluğa $x$ ile geçilecek olan bu sayıydı.

Sonra Clarke yoğunluğu $\lambda$ tahmin edici hücre sayısı bölü bomba
sayısı üzerinden tanımladı, ve bu yoğunluktan tüm sayılar için bir
tahmini bomba sayısı ürettirdi, sonuçları gerçek bomba sayıları ile
karşılaştırdı.

```python
N = 576.
lam = 537/N
d = N*np.exp(-lam)
probs = [d*1, d*lam, d*lam**2/2, d*(lam**3)/(3*2), d*(lam**4)/(4*3*2)]
list(map(lambda x: float(np.round(x,2)), probs))
```

```text
Out[1]: [226.74, 211.39, 98.54, 30.62, 7.14]
```

Gerçek sayılar 229, 211, 93, 35, 7, .. idi, görüldüğü gibi oldukca
yakın sayılar. Bir adım daha atılıp bunun üzerinde bir istatistik
testi uygulanınca Poisson varlığı, ve dolaylı olarak kümelemenin
olmadığı ispatlanmış oldu.

### Zaman Serileri, Sayım Verisi

Üstteki örnekte tek bir Poisson dağılımı modellendi. Peki ya zamana
yayılmış (sene bazında), ve bir A grubu ile bir diğer B grubunun sayım
verisini karşılaştırmak isteseydik? Problem şu şekilde ortaya
çıkabilir:

Ülke bazında spesifik bir olaya bağlı bir sayım verisine bakıyoruz, bu
veri sene bazında toplanıyor. Bir veri mesela yıllık "dört yol
kavşaklarında olan kaza sayısı" olabilir. Kavşaklar tabii ki ülkenin
her tarafında, hepsine bakıp her sene için orada olan kazaları
topluyoruz. Sonra diyelim ki bu kazaların normal (!)  kazalardan daha
yüksek / farklı olup olmadığını merak ediyoruz, o zaman diğer bir
ölçüm sene bazlı dört yol kavşakları *dışındaki* kazalar olur.

O zaman elimizde iki zaman serisi olacak, her sene için iki tane
ölçüm. Her ölçüm rakamının, bir sayım olduğu için, Poisson
dağılımından geldiğini kabul edebiliriz. Fakat dikkat, her sene *aynı*
Poisson dağılımından mı geliyor? Büyük ihtimalle hayır çünkü kaza
sayılarında sene bazlı değişim olabilir: araç sayıları farklı
olabilir, yol şartları değişmiş olabilir. Karşılaştırma mekanizmasının
bunu hesaba katması gerekir.

Bir diğer problem ölçekleme (scaling) problemi olabilir, kavşaklar
yolların ufak bir alanını temsil eder, kıyasla yolların tamamı
fiziksel olarak daha fazla yer kaplar bu sebeple kavşak olmayan yol
bölümünde olan kazaların sayıca daha fazla olması muhtemeldir. Bu
fazlalık karşılaştırmayı yanıltabilir, elmalar ve armutları
karşılaştırmış oluruz. Eğer elmalar ile elmaları karşılaştırmak
istiyorsak kavşak dışındaki sayımları ölçekleyip diğer ölçüme
skalasına yaklaştırmamız gerekir. Bu çok zor olmasa gerek, basit bir
toplam ve bölme işlemi ile bunu başarabiliriz.

Şimdi üzerinde karşılaştırma yapmak için sentetik veri üretelim. İlk
veri birbirine yakın iki zaman serisi gösteriyor.

```python
import pandas as pd
import pymc as pm
import arviz as az
import data

years = np.arange(1950, 2011)

sim = data.generate_synthetic(years=years,
                              alpha_log=3.2,
                              beta_log=0,
                              sigma_year=0.4,
                              obs_model="poisson",
                              trend=data.slow_trend)

df = pd.DataFrame({
    "year": sim["years"],
    "A": sim["A"],
    "B": sim["B"]
})

SEED = 333
np.random.seed(SEED)

# quick plot of generated counts
plt.figure(figsize=(10,4))
plt.plot(df.year, df.A, label="kavsak", marker="o")
plt.plot(df.year, df.B, label="diger", marker="x")
plt.xlabel("Sene"); plt.ylabel("Sayim")
plt.title("Sentetik sayim (Poisson)")
plt.legend()
plt.tight_layout()
plt.savefig('stat_082_rapoi_01.jpg')
```

![](stat_082_rapoi_01.jpg)

Verinin modeli ne olacak? Her sene farklı bir Poisson dağılımı olsun
demiştik, ama bu dağılımların birbirine bazı yönlerden benzerlikleri
de olmalı. Bir Poisson GLM (genel lineer model -generalized linear
model-) yaratabiliriz, her iki zaman serisindeki her sene ölçümü
farklı bir $\lambda$ ile üretiliyor olabilir, parametrelerde kavşak
için A diğeri için B dersek,

$$
\lambda_{A,t} = e^{\alpha + \beta + u_t}
$$

$$
\lambda_{B,t} = e^{\alpha + u_t}
$$

Üstteki parametreleri kullanarak $y$ verisinin şu şekilde üretildiğini
farz edebiliriz,

$$
y_{A,t} \sim Poisson(\lambda_{A,t})
$$

$$
y_{B,t} \sim Poisson(\lambda_{B,t})
$$

Denklemlerde $\beta$ dolaylı olarak $\lambda$ oranlarını hesaplayacak,
dikkat edelim bir formülde var, diğerinde yok. Bunun sebebini birazdan
göreceğiz. Peki niye üstel $e$ kullanımı var? Eğer $\log$ alsaydık
mesela ilk denklem için

$$
\log(\lambda_{A,t}) = \alpha + \beta + u_t
$$

elde ediyoruz, bu standart lineer regresyondan tanıdık bir format. Log
bağlantının sebebi (ya da dolaylı $e$ kullanımı)

$$
\lambda_{A,t} = e^{\alpha} \cdot e^{\beta} \cdot e^{u_t}
$$

yapısına izin vermek / onu aktıve etmek. Böylece $\lambda$'nin pozitif
olmasını sağlıyoruz (çünkü Poisson oran $\lambda$ pozitif olmalıdır),
ayrıca oran hesapları çarpımsal parametreler içerdiği için bu
varsayımı modele dahil etmiş oluyoruz. Tıpta bir ilaç uygulaması
hastalık oranını katlayarak etkiler, hava kirliliği astim hastalığı
oranını katlayarak arttırır, vb. Bu tür faraziyeler üstel hesap
üzerinden formülasyona dahil edilmiş oldu.

Peki aradığımız oran $\beta$ nasıl hesaplanacak? Bu hesap aslında
*dolaylı* bir hesap. Biraz cebir ile $\beta$'nin neye eşit olduğunu
görünce bunu anlayacağız, $\beta$'i bir eşitliğin sağında olacak
şekilde bildiğimiz denklemleri düzenlersek,

$$
\lambda_{A,t} - \lambda_{B,t} = \alpha + \beta + u_t - (\alpha + u_t) 
$$

$$
= \alpha + \beta + u_t - \alpha - u_t = \beta
$$

Yani,

$$
\lambda_{A,t} - \lambda_{B,t} = \beta
$$

Eğer iki tarafın üstelini alırsak

$$
\frac{e^{A,t}}{e^{B,t}} = e^{\beta}
$$

Yani $\beta$ parametresi otomatik olarak A ve B farklı iki Poisson
$\lambda$ parametrelerinin oranını hesaplıyor! Dikkat bu hesabın
yapılabilmesi için GLM'in spesifik bir oran hesabı yapması gerekli
değil. Tek gereken GLM Poisson için gereken temel veri GLM
regresyonuna, ya da Bayes MCMC hesabına verilirken, tüm verinin, yani
A ve B birleşmiş olarak şu şekilde sunulması,

$$
\log(\lambda_{A,t}) = \alpha + \beta \cdot g_t + u_t
$$

Tabii üsttekinin hemen ardından $\lambda$ kullanılarak bir Poisson
üretimi yapıldığını düşünüyoruz, geri yönde ise eldeki veri üzerinden
MCMC ile sonsal -posterior- elde edip o dağılımdan örneklem
alıyoruz, bunlar standart yaklaşımlar.

Konuya dönersek $\beta \cdot g_t$ ifadesindeki $g_t$'ye dikkat, bu
parametre A grup verisi için 1, B grup verisi için 0 diyecek. Yani bu
formülasyonun doğal sonucu olarak elde ettiğimiz $\beta$ iki $\lambda$
parametresinin oranı haline gelecek. Her veri noktası $\beta$'yi kendi
tarafına doğru çekmeye uğraşacak, bu al-ver itme-çekme arasında
$\beta$'nin varacağı yer onun tüm zaman dilimleri için geçerli bir
orana ulaşmasıdır.

Senesel farklar, eğer var ise, her sene için farklı olmasına izin
verdiğimiz $u_t$ ile olabilir, bu parametre ile o farklılığı
"yakalayabiliyoruz".

Verinin nasıl oluşturulduğunu görelim,

```python
def build_stacked_arrays(simdict):
    years = np.array(simdict["years"])
    A = np.array(simdict["A"])
    B  = np.array(simdict["B"])
    n_years = len(years)
    counts = np.concatenate([A, B])
    group = np.concatenate([np.ones(n_years, dtype=int), np.zeros(n_years, dtype=int)])
    year_idx = np.concatenate([np.arange(n_years), np.arange(n_years)])
    return years, counts, group, year_idx, A, B

years, counts_pois, group_pois, year_idx_pois, A_arr, B_arr = build_stacked_arrays(sim)
print (len(A_arr), A_arr[:5], '..')
print (len(B_arr), B_arr[:5], '..')
print (len(counts_pois), counts_pois)
print ('A mi B mi?')
print (group_pois)
```

```text
61 [14 24 38 15 13] ..
61 [10 24 26 13 12] ..
122 [14 24 38 15 13 47  7 17 28 13 14 22 43 19 26 18 53 46 30 25 33 44 14 37
 15 26 22 12 24 15 21  8 10 23 51 19 22 24 16 23 14 16 20 34 34 21 67 26
 44 57 20 16 54 18 26 49 35 51 50 25 18 10 24 26 13 12 39 10 22 41 15 15
 24 33 18 18 17 64 57 35 22 32 44 15 31  8 14 35 14 27 22 26 10 13 17 41
 27 31 42 11 27 19 12 31 27 28 25 69 36 30 69 20 17 52 15 33 53 38 51 44
 37 27]
A mi B mi?
[1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0]
```

Şimdi oranın sonsal dağılımını PyMC ile ortaya çıkartalım ve ondan
örneklem alalım.

```python
def fit_poisson_ratio(years, A_arr, B_arr, fout):
    n_years = len(years)
    with pm.Model() as model_synth:
        sigma_year = pm.HalfNormal("sigma_year", sigma=1.0)
        year_offset = pm.Normal("year_offset", 0.0, 1.0, shape=n_years)
        year_effect = pm.Deterministic("year_effect", year_offset * sigma_year)

        alpha = pm.Normal("alpha", 0.0, 2.0)
        beta  = pm.Normal("beta", 0.0, 1.0)

        log_lambda = alpha + beta * group_pois + year_effect[year_idx_pois]
        lambda_ = pm.math.exp(log_lambda)

        obs = pm.Poisson("obs", mu=lambda_, observed=counts_pois)

        rate_ratio = pm.Deterministic("rate_ratio", pm.math.exp(beta))

        idata = pm.sample(1000, tune=1000, target_accept=0.9, return_inferencedata=True, random_seed=SEED)

        graphviz = pm.model_to_graphviz(model_synth)
        graphviz.graph_attr.update(dpi="100")
        graphviz.render(fout, format="jpg")
        print(az.summary(idata, var_names=["alpha", "beta", "sigma_year", "rate_ratio"], round_to=3))
        rr_samples = idata.posterior["rate_ratio"].values.flatten()
        p_gt_1 = (rr_samples > 1.0).mean()
        print(f"\nP(rate_ratio > 1) = {p_gt_1:.3f}")    
        return idata
	
idata = fit_poisson_ratio(years, A_arr, B_arr, "stat_082_rapoi_03")
```

```text
                                                                                
                              Step      Grad      Sampli…                       
  Progre…   Draws   Diverg…   size      evals     Speed     Elapsed   Remaini…  

     2000    0         0.229     15        639.58    0:00:03   0:00:00   
                                           draws/s                       
     2000    0         0.306     15        676.39    0:00:02   0:00:00   
                                           draws/s                       
     2000    0         0.217     15        625.81    0:00:03   0:00:00   
                                           draws/s                       
     2000    0         0.278     15        660.10    0:00:03   0:00:00   
                                                  draws/s                       
                                                                                
             mean     sd  hdi_3%  hdi_97%  ...  mcse_sd  ess_bulk  ess_tail  r_hat
alpha       3.234  0.068   3.107    3.366  ...    0.002   480.667   818.932  1.012
beta       -0.040  0.035  -0.107    0.023  ...    0.001  6166.596  2749.392  1.000
sigma_year  0.489  0.050   0.397    0.582  ...    0.001   615.175  1211.197  1.011
rate_ratio  0.961  0.033   0.899    1.024  ...    0.001  6166.596  2749.392  1.000

[4 rows x 9 columns]

P(rate_ratio > 1) = 0.121
```

![](stat_082_rapoi_03.jpg)


```python
rr_samples = idata.posterior["rate_ratio"].values.flatten()
plt.figure(figsize=(6,3))
az.plot_posterior(rr_samples, hdi_prob=0.95)
plt.title("Hesaplanan oran (Poisson)")
plt.legend()
plt.tight_layout()
plt.savefig('stat_082_rapoi_02.jpg')
```

![](stat_082_rapoi_02.jpg)

Görüyoruz ki oranın dağılımı 1 etrafında kümelenmiş, o zaman iki sayım
zaman serisinin birbirine yakın olduğuna karar verebiliriz çünkü
oranları 1'e yakın.

Şimdi yakın olmayan iki zaman serisi yaratalım,


```python
np.random.seed(42)
years = np.arange(1980, 2020)
T = len(years)
u_t = np.random.normal(0, 0.4, size=T)
alpha = 3.0
beta_true = np.log(1.3)   # 30% higher A rate
mu_B = np.exp(alpha + u_t)
mu_A = np.exp(alpha + beta_true + u_t)
B = np.random.poisson(mu_B)
A = np.random.poisson(mu_A)

sim = {"years": years, "A": A, "B": B}
years, counts_pois, group_pois, year_idx_pois, A_arr, B_arr = build_stacked_arrays(sim)
idata = fit_poisson_ratio(years, A_arr, B_arr, "stat_082_rapoi_03")
```

```text
                                                                                
                              Step      Grad      Sampli…                       
  Progre…   Draws   Diverg…   size      evals     Speed     Elapsed   Remaini…  

     2000    0         0.346     7         693.28    0:00:02   0:00:00   
                                           draws/s                       
     2000    0         0.305     15        707.15    0:00:02   0:00:00   
                                           draws/s                       
     2000    0         0.314     15        658.19    0:00:03   0:00:00   
                                           draws/s                       
     2000    0         0.278     15        651.81    0:00:03   0:00:00   
                                                  draws/s                       
                                                                                
             mean     sd  hdi_3%  hdi_97%  ...  mcse_sd  ess_bulk  ess_tail  r_hat
alpha       2.879  0.073   2.736    3.011  ...    0.001  1030.075  1813.990  1.002
beta        0.317  0.047   0.229    0.408  ...    0.001  8153.224  2749.679  1.000
sigma_year  0.379  0.053   0.277    0.474  ...    0.001  1173.341  1764.667  1.003
rate_ratio  1.375  0.065   1.258    1.503  ...    0.001  8153.224  2749.679  1.000

[4 rows x 9 columns]

P(rate_ratio > 1) = 1.000
```

```python
rr_samples = idata.posterior["rate_ratio"].values.flatten()
plt.figure(figsize=(6,3))
az.plot_posterior(rr_samples, hdi_prob=0.95)
plt.title("Hesaplanan oran (Poisson)")
plt.legend()
plt.tight_layout()
plt.savefig('stat_082_rapoi_04.jpg')
```

![](stat_082_rapoi_04.jpg)

Bu zaman serileri birbirine yakın değiller. Sonsal dağılımın
kümelendiği yer 1'den çok uzakta. 

Kaynaklar

[1] Bayramli, Istatistik, *Sayım, Poisson ve Negatif Binom Bazlı Genel Lineer Modelleri (GLM)*

[2] Clarke, *An application of the Poisson distribution*,
     [https://www.actuaries.org.uk/system/files/documents/pdf/0481.pdf](https://www.actuaries.org.uk/system/files/documents/pdf/0481.pdf)
