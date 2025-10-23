# Çok Seviyeli Modeller (Multilevel Models)

Lineer, lojistik regresyon tek seviyeli modellerdir; modellenen verinin
regresyona bildirilen tüm katsayılarının hepsi, aynı anda kullanılır. Fakat
bazı durumlarda, mesela coğrafi bir parametrenin modelin parçası olduğu
durumlarda daha değişik bir yaklaşım gerekli olabilir. Eğer regresyonumuzun
katsayılarının belli bir grup için (şehir, okul, zaman, bölge, vs), her
grup için farklı şekillerde veriye uydurulmasını (fit) istiyorsak, o zaman
çok seviyeli modelleri kullanmak gerekebilir.

Altta gösterilen iki parametreli klasik regresyon

$$ y_i = \alpha + \beta x_i + \epsilon_i $$

çok seviyeli modellerde mesela $\alpha$'yi, yani kesisi (intercept) her
grupta farklı olacak şekilde uydurabilir,

$$ y_i = \alpha_{j[i]} + \beta x_i + \epsilon_i $$

Bu durumda her grup $j$'nin kendi kesi değeri olacaktır. Ya da her grubun
kendi eğimi (slope) olacak şekilde $\beta$'nin gruptan gruba değişmesine
izin verilebilir,

$$ y_i = \alpha + \beta_{j[i]} x_i + \epsilon_i $$

Ya da her ikisinin birden değişmesine izin verilebilir,

$$ y_i = \alpha_{j[i]} + \beta_{j[i]} x_i + \epsilon_i $$

Terminoloji

Literatürde bazen çok seviyeli modeller hakkında sabit etkiler (fixed
effects), rasgele etkiler (random effects) gibi kelimeler kullanıldığını
görürsünüz. Bu terminolojiye göre grup seviyesinde değişmesine izin verilen
$\alpha_j,\beta_j$ gibi parametrelere "sabit etki'' adı veriliyor, çünkü o
parametreler grup içinde değişmemektedir, modelin geri kalanı ise rasgele
etki olacaktır. Bu iki kavramın karışımı da (ki neredeyse her zaman öyle
olur) "karışık etki (mixed effects)'' modeli olarak anılıyor. Bu
terminoloji biraz kafa karıştırıcı olabilir, ama bilinmesi iyidir böylece
literatürü takip edebiliriz. Biz burada Gelman & Hill [1]’deki tavsiyeyi
kullanıp "çok seviyeli modeller" kelimelerini tercih edeceğiz.

Örnek: İlaç Etkisi Verisi

Yeni bir ilacın etkili olup olmadığını anlamak için hastalar (subject)
üzerinde deneyler yapılır [2]. Bu veride ilginç olan hastanın durumunun
tekrar tekrar belli aralıklarla ölçülmesi, ve durumun (status) yeni bir
veri satırı olarak kaydedilmesi. Ayrıca rasgele seçilen hastalara ya ilaç,
ya da etkisiz ilaç (placebo) veriliyor. Veride cinsiyet (gender), yaş
(age), tedavi merkezi numarası (centre) kolonları var. İlk aydaki durum
"başlangıç noktası (baseline)" olarak ayrı bir kolona ayrılıyor, ve ilk
ay satırları regresyon öncesi siliniyor. Soru şudur: ilaç etkili midir?

Soru bir evet/hayır sorusu olduğu için lojistik regresyon kullanacağız.

Tek seviyeli (bağımsız gözlem varsayımıyla) model

```python
import statsmodels.api as sm, pandas as pd
import statsmodels.formula.api as smf

df = pd.read_csv('respiratory.csv', index_col=0)
baseline = df[df['month'] == 0][['subject','status']].set_index('subject')
df['status'] = (df['status'] == 'good').astype(int)
df['baseline'] = df.apply(lambda x: baseline.loc[x['subject']], axis=1)
df['centre'] = df['centre'].astype(str)
df = df[df['month'] > 0]

mdlm = smf.logit("status ~ baseline + month + treatment + gender + age + C(centre)", df)
mdlmf = mdlm.fit()
print(mdlmf.summary())
````

```
Pseudo R-squ.: 0.2071
...
treatment[T.treatment]  1.3006 (p<0.001)
```

Bu sonuçlara göre tedavinin katsayısı **1.30**, yani
$exp(1.3)=3.66$. İlaç uygulaması, iyileşme olasılığını yaklaşık **3.66 kat**
artırmaktadır.

Fakat bu modelin önemli bir problemi vardır: aynı kişiden gelen tekrar ölçümleri
bağımsız varsayar. Halbuki bir hastaya ait 4–5 satır veri birbirine
bağımlıdır. Bu nedenle, standart hatalar olduğundan küçük çıkabilir ve
sonuçlar fazla iyimser olur.

Çok seviyeli model (Bayesçi hiyerarşik yaklaşım, PyMC ile)

Bu problemi çözmek için hiyerarşik (çok seviyeli) modeller
kullanabiliriz. Bu modellerde, her **hasta** kendi kesisine (intercept)
sahip olur ve bu değerlerin tamamı üst düzey bir dağılımdan (örneğin
normal) geldiği varsayılır. Böylece bireysel farklılıklar hesaba katılır.

Bayesçi çerçevede bu yaklaşımın avantajları:

* Her parametre için **önceller (priors)** tanımlanır. Bu, modelin
  gerçekçi sınırlara sahip olmasını sağlar.
* Model çıktısı bir tek sayı yerine, her parametrenin **olasılık
  dağılımını (posterior)** verir.
* Hiyerarşik yapı sayesinde küçük gruplar veya az gözleme sahip bireyler
  için **kısmi havuzlama (partial pooling)** gerçekleşir; bu da
  katsayıların aşırı uç değerlerden uzaklaşmasını sağlar.

Model kurulumu

```python
import pandas as pd
import pymc as pm
import arviz as az
import numpy as np

df = pd.read_csv('respiratory.csv', index_col=0)
baseline = df[df['month'] == 0][['subject','status']].set_index('subject')
df['status'] = (df['status'] == 'good').astype(int)
df['baseline'] = df.apply(lambda x: baseline.loc[x['subject']], axis=1)
df['centre'] = df['centre'].astype(str)
df = df[df['month'] > 0]

# Kodlamalar
df['baseline_code'] = (df['baseline'] == 'poor').astype(int)
df['treatment_code'] = (df['treatment'] == 'treatment').astype(int)
df['gender_code'] = (df['gender'] == 'male').astype(int)
subject_idx, subjects = pd.factorize(df['subject'])
centre_idx, centres = pd.factorize(df['centre'])

with pm.Model() as hierarchical_model:
    sigma_subject = pm.HalfNormal('sigma_subject', sigma=2)
    sigma_centre = pm.HalfNormal('sigma_centre', sigma=2)
    subject_offset = pm.Normal('subject_offset', mu=0, sigma=1, shape=len(subjects))
    centre_offset = pm.Normal('centre_offset', mu=0, sigma=1, shape=len(centres))
    subject_effect = sigma_subject * subject_offset
    centre_effect = sigma_centre * centre_offset

    alpha = pm.Normal('alpha', mu=0, sigma=2)
    beta_baseline = pm.Normal('beta_baseline', mu=0, sigma=2)
    beta_month = pm.Normal('beta_month', mu=0, sigma=1)
    beta_treatment = pm.Normal('beta_treatment', mu=0, sigma=2)
    beta_gender = pm.Normal('beta_gender', mu=0, sigma=1)
    beta_age = pm.Normal('beta_age', mu=0, sigma=0.1)

    mu = (alpha +
          subject_effect[subject_idx] +
          centre_effect[centre_idx] +
          beta_baseline * df['baseline_code'].values +
          beta_month * df['month'].values +
          beta_treatment * df['treatment_code'].values +
          beta_gender * df['gender_code'].values +
          beta_age * df['age'].values)

    y = pm.Bernoulli('y', logit_p=mu, observed=df['status'].values)
    trace = pm.sample(2000, tune=1000, chains=4, target_accept=0.95)
```

Sonuçlar

Örneklemden elde edilen posterior özetleri:

| Parametre     | Ortalama | sd    | 3% HDI | 97% HDI |
| ------------- | -------- | ----- | ------ | ------- |
| sigma (Intercept) | 1.52     | 1.09  | -0.67  | 3.46    |
| beta_baseline    | -2.92    | 0.57  | -4.06  | -1.90   |
| beta_month       | -0.09    | 0.13  | -0.33  | 0.15    |
| beta_treatment   | 2.07     | 0.55  | 1.03   | 3.08    |
| beta_gender      | 0.12     | 0.58  | -0.99  | 1.22    |
| beta_age         | -0.018   | 0.021 | -0.056 | 0.021   |
| sigma_subject     | 2.19     | 0.34  | 1.56   | 2.82    |
| sigma_centre      | 1.27     | 0.93  | 0.00   | 2.98    |

Tedavi etkisi (beta_treatment) için posterior dağılımın log-odds değeri
2.07’tir. Bunun üssü alınarak olasılık oranı (odds ratio) hesaplanır:

$$ \text{OR} = e^{2.07} \approx 7.75 $$

94% güven aralığı (HDI): [2.05, 19.16]

Bu sonuç, tedavinin iyileşme olasılığını yaklaşık 7.75 kat artırdığını
göstermektedir.  Bu değer, tek seviyeli modelin bulduğu 3.66
katsayısından oldukça büyüktür. Ayrıca yaş değişkeninin belirsizliği
daha gerçekçi (daha geniş) hale gelmiştir. Bu, hiyerarşik modelin
gruplar arası varyansı hesaba katmasının bir sonucudur.

Sonuçların yorumlanması

* **Hiyerarşik yapı**, aynı kişiye ait tekrar ölçümlerin bağımlılığını
  dikkate alır ve parametre tahminlerini daha güvenilir hale getirir.
  
* **Posterior dağılımlar**, sadece noktadan tahmin yerine olasılık aralıkları
  verir; bu da belirsizliği açıkça görmemizi sağlar.
  
* **sigma_subject** değeri (~2.19), bireyler arası farklılıkların güçlü
  olduğunu göstermektedir.
  
* **sigma_centre** (~1.27) ise tedavi merkezleri arasında da bir miktar
  varyans bulunduğunu işaret eder.

Sonuç olarak, Bayesçi çok seviyeli yaklaşım hem daha esnek hem de daha
gerçekçi bir modelleme imkânı sunar. Posterior dağılımları inceleyerek
hem tahmin değerlerini hem de bu tahminlerin belirsizlik düzeyini
görebiliriz.

Kaynaklar

[1] Gelman, Hill, *Data Analysis Using Regression and Multilevel/Hierarchical Models*

[2] Everitt, *A Handbook of Statistical Analysis Using R*

