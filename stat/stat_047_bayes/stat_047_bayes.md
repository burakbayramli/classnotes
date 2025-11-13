# Bayes Usulü İstatistiki Analiz

Veri analizinde Bayes teorisi kullanımı sayesinde veri olasılığı,
bilmediğimiz parametreler hakkındaki bazı bilgilerimizi formülasyona
dahil edebiliyoruz. Bunun bir örneğini MAP hesaplarını işlerken
gördük.  Bu bölümde Bayes formüllerinin sonuçlarını bulmak için bazı
hesapsal teknikleri işleyeceğiz, ve niye faydalı olduklarını anlamaya
uğraşacağız.

Temel olasılık teorisinden biliyoruz ki Bayes teorisi

$$
P(A | B) = \frac{P(B|A) P(A)}{P(B)}
$$

diye gider. Üstteki formülü veri analizine uyarlayabiliriz.
İstatistiki modelin parametreleri $\theta$'yi $A = \theta$ yaparız, $B
= veri$ deriz, o zaman Bayes teorisi elde edilen verinin parametre
hesabı $\theta$ için nasıl kullanılacağının formülünü gösterir,

$$
P(\theta | veri) = \frac{P(veri | \theta) \times P(\theta)}{P(veri)}
$$

Formül öğelerinin açıklamasını [4] yazısında bulabiliriz.

```python
import pymc as pm, scipy.stats as stats
import pandas as pd
```


```python
Y = stats.bernoulli(0.7).rvs(20)

theta = 0.5
with pm.Model() as model:
     theta = pm.Beta("theta", alpha=1, beta=1)
     y_obs = pm.Binomial("eta_obs", n=1, p=theta, observed=Y)
     idata = pm.sample(1000, return_inferencedata=True)
```

```python
theta_post = np.array(idata.posterior['theta'])
print (np.mean(theta_post))
plt.hist(theta_post[0],bins=10)
plt.savefig('tser_023_bsts_03.jpg')
```

```text
0.5952869333604922
```

![](tser_023_bsts_03.jpg)

```python
graphviz = pm.model_to_graphviz(model)
graphviz.graph_attr.update(dpi="300")
graphviz.render("tser_023_bsts_02", format="jpg")
```

```text
Out[1]: 'tser_023_bsts_02.jpg'
```

<img width="200px" src="tser_023_bsts_02.jpg">



### T-Testi


```python
N = 250
mu_A, std_A = 30, 4
mu_B, std_B = 26, 7

durations_A = np.random.normal(mu_A, std_A, size=N)
durations_B = np.random.normal(mu_B, std_B, size=N)

print (durations_A[:8])
print (durations_B[:8])
```

```text
[24.16407223 26.33809627 26.02574579 33.21505722 28.50448144 26.99996422
 30.76649221 30.59385573]
[35.63522369 24.06562288 30.18425953 22.17136243 32.47047614 32.74779037
 33.07401654 34.0171137 ]
```

```python
# Pool the data for priors
pooled_mean = np.r_[durations_A, durations_B].mean()
pooled_std = np.r_[durations_A, durations_B].std()

# Build the model in modern PyMC
with pm.Model() as model:
    # Priors
    mu_A = pm.Normal("mu_A", mu=pooled_mean, sigma=1000*pooled_std)
    mu_B = pm.Normal("mu_B", mu=pooled_mean, sigma=1000*pooled_std)
    
    std_A = pm.Uniform("std_A", lower=pooled_std/1000., upper=1000.*pooled_std)
    std_B = pm.Uniform("std_B", lower=pooled_std/1000., upper=1000.*pooled_std)
    
    nu_minus_1 = pm.Exponential("nu_minus_1", lam=1./29)
    nu = pm.Deterministic("nu", nu_minus_1 + 1)
    
    # Likelihood - using StudentT instead of NoncentralT
    obs_A = pm.StudentT("obs_A", nu=nu, mu=mu_A, sigma=std_A, observed=durations_A)
    obs_B = pm.StudentT("obs_B", nu=nu, mu=mu_B, sigma=std_B, observed=durations_B)
    
    # Derived quantities (deterministic nodes)
    diff_of_means = pm.Deterministic("diff_of_means", mu_A - mu_B)
    diff_of_stds = pm.Deterministic("diff_of_stds", std_A - std_B)
    effect_size = pm.Deterministic("effect_size", 
                                    (mu_A - mu_B) / pm.math.sqrt((std_A**2 + std_B**2) / 2))
    
    # Sample
    trace = pm.sample(2000, tune=1000, return_inferencedata=False, cores=1)

```

```text
                                                                                
                              Step      Grad      Sampli…                       
  Progre…   Draws   Diverg…   size      evals     Speed     Elapsed   Remaini…  
 
            3000    0         0.901     7         1955.03   0:00:01   0:00:00   
                                                  draws/s                       
            3000    0         0.982     7         962.38    0:00:03   0:00:00   
                                                  draws/s                       
                                                                                
```


```python
graphviz = pm.model_to_graphviz(model)
graphviz.graph_attr.update(dpi="300")
graphviz.render("stat_047_bayes_02", format="jpg")
```

```text
Out[1]: 'stat_047_bayes_02.jpg'
```

<img width="500" src="stat_047_bayes_02.jpg"/>


```python
diff_means_trace = trace['diff_of_means']
diff_stds_trace = trace['diff_of_stds']
effect_size_trace = trace['effect_size']

plt.figure(figsize=(10, 4))
plt.hist(diff_means_trace, bins=50, histtype='stepfilled', alpha=0.85, density=True)
plt.axvline(0, color='red', linestyle='--', label='Sifir (Null) Hipotezi')
plt.xlabel('μ_A - μ_B')
plt.ylabel('Yogunluk (Density)')
plt.title('Ortalama Farki Sonsal Dagilimi')

# Sifirdan buyuk olma yuzdesi
pct_greater = (diff_means_trace > 0).sum() / len(diff_means_trace) * 100
pct_less = (diff_means_trace < 0).sum() / len(diff_means_trace) * 100
plt.text(0.7, 0.9, f'{pct_less:.1f}% < 0 < {pct_greater:.1f}%', 
         transform=plt.gca().transAxes, fontsize=12)

plt.legend()
plt.tight_layout()
plt.savefig('stat_047_bayes_01.jpg')
```

![](stat_047_bayes_01.jpg)




[devam edecek]

Kaynaklar

[1] Kruschke, *Bayesian Estimation Supersedes the t Test*

[2] Pillon, *Bayesian Method for Hackers*

[3] Gimenez, <a href="https://oliviergimenez.github.io/banana-book/crashcourse.html">
              Bayesian analysis of capture-recapture data with hidden Markov models
	      </a>

[4] Bayramli, Istatistik, *Tahmin Edici Hesaplar (Estimators)*

[5] Bayramli, Istatistik, *Değişim Noktası Analizi (Changepoint Analysis)*

