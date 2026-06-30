# Bayes Usulü Parçalı Regresyon

Diyelim ki alttaki veri üzerinde lineer regresyon işletmek istiyoruz, yani
veriye bir çizgi uydurma problemi olarak yaklaşacağız.

```python
import pandas as pd
df = pd.read_csv('../../compscieng/compscieng_app20cfit/cave.csv')
plt.figure(figsize=(5, 3))
plt.scatter(df.Temp, df.C,s=3)
plt.grid(True)
plt.savefig('stat_102_regchpt_01.jpg')
```

![](stat_102_regchpt_01.jpg)

```python
import statsmodels.formula.api as smf
results = smf.ols('C ~ Temp', data=df).fit()
print ("R^2 = %0.2f (%0.2f, %0.2f)" % (results.rsquared, results.params[0],results.params[1],))
```

```text
R^2 = 0.62 (44.26, -0.30)
```

Uyum (fit) başarısı üstteki gibi rapor edildi. Sonuç fena değil ama
daha iyi olabilirdi. Şunu soralım, üstteki veriye tek bir çizgi
uydurmak uygun mudur? Aslında iki (ya da daha fazla) çizgi daha uygun
olmaz mıydı? Biz kabaca bakarak bile bunu görebiliyoruz. Kontrol
edelim, veriyi iki parça olarak alalım, ve her parça üzerinde ayrı bir
regresyon işletelim.


```python
results = smf.ols('C ~ Temp', data=df[0:15]).fit()
print ("R^2 = %0.2f" % results.rsquared)
results = smf.ols('C ~ Temp', data=df[15:-1]).fit()
print ("R^2 = %0.2f" % results.rsquared)
```

```text
R^2 = 0.77
R^2 = 0.80
```

Parçalı uyum sonucu daha iyi olarak rapor edildi. Demek ki üstteki
veride tek değil (en az) iki ve birbirinden farklı doğrusal ilişki
var.


```python
x_data = df['Temp'].values
y_data = df['C'].values
N = len(x_data)

def log_likelihood(alpha, beta, sigma, x, y):
    if sigma <= 0:
        return -np.inf

    mu = alpha + beta * x
    
    term1 = -0.5 * np.log(2 * np.pi)
    term2 = -np.log(sigma)
    term3 = -((y - mu) ** 2) / (2 * (sigma ** 2))
    return np.sum(term1 + term2 + term3)

def log_prior(alpha, beta, sigma):
    if sigma <= 0 or sigma > 50: 
        return -np.inf
    if not (-100 < alpha < 100): 
        return -np.inf
    if not (-10 < beta < 10):    
        return -np.inf
    return 0.0

def log_posterior(alpha, beta, sigma, x, y):
    return log_prior(alpha, beta, sigma) + log_likelihood(alpha, beta, sigma, x, y)

def metropolis_sampler(x, y, iterations=25000, proposal_width_alpha=1.0, proposal_width_beta=0.05, proposal_width_sigma=0.5):
    current_alpha = 0.0
    current_beta = 0.0
    current_sigma = 1.0
    
    alpha_trace = np.zeros(iterations)
    beta_trace = np.zeros(iterations)
    sigma_trace = np.zeros(iterations)
    
    accepted_count = 0
    
    for i in range(iterations):
        proposed_alpha = np.random.normal(current_alpha, proposal_width_alpha)
        proposed_beta = np.random.normal(current_beta, proposal_width_beta)
        proposed_sigma = np.random.normal(current_sigma, proposal_width_sigma)
        
        log_post_current = log_posterior(current_alpha, current_beta, current_sigma, x, y)
        log_post_proposed = log_posterior(proposed_alpha, proposed_beta, proposed_sigma, x, y)
        
        log_acceptance_ratio = log_post_proposed - log_post_current
        
        if np.log(np.random.uniform(0, 1)) < log_acceptance_ratio:
            current_alpha = proposed_alpha
            current_beta = proposed_beta
            current_sigma = proposed_sigma
            accepted_count += 1
            
        alpha_trace[i] = current_alpha
        beta_trace[i] = current_beta
        sigma_trace[i] = current_sigma
        
    print(f"Acceptance Rate: {accepted_count / iterations * 100:.2f}%")
    return alpha_trace, beta_trace, sigma_trace

iterations = 25000
burn_in = 5000

alpha_chain, beta_chain, sigma_chain = metropolis_sampler(x_data, y_data, iterations=iterations)

final_beta = beta_chain[burn_in:]
final_sigma = sigma_chain[burn_in:]

print(f"\n--- Posterior Estimates (Standardized Scale) ---")
print(f"Beta: {np.mean(final_beta):.4f} ± {np.std(final_beta):.4f}")
print(f"Sigma (Residual Noise):      {np.mean(final_sigma):.4f} ± {np.std(final_sigma):.4f}")
```

```text
Acceptance Rate: 19.79%

--- Posterior Estimates (Standardized Scale) ---
Beta: -0.2970 ± 0.0247
Sigma (Residual Noise):      6.1444 ± 0.4725
```






Kaynaklar

[1] Bayramlı, İstatistik, *Lineer Regresyon*

[2] Bayramlı, Hesapsal Bilim, Eğri Uydurma, Aradeğerleme (Interpolation)