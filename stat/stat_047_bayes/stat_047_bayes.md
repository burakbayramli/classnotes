# Bayes Usulü İstatistiki Analiz

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



### t-Testi


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
[25.23675052 32.12395504 33.5959748  27.86411463 29.14062914 30.21713287
 31.89432325 33.96167142]
[17.55596863 22.93544608 26.08789679 29.68123357 14.45614477 36.28805289
 33.01712517 18.72362312]
```





```python
import pymc as pm

# Your data setup (from Pillon)
N = 250
mu_A, std_A = 30, 4
mu_B, std_B = 26, 7
durations_A = np.random.normal(mu_A, std_A, size=N)
durations_B = np.random.normal(mu_B, std_B, size=N)

# Pool the data for priors
pooled_mean = np.r_[durations_A, durations_B].mean()
pooled_std = np.r_[durations_A, durations_B].std()

tau = 1./np.sqrt(1000.*pooled_std)
mu_A = pm.Normal("mu_A", pooled_mean, tau)
mu_B = pm.Normal("mu_B", pooled_mean, tau)

std_A = pm.Uniform("std_A", pooled_std/1000., 1000.*pooled_std)
std_B = pm.Uniform("std_B", pooled_std/1000., 1000.*pooled_std)

nu_minus_1 = pm.Exponential("nu-1", 1./29)

obs_A = pm.NoncentralT("obs_A", mu_A, 1.0/std_A**2, nu_minus_1 + 1,
                       observed=True, value=durations_A)
obs_B = pm.NoncentralT("obs_B", mu_B, 1.0/std_B**2, nu_minus_1 + 1,
                       observed=True, value=durations_B)
```




```python
# HERE'S THE KEY: Add a deterministic node for the difference
@pm.deterministic
def diff_of_means(mu_A=mu_A, mu_B=mu_B):
    return mu_A - mu_B

@pm.deterministic
def diff_of_stds(std_A=std_A, std_B=std_B):
    return std_A - std_B

@pm.deterministic
def effect_size(mu_A=mu_A, mu_B=mu_B, std_A=std_A, std_B=std_B):
    return (mu_A - mu_B) / np.sqrt((std_A**2 + std_B**2) / 2)

# Run MCMC
mcmc = pm.MCMC([obs_A, obs_B, mu_A, mu_B, std_A, std_B, nu_minus_1, 
                diff_of_means, diff_of_stds, effect_size])
mcmc.sample(25000, 10000)

# Now you can access the traces
diff_means_trace = mcmc.trace('diff_of_means')[:]
diff_stds_trace = mcmc.trace('diff_of_stds')[:]
effect_size_trace = mcmc.trace('effect_size')[:]

# Plot the difference of means (Kruschke style)
plt.figure(figsize=(10, 4))
plt.hist(diff_means_trace, bins=50, histtype='stepfilled', alpha=0.85, density=True)
plt.axvline(0, color='red', linestyle='--', label='Null hypothesis')
plt.xlabel('μ_A - μ_B')
plt.ylabel('Density')
plt.title('Posterior Distribution of Difference of Means')

# Calculate percentage > 0
pct_greater = (diff_means_trace > 0).sum() / len(diff_means_trace) * 100
plt.text(0.7, 0.9, f'{pct_greater:.1f}% > 0', transform=plt.gca().transAxes)

# Add 95% HDI (you'd need to calculate this - see below)
plt.legend()
plt.savefig('stat_047_bayes_01.jpg')
```













[devam edecek]

Kaynaklar



