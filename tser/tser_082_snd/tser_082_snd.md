# Ses Verisi İşleme, Tanıma












### Sinüssel Regresyon (Sinusoidal Regression)

Alttaki veriye bir veya birden fazla sinüs eğrisi uydurmak istiyoruz. 

```python
import pandas as pd
df = pd.read_csv('baltic.csv')
df.plot(x='toy',y='degs',kind='scatter')
plt.savefig('tser_sinreg_01.png')
```

![](tser_sinreg_01.png)

Fakat sinüs eğrisini, tek sinüs eğrisi olduğu durumda bile, nasıl yana
kaydırarak tam doğru noktayı bulacağız? Ayrıca eğrinin genliği (amplitude)
önemli. Tüm bunları kapsayan formül

$$ f(x) = A \sin (x+\varphi) $$

Genlik $A$ ile faz ise $\varphi$ ile gösterilmiş, öyle bir $A,\varphi$ bulalım
ki sonuç sinüs eğrisi tam veriye uysun. Veriye uydurma deyince akla lineer
regresyon geliyor, fakat üstteki formülü olduğu gibi regresyona sokmak mümkün
değil, çünkü faz kaydırmak için $\sin$ içindeki parametrenin değişmesi lazım,
regresyon bunları yapamaz. Ama regresyona problemi `katsayı çarpı basit formül''
şeklinde sunabilir miyiz acaba? Bir trigonometrik eşitlikten biliyoruz ki

$$  A \sin (x+\varphi) = a\sin(x) + b\cos(x) $$

ki $\sin\varphi = \frac{b}{\sqrt{a^2+b^2}}$, ve $A = \sqrt{a^2+b^2}$ olacak  sekilde. 

Bu eşitliğin doğru olduğunu kontrol edelim,

$$ a \sin(x) + b \cos(x) = \sqrt{a^2+b^2} \left(\frac{a}{\sqrt{a^2+b^2}} \sin(x) + \frac{b}{\sqrt{a^2+b^2}} \cos(x)\right) $$

$$  = A\left[\sin(x)\cos(\varphi) + \cos(x)\sin(\varphi)\right] $$

$$ = A\sin(x+\varphi) $$

O zaman $a \sin(x) + b \cos(x)$ için regresyon yapabiliriz. Regresyon iki toplam
üzerinden tanımlı fonksiyonlar için en uygun $a,b$ katsayılarını
hesaplayacak. Önce $\sin$ içinde $2\pi x$ ile başlarız, 

```python
import statsmodels.formula.api as smf
results = smf.ols('degs ~ np.sin(2*np.pi*toy) + np.cos(2*np.pi*toy)', data=df).fit()
print (results.summary())
```

```text
                            OLS Regression Results                            
==============================================================================
Dep. Variable:                   degs   R-squared:                       0.969
Model:                            OLS   Adj. R-squared:                  0.968
Method:                 Least Squares   F-statistic:                     704.3
Date:                Fri, 27 Jun 2025   Prob (F-statistic):           1.10e-34
Time:                        12:37:22   Log-Likelihood:                -63.360
No. Observations:                  48   AIC:                             132.7
Df Residuals:                      45   BIC:                             138.3
Df Model:                           2                                         
Covariance Type:            nonrobust                                         
===========================================================================================
                              coef    std err          t      P>|t|      [0.025      0.975]
-------------------------------------------------------------------------------------------
Intercept                   8.2917      0.135     61.407      0.000       8.020       8.564
np.sin(2 * np.pi * toy)    -5.9156      0.191    -30.979      0.000      -6.300      -5.531
np.cos(2 * np.pi * toy)    -4.0463      0.191    -21.190      0.000      -4.431      -3.662
==============================================================================
Omnibus:                       28.673   Durbin-Watson:                   1.051
Prob(Omnibus):                  0.000   Jarque-Bera (JB):                4.298
Skew:                          -0.158   Prob(JB):                        0.117
Kurtosis:                       1.569   Cond. No.                         1.41
==============================================================================
```

```python
a,b = results.params[1],results.params[2]
A = (a**2+b**2)
print (A, np.rad2deg(np.arcsin(b**2 / A)))
```

```text
51.367286414382804 18.586661396939277
```

```python
fit1 = results.params[0] + results.params[1] * np.sin(2*np.pi*df.toy) + \
       results.params[2] * np.cos(2*np.pi*df.toy)
plt.scatter(df.toy,df.degs)
plt.plot(df.toy,fit1)
plt.savefig('tser_sinreg_02.png')
```

![](tser_sinreg_02.png)

Uyum fena değil. Daha iyi uyum için daha fazla terim ekleyebiliriz, mesela 
$\sin,\cos$ içinde $2 \pi x$ kullandık, bir de $4 \pi x$'li terimler ekleyerek,

```python
import statsmodels.formula.api as smf
formula = 'degs ~ np.sin(2*np.pi*toy) + np.cos(2*np.pi*toy) + ' + \
          '       np.sin(4*np.pi*toy) + np.cos(4*np.pi*toy)'
results = smf.ols(formula, data=df).fit()
print (results.summary())
```

```text
                            OLS Regression Results                            
==============================================================================
Dep. Variable:                   degs   R-squared:                       0.999
Model:                            OLS   Adj. R-squared:                  0.999
Method:                 Least Squares   F-statistic:                     9519.
Date:                Fri, 27 Jun 2025   Prob (F-statistic):           9.48e-63
Time:                        12:37:40   Log-Likelihood:                 16.130
No. Observations:                  48   AIC:                            -22.26
Df Residuals:                      43   BIC:                            -12.90
Df Model:                           4                                         
Covariance Type:            nonrobust                                         
===========================================================================================
                              coef    std err          t      P>|t|      [0.025      0.975]
-------------------------------------------------------------------------------------------
Intercept                   8.2917      0.026    314.450      0.000       8.238       8.345
np.sin(2 * np.pi * toy)    -5.9156      0.037   -158.634      0.000      -5.991      -5.840
np.cos(2 * np.pi * toy)    -4.0463      0.037   -108.506      0.000      -4.122      -3.971
np.sin(4 * np.pi * toy)     1.2124      0.037     32.513      0.000       1.137       1.288
np.cos(4 * np.pi * toy)     0.3333      0.037      8.939      0.000       0.258       0.409
==============================================================================
Omnibus:                        0.473   Durbin-Watson:                   2.983
Prob(Omnibus):                  0.790   Jarque-Bera (JB):                0.338
Skew:                          -0.200   Prob(JB):                        0.845
Kurtosis:                       2.909   Cond. No.                         1.41
==============================================================================

```

```python
fit2 = results.params[0] + \
       results.params[1] * np.sin(2*np.pi*df.toy) + \
       results.params[2]*np.cos(2*np.pi*df.toy) + \
       results.params[3] * np.sin(4*np.pi*df.toy) + \
       results.params[4]*np.cos(4*np.pi*df.toy) 
      
plt.scatter(df.toy,df.degs)
plt.plot(df.toy, fit2)
plt.savefig('tser_sinreg_03.png')
```

![](tser_sinreg_03.png)

Uyum daha iyi hale geldi.

Bir tane de mutlak değer içeren bir fonksiyon.

```python
import pandas as pd
x = np.linspace(0,10,400)
y = np.abs(np.sin(2*np.pi*x)) + np.random.random(len(x)) * 0.5
df = pd.DataFrame(x)
df['y'] = y
df.columns = ['x','y']
df.plot(x='x',y='y')
plt.savefig('tser_sinreg_04.png')
```

![](tser_sinreg_04.png)

```python
import statsmodels.formula.api as smf
results = smf.ols('y ~ np.abs(np.sin(2*np.pi*x)) + np.abs(np.cos(2*np.pi*x))', data=df).fit()
print (results.summary())
```

```text
                            OLS Regression Results                            
==============================================================================
Dep. Variable:                      y   R-squared:                       0.830
Model:                            OLS   Adj. R-squared:                  0.829
Method:                 Least Squares   F-statistic:                     968.9
Date:                Fri, 27 Jun 2025   Prob (F-statistic):          1.84e-153
Time:                        12:38:04   Log-Likelihood:                 206.22
No. Observations:                 400   AIC:                            -406.4
Df Residuals:                     397   BIC:                            -394.5
Df Model:                           2                                         
Covariance Type:            nonrobust                                         
=================================================================================================
                                    coef    std err          t      P>|t|      [0.025      0.975]
-------------------------------------------------------------------------------------------------
Intercept                         0.3500      0.074      4.719      0.000       0.204       0.496
np.abs(np.sin(2 * np.pi * x))     0.9428      0.059     15.943      0.000       0.827       1.059
np.abs(np.cos(2 * np.pi * x))    -0.0979      0.059     -1.650      0.100      -0.215       0.019
==============================================================================
Omnibus:                      292.929   Durbin-Watson:                   1.972
Prob(Omnibus):                  0.000   Jarque-Bera (JB):               25.280
Skew:                           0.035   Prob(JB):                     3.24e-06
Kurtosis:                       1.770   Cond. No.                         20.5
==============================================================================

```






```python
import scipy.io.wavfile
fs = 16000 
T = 1.0   
t = np.linspace(0, T, int(T*fs), endpoint=False) 
x = np.sin(2*np.pi*440*t)
x_scaled = x * 32767.0
x_int16 = x_scaled.astype(np.int16)
scipy.io.wavfile.write('/tmp/sound-out1.wav', fs, x_int16)
```

```python
x = np.sin(2*np.pi*1500*t - 100*np.sin(2*2*np.pi*t))
x_scaled = x * 32767.0
x_int16 = x_scaled.astype(np.int16)
scipy.io.wavfile.write('/tmp/sound-out2.wav', fs, x_int16)
```

Sinussoidal Regression

```python
import scipy.io.wavfile
import statsmodels.api as sm
import pandas as pd

tmp, ow = scipy.io.wavfile.read('/home/burak/Documents/classnotes/sk/2024/11/phonemes/ow.wav')
fs = 16000 
df = pd.DataFrame(ow)
N = len(df) # Number of samples
df.columns = ['wav']
t = np.arange(N) / fs
f_start = 100.0
f_end = 4000.0
frequencies_hz = np.linspace(f_start, f_end, 500)
for f in frequencies_hz:
   df['sin%f' % f] = np.sin(2 * np.pi * f * t)
   df['cos%f' % f] = np.cos(2 * np.pi * f * t)

X = sm.add_constant(np.array(df.drop(['wav'],axis=1)))
Y = df.wav.astype(float)
model = sm.OLS(Y,X)
results = model.fit()
```

```python
fs = 16000 
left = df.drop('wav',axis=1)
right = results.params
filt = df.columns[results.pvalues < 0.05]
print ('tum',len(df.columns), 'filtre sonrasi',len(filt))
left = left.loc[:,filt]
right = right[results.pvalues < 0.05]
tmp = np.zeros((1,len(right)))
tmp[0,:] = right
dff = left.dot(tmp.T)
y = np.array(dff).flatten() 
y_int16 = y.astype(np.int16)
scipy.io.wavfile.write('/tmp/sound-out3.wav', fs, y_int16)
```

```text
tum 1001 filtre sonrasi 88
```





[devam edecek]

Kaynaklar

./sk/2024/11/ses-tanima-isleme.html

Sezonsallık, Periyotlar, ./tser/tser_017_seas/tser_017_seas.html

Bayramli, Istatistik, Tahmin Edici Hesaplar (Estimators)

Cross Validated, *How to find a good fit for semi­sinusoidal model in  R?*,
[http://stats.stackexchange.com/questions/60500/how-to-find-a-good-fit-for-semi-sinusoidal-model-in-r](http://stats.stackexchange.com/questions/60500/how-to-find-a-good-fit-for-semi-sinusoidal-model-in-r)

