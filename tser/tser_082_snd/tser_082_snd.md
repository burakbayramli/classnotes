# Ses Verisi İşleme, Tanıma

Kulağımızla duyduğumuz sesleri tek boyutlu bir zaman serisi olarak
düşünebiliriz. Bir ses parçasını (phoneme) grafiklemek istesek alttaki
gibi bir grafik elde ederdik.

```python
import scipy
owfile = "/home/burak/Documents/classnotes/sk/2024/11/phonemes/ow.wav"
fs, ow = scipy.io.wavfile.read(fname)
plt.plot(ow)
plt.savefig('tser_082_snd_01.jpg')
```

![](tser_082_snd_01.jpg)

Grafikte periyodik dalgalar görülebiliyor, fakat bir tane değil birden
fazla var, yani üstteki ses birkaç periyodik zaman serisinin toplamı.
Genel olarak bir ses dalgasının bir ve daha fazla sinüs eğrisi toplamı
olduğunu farzedebiliriz. Sinüs derken tabii ki her bileşenin genliği,
frekansı, ve fazı (phase) farklı olabilir.

```python
x = np.linspace(0,4,100)
fig, axs = plt.subplots(2, 2, sharex=True)

axs[0,0].plot(x,np.sin(2*np.pi*x))
axs[0,0].set_ylim(-2,2)
axs[0,0].grid(True)
axs[0,0].set_title(r'$sin(2 \pi x)$')

axs[0,1].plot(x,2*np.sin(2*np.pi*x))
axs[0,0].set_ylim(-2,2)
axs[0,1].grid(True)
axs[0,1].set_title(r'$2 \cdot sin(2 \pi x)$')

axs[1,0].plot(x,np.sin(2*np.pi*x))
axs[1,0].plot(x,np.sin(4*np.pi*x))
axs[1,0].set_ylim(-2,2)
axs[1,0].grid(True)
axs[1,0].set_title(r'$sin(2 \pi x)$ ve $sin(4 \pi x)$')

axs[1,1].plot(x,np.sin(2*np.pi*x))
axs[1,1].plot(x,np.sin(2*np.pi*x + 1.0))
axs[1,1].set_ylim(-2,2)
axs[1,1].grid(True)
axs[1,1].set_title(r'$sin(2 \pi x)$ ve $sin(2 \pi x + 0.5)$')

plt.savefig('tser_082_snd_02.jpg')
```

![](tser_082_snd_02.jpg)

Üstteki grafiklerde birkaç farklı sinüs eğrilerine bakabiliriz. Üst
solda bir $\sin(2 \pi x)$ eğrisi var. Onun genliğini (amplitude)
arttırmak için bir sabit sayı ile çarpıyoruz, örnekte iki ile, bu bize
sağ üstteki $2 \sin (2\pi x)$ sonucunu veriyor. Frekans arttırımı
için, yani aynı periyot içinde daha fazla salınım için $\sin$ hesabına
geçilen değer daha yüksek bir katsayı ile çarpılabilir, bunun sonucu
sol altta. Eğer aynı eğriyi sadece kaydırmak isteseydik bunu $\sin$
hesabına geçilen değere bir değer toplayarak yapardık, böylece
kaydırım gerçekleşirdi, onun sonucu ise sağ altta.

Simdi bu kavramlari kullanarak kendimiz iki tane ses dalgasi yaratalim.

```python
import scipy.io.wavfile
T = 1.0; fs = 16000
t = np.linspace(0, T, int(T*fs), endpoint=False) 
```

Alttaki sürekli bir bip sesi verecek, telefonda meşgul veren sese
benziyor. Bu sesi üretmek için tek bir sinüs eğrisi yeterli.

```python
x = np.sin(2*np.pi*440*t)
x_scaled = x * 32767.0
x_int16 = x_scaled.astype(np.int16)
scipy.io.wavfile.write('/tmp/sound-out1.wav', fs, x_int16)
```

Bir ambulans sirenine benzeyen sesi üretmek için iki tane sinüs eğrisi
birleştirebiliriz, bu dalgaların ikisi farklı frekanslarda, ve
karışımları bize sonuç sesi veriyor.

```python
x = np.sin(2*np.pi*1500*t - 100*np.sin(2*2*np.pi*t))
x_scaled = x * 32767.0
x_int16 = x_scaled.astype(np.int16)
scipy.io.wavfile.write('/tmp/sound-out2.wav', fs, x_int16)
```

### Sinüssel Regresyon (Sinusoidal Regression)

Ses tanima kavramina gelelim. Eger bir sesin birden fazla farkli sinus
egrisi toplami oldugu dogru ise bu ses dalgasinin sinus bilesenlerini
ayirabilirsek, ses tanima baglaminda saglam bir adim atmis olurduk.
Fakat sinüs eğrisini, tek sinüs eğrisi olduğu durumda bile, nasıl
genligini buyuterek, yana kaydırarak, frekansini arttirarak tam doğru
uyum noktasini bulacağız? Yani veriye uydurmak istedigimiz formül

$$
f(x) = A \sin (x+\varphi)
\qquad (1)
$$

Genlik $A$ ile faz ise $\varphi$ ile gösterilmiş, öyle bir $A,\varphi$ bulalım
ki sonuç sinüs eğrisi tam veriye uysun.

Veriye uydurma deyince akla lineer regresyon geliyor, fakat üstteki
formülü olduğu gibi regresyona sokmak mümkün değil, çünkü faz
kaydırmak için $\sin$ içindeki parametrenin değişmesi lazım, regresyon
bunları yapamaz. Ama regresyona problemi `katsayı çarpı basit formül''
formunda sunabilir miyiz acaba? Bir trigonometrik eşitlikten biliyoruz
ki

$$  A \sin (x+\varphi) = a\sin(x) + b\cos(x) $$

ki $\sin\varphi = \frac{b}{\sqrt{a^2+b^2}}$, ve $A = \sqrt{a^2+b^2}$ olacak  sekilde. 

Bu eşitliğin doğru olduğunu kontrol edelim,

$$ a \sin(x) + b \cos(x) = \sqrt{a^2+b^2} \left(\frac{a}{\sqrt{a^2+b^2}} \sin(x) + \frac{b}{\sqrt{a^2+b^2}} \cos(x)\right) $$

$$  = A\left[\sin(x)\cos(\varphi) + \cos(x)\sin(\varphi)\right] $$

$$ = A\sin(x+\varphi) $$

O zaman $a \sin(x) + b \cos(x)$ için regresyon yapmak (1) için
regresyon yapmak ile aynı şeydir. Regresyon iki toplam üzerinden
tanımlı o formül için en uygun $a,b$ katsayılarını hesaplayacak.

Eğer birden fazla farklı frekanstaki eğrileri uydurmak istersek,
ana formülü şu halde düşünebilirdik,

$$
A_1 \sin(2\pi x+\varphi_1) + A_2 \sin(4\pi x+\varphi_2) + ...
$$

Ve onun regresyona hazır açılımı

$$
a_1 \sin(2\pi x) + b_1 \cos(2\pi x) + a_1 \sin(4\pi x) + b_1 \cos(4\pi x) + ...
$$

formülünü regresyonda kullanırdık.

Ses verisine gelmeden önce alttaki daha basit veri üzerinde görelim,
orada bir veya birden fazla sinüs eğrisi uydurmak istiyoruz.

```python
import pandas as pd
df = pd.read_csv('baltic.csv')
df.plot(x='toy',y='degs',kind='scatter')
plt.savefig('tser_sinreg_01.png')
```

![](tser_sinreg_01.png)

Önce $\sin$ içinde $2\pi x$ ile başlarız, 

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

```python
fs, ow = scipy.io.wavfile.read(owfile)

N = len(ow) 
t = np.arange(N) / fs

f_start = 100.0
f_end = 4000.0
frequencies_hz = np.linspace(f_start, f_end, 500)

basis_functions = {}
for f in frequencies_hz:
    angular_freq = 2 * np.pi * f * t
    basis_functions[f'sin{f:.6f}'] = np.sin(angular_freq)
    basis_functions[f'cos{f:.6f}'] = np.cos(angular_freq)

X_unaugmented = pd.DataFrame(basis_functions)

Y = pd.Series(ow, name='wav').astype(np.float64) # Use the raw 'ow' array for Y

X_full = sm.add_constant(X_unaugmented, prepend=True) 

model = sm.OLS(Y, X_full)
results = model.fit()
```

```python
basis_matrix_full = X_full 
all_coeffs = results.params

is_significant = results.pvalues < 0.05

significant_features = results.pvalues[is_significant].index

print (f'tum {len(basis_matrix_full.columns)} filtre sonrasi {len(significant_features)}')

selected_basis_matrix = basis_matrix_full.loc[:, significant_features]

selected_coeffs = all_coeffs[is_significant]

reconstructed_signal_series = selected_basis_matrix.dot(selected_coeffs)

y = np.array(reconstructed_signal_series).flatten() 

y_int16 = y.astype(np.int16)

scipy.io.wavfile.write('/tmp/sound-out3.wav', fs, y_int16)
```





[devam edecek]

Kaynaklar

[1] <a href="../../sk/2018/01/speech-recognition.html">YSA ile Konuşma Tanıma (Speech Recognition)</a>

[2] ./sk/2024/11/ses-tanima-isleme.html

[3] Bayramli, Istatistik, Tahmin Edici Hesaplar (Estimators)

[4] Cross Validated, *How to find a good fit for semi­sinusoidal model in  R?*,
[http://stats.stackexchange.com/questions/60500/how-to-find-a-good-fit-for-semi-sinusoidal-model-in-r](http://stats.stackexchange.com/questions/60500/how-to-find-a-good-fit-for-semi-sinusoidal-model-in-r)

