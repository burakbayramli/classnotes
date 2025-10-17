# Ses Verişi İşleme, Tanıma















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