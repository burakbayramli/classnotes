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
print ("%0.2f" % results.rsquared)
```

```text
0.62
```

Uyum (fit) başarısı üstteki gibi rapor edildi. Sonuç fena değil ama
daha iyi olabilirdi. Şunu soralım, üstteki veriye tek bir çizgi
uydurmak uygun mudur? Aslında iki (ya da daha fazla) çizgi daha uygun
olmaz mıydı? Biz kabaca bakarak bile bunu görebiliyoruz. Kontrol
edelim, veriyi iki parça olarak alalım, ve her parça üzerinde ayrı bir
regresyon işletelim.


```python
results = smf.ols('C ~ Temp', data=df[0:15]).fit()
print ("%0.2f" % results.rsquared)
results = smf.ols('C ~ Temp', data=df[15:-1]).fit()
print ("%0.2f" % results.rsquared)
```

```text
0.77
0.80
```

Parçalı uyum sonucu daha iyi olarak rapor edildi. Demek ki üstteki
veride tek değil en az iki ve birbirinden farklı doğrusal ilişki var.








Kaynaklar

[1] Bayramlı, İstatistik, *Lineer Regresyon*

[2] Bayramlı, Hesapsal Bilim, Eğri Uydurma, Aradeğerleme (Interpolation)