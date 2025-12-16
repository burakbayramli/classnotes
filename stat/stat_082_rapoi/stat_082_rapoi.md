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
list(map(lambda x: np.round(x,2), probs))
```

```
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
çıkabilir, 

Ülke bazında spesifik bir olaya bağlı bir sayım verisine bakıyoruz, bu
veri sene bazında toplanıyor. Bir veri mesela yıllık "dört yol
kavşaklarında olan kaza sayısı" olabilir. Kavşaklar tabii ki ülkenin
her tarafında, hepsine bakıp her sene için orada olan kazaları
topluyoruz. Sonra diyelim ki bu kazaların normal (!)  kazalardan daha
yüksek / farklı olup olmadığını merak ediyoruz, o zaman diğer bir
ölçüm sene bazlı dört yol kavşakları *dışındaki* kazalar olur.

O zaman elimizde iki zaman serisi olacak, her sene için iki tane
ölçüm. Her ölçüm rakamının, bir sayım olduğu için, Poisson
dağılımınden geldiğini kabul edebiliriz. Fakat dikkat, her sene *aynı*
Poisson dağılımından mı geliyor? Büyük ihtimalle hayır çünkü kaza
sayılarında sene bazlı değişim olabilir: araç sayıları farklı
olabilir, yol şartları değişmiş olabilir.

Bir diger problem, ayni senedeki iki olcum karsilastirirken bile, bir
olcekleme (scaling) problemi olabilir.







![](stat_082_rapoi_01.jpg)

![](stat_082_rapoi_02.jpg)


[devam edecek]

Kaynaklar

[1] Bayramli, Istatistik, *Sayım, Poisson ve Negatif Binom Bazlı Genel Lineer Modelleri (GLM)*

[2] Clarke, *An application of the Poisson distribution*,
     [https://www.actuaries.org.uk/system/files/documents/pdf/0481.pdf](https://www.actuaries.org.uk/system/files/documents/pdf/0481.pdf)
