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
terminoloji biraz kafa karıştırıcı olabilir, bilinmesi iyidir böylece
literatürü takip edebiliriz, ama biz [1]'deki tavsiyeyi kullanıp "çok
seviyeli modeller'' kelimelerini kullanacağız.

Örnek

Yeni bir ilacın etkili olup olmadığını anlamak için hastalar (subject)
üzerinde deneyler yapılır [2]. Bu veride ilginç olan hastanın durumunun
tekrar tekrar belli aralıklarla ölçülmesi, ve durumun (status) yeni bir
veri satırı olarak kaydedilmesi. Ayrıca rasgele seçilen hastalara ya ilaç,
ya da etkisiz ilaç (placebo) veriliyor. Veride cinsiyet (gender), yaş
(age), tedavi merkezi numarası (centre) kolonları var. İlk aydaki durum
"başlangıç noktası (baseline)'' olarak ayrı bir kolona ayrılıyor, ve ilk
ay satırları regresyon öncesi siliniyor. Soru şudur: ilaç etkili midir?

Soru bir evet/hayır sorusu olduğu için lojistik regresyon kullanacağız. 

```python
import statsmodels.api as sm, pandas as pd
import statsmodels.formula.api as smf
df = pd.read_csv('respiratory.csv',index_col=0)
baseline = df[df['month'] == 0][['subject','status']].set_index('subject')
df['status'] = (df['status'] == 'good').astype(int)
df['baseline'] = df.apply(lambda x: baseline.loc[x['subject']],axis=1)
df['centre'] = df['centre'].astype(str)
df = df[df['month'] > 0]
print (df.head(4).to_string())
```

```
    centre treatment  gender  age  status  month  subject baseline
112      1   placebo  female   46       0      1        1     poor
223      1   placebo  female   46       0      2        1     poor
334      1   placebo  female   46       0      3        1     poor
445      1   placebo  female   46       0      4        1     poor
```

```python
mdlm = smf.logit("status ~ baseline + month  + treatment + gender + \
age + C(centre)", df)
mdlmf = mdlm.fit()
print(mdlmf.summary())
```

```
Optimization terminated successfully.
         Current function value: 0.543694
         Iterations 6
                           Logit Regression Results                           
==============================================================================
Dep. Variable:                 status   No. Observations:                  444
Model:                          Logit   Df Residuals:                      437
Method:                           MLE   Df Model:                            6
Date:                Tue, 13 Nov 2018   Pseudo R-squ.:                  0.2071
Time:                        12:00:13   Log-Likelihood:                -241.40
converged:                       True   LL-Null:                       -304.47
                                        LLR p-value:                 8.385e-25
==========================================================================================
                             coef    std err          z      P>|z|      [0.025      0.975]
------------------------------------------------------------------------------------------
Intercept                  1.1436      0.426      2.682      0.007       0.308       1.979
baseline[T.poor]          -1.8841      0.241     -7.802      0.000      -2.357      -1.411
treatment[T.treatment]     1.3006      0.237      5.488      0.000       0.836       1.765
gender[T.male]             0.1194      0.295      0.405      0.686      -0.458       0.697
C(centre)[T.2]             0.6723      0.240      2.805      0.005       0.203       1.142
month                     -0.0643      0.100     -0.646      0.518      -0.259       0.131
age                       -0.0182      0.009     -2.050      0.040      -0.036      -0.001
==========================================================================================
```

Statsmodels altyapısı kategorik gördüğü değerleri 1-hot kodlamasıyla 1/0
değerli kolonlara çevirir, yani `treatment[T.treatment]` tedavi
uygulanıp uygulanmadığını gösteren 1/0 değerli kolondur. Bir başkası
`treatment[T.placebo]`; fakat bu kolon regresyonda "önemli''
bulunmadığı için üstte gösterilmemiş. 

Görülen katsayılara göre tedavinin (treatment) katsayısı 1.3,
$exp(1.3)=3.66$. Yani tedavi katsayısındaki 1 birimlik değişiklik (ki bu
0/1 bazlı bir değişken olduğu için tedavi uygulamak ya da uygulamamak
anlamına gelir), hastanın iyileşmesinde 3.66 kat etki yaratıyor.

Fakat bu regresyon sonuçlarındaki standart hatalarının bazılarından pek
memnun değiliz, mesela gruplararası değişkenlerin (between-subject
covariates), yaş gibi, standart hataları aşırı ufak. Bunun sebebi regresyon
işleminin tüm veri satırlarını bağımsız (independent) kabul etmesidir, yani
her satırdaki verinin çoğu aynı kişiye ait olsa bile farklı kişilere aitmiş
gibi işlenmektedir. Regresyon sonuçlarını irdelerken sürekli tetikte olmak
gerekir, görüldüğü gibi ufak hata bile bazen iyi bir şey olmayabiliyor!

Peki çözüm nedir? Çok seviyeli modeller burada devreye girebilir. Eğer
kişiyi ve ona tekabül eden tüm verileri bir grup olarak alırsak, o kişi
için alınan tüm ölçümlerin tekrar eden kısımlarının genele daha az etkide
bulunmasını sağlayabiliriz. Altta `glmer` adlı komut üzerinden çok
seviyeli regresyon örneğini görüyoruz, ayrıca R diliyle bağlantı kurmak ta
burada gösteriliyor; Python `statsmodels`'a bu fonksiyon daha
taşınmadı. Daha fazla detay için [3]'e bakılabilir.

```python
%load_ext rpy2.ipython
%R library(lme4)
```

```python
%R -i df 
%R p1 = "status ~ baseline + month + treatment + gender "
%R p2 = "+ age + centre + (1 | subject) "
%R params = paste(p1,p2)
%R resp_lmer <- glmer(as.formula(params), family = binomial(), data = df)
%R -o res res = summary(resp_lmer)
%R -o exp_res exp_res = exp(fixef(resp_lmer))
print (res)
print (exp_res)
```

```
Generalized linear mixed model fit by maximum likelihood (Laplace
  Approximation) [glmerMod]
 Family: binomial  ( logit )
Formula: status ~ baseline + month + treatment + gender + age + centre +  
    (1 | subject)
   Data: df

     AIC      BIC   logLik deviance df.resid 
   444.3    477.1   -214.2    428.3      436 

Scaled residuals: 
    Min      1Q  Median      3Q     Max 
-2.8574 -0.3590  0.1427  0.3693  2.2393 

Random effects:
 Groups  Name        Variance Std.Dev.
 subject (Intercept) 3.89     1.972   
Number of obs: 444, groups:  subject, 111

Fixed effects:
                   Estimate Std. Error z value Pr(>|z|)    
(Intercept)         1.68254    0.84436   1.993 0.046296 *  
baselinepoor       -3.07838    0.60272  -5.107 3.26e-07 ***
month              -0.10133    0.12518  -0.809 0.418257    
treatmenttreatment  2.16325    0.55644   3.888 0.000101 ***
gendermale          0.20249    0.67270   0.301 0.763402    
age                -0.02546    0.02014  -1.264 0.206125    
centre2             1.04667    0.54784   1.911 0.056064 .  
---

Correlation of Fixed Effects:
            (Intr) bslnpr month  trtmnt gndrml age   
baselinepor -0.367                                   
month       -0.383  0.041                            
trtmnttrtmn -0.178 -0.301 -0.031                     
gendermale   0.065 -0.102 -0.003  0.219              
age         -0.655 -0.015  0.009 -0.050 -0.263       
centre2     -0.184  0.150 -0.015  0.058 -0.147 -0.223

       (Intercept)       baselinepoor              month treatmenttreatment 
        5.37919357         0.04603378         0.90363768         8.69940763 
        gendermale                age            centre2 
        1.22445202         0.97485954         2.84815273 

```

`(1+subject)` kullanımı gruplamayı kişi bazında yapıyor ve her grup
için kesinin değişmesine izin veriliyor. Regresyonun sonucu 2.16,
$exp(2.16)=8.67$, yani bu ilaç aslında hastanın iyileşmesinde 8.67 kat
etkili! Bu çok daha büyük bir rakam ve gerçek sonuç aslında bu. Yaş
değişkeninin standart hatasına bakarsak, daha büyük olduğunu görüyoruz,
yani bu katsayı daha uygun bir seviyeye gelmiş bulunuyor.

Kaynaklar

[1] Gelman, Hill, {\em Data Analysis Using Regression and
  Multilevel/Hierarchical Models}

[2] Everitt, *A Handbook of Statistical Analysis Using R*

[3] Bayramlı, 
   *iPython, rpy2, rmagic*, 
   [https://burakbayramli.github.io/dersblog/sk/2015/02/ipython-rpy2-rmagic.html](https://burakbayramli.github.io/dersblog/sk/2015/02/ipython-rpy2-rmagic.html)





