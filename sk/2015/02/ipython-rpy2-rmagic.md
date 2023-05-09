# iPython, rpy2, rmagic

iPython icinden (ya da bizim Emacs eklentisi emacs-ipython icinden) R
kodlari cagirmak icin bazi kurulum numaralari; ozellikle alttaki ornek
icin.

R kurulacak, biz suradan Ubuntu 12 (precise) icin
r-base-core_3.1.2-1precise0_amd64.deb indirdik ve sudo dpkg -i [deb
dosyasi] ile kurduk, sonra R kutuphaneleri, R komut satiri icinden,

```
install.packages("lme4")
install.packages("HSAUR2")
```

ile kurulabilir. Bu cagrilar bir pencere acip hangi bolgedeki servise
baglanmak istediginizi sorar, birini secin, indirim, kurulum
baslar. iPython baglantisi icin

```
sudo pip install rpy2
```

En son versiyon icin

```
sudo pip install --upgrade rpy2
```

Bende rpy2 kurulumu sirasinda hata mesajlari geldi, -llzma derleyici
secenegi ile alakali (o kutuphane bulunamamis),

```
sudo apt-get install liblzma-dev
```

sonrasi hata gitti. Zaten ipython kurulmus olmali, o da pip ile olur.

Ornek icin

```
%load_ext rpy2.ipython%R library(lme4)%R data("respiratory", package = "HSAUR2")
%R write.csv(respiratory, 'respiratory2.csv')
%R resp <- subset(respiratory, month > "0")
%R resp$baseline <- rep(subset(respiratory, month == "0")$status,rep(4, 111))
%R resp_lmer <- glmer(status ~ baseline + month + treatment + gender + age + centre + (1 | subject),family = binomial(), data = resp)
%R -o resp_lmer_summary resp_lmer_summary = summary(resp_lmer)
%R -o exp_res exp_res = exp(fixef(resp_lmer))print resp_lmer_summaryprint exp_res
```

Simdi bu isin bir kismini Python ile bir kismini R ile yapalim,

```python
import pandas as pd
df = pd.read_csv('respiratory2.csv',index_col=0)
baseline = df[df['month'] == 0][['subject','status']].set_index('subject')
df['status'] = (df['status'] == 'good').astype(int)
df['baseline'] = df.apply(lambda x: baseline.ix[x['subject']],axis=1)
df['centre'] = df['centre'].astype(str)df = df[df['month'] > 0]
```

```
%R -i df%R resp_lmer <- glmer(status ~ baseline + month + treatment + gender + age + centre + (1 | subject),family = binomial(), data = df)
%R -o res res = summary(resp_lmer)
%R -o exp_res exp_res = exp(fixef(resp_lmer))print resprint exp_res
```

Bu ayni sonucu verecektir.

%R ile -i ile degisken veriliyor, -o ile degisken aliniyor. Pandas
dataframe objeleri sorunsuz R'ye verilebiliyor.

%R icinde print ile birsey basmak, bazi ortamlarda islemiyor, o
sebeple summary() sonucunu bile biz bir degiskene set edip onu python
ile ekrana basiyoruz.

R'yi hala bazi durumlarda kullanmak lazim, mesela ustte gosterilen R
lme4 glmer kullanimi (karisim etkileri -mixed effects- modelleri)
halen statsmodels'da kodlanmis degil.

DTA, TXT Cevirmek, R Dili

S, R, Stata gibi paketler DTA dosyalari kullanabiliyorlar, bu
dosyalari R dilinde txt formatina cevirmek icin

```
library ("foreign")
x = read.dta ("[dta file name]")
write.table(x, file = "[txt file name]")
```




