# R ve iPython Baglantisi

iPython not defteri icinden direk R kullanmak mumkun. Hatta Pandas
DataFrame objesini bile R'ye gecmek mumkun. R baglantisi ipython
icindeki sihir (magic) fonksiyonlari ile kullaniliyor, bu fonksiyonlar
yuzde % ile basliyorlar ve dis sistem baglantisi, hata ayiklayici ile
iletisim gibi "kutu disinda" olan servisleri ipython icine
getiriyorlar.

Arka planda R ile baglanti rpy2 ile yapiliyor, bunun icin

```
sudo apt-get install python-rpy2
```

iPython'un kendi ek baglanti kodlari da lazim - bu kutuphanenin ismi
rmagic, ki kodlari ipython 0.14 (su anki son stabil surum) icinde
var. Eger alttaki load_ext calismazsa, 0.14'u alip kurmak lazim.

Kurulduktan sonra, not defteri, ya da pur ipython icinden alttakiler
isler.

```
In [2]:%load_ext rmagic
%R X=c(1,4,5,7); sd(X); mean(X)

Out [2]:array([ 4.25])

In [3]:from pandas import *
parasiteData = read_csv("parasite_data.csv", sep=",", na_values=["", " "])

%R -i parasiteData
print(summary(parasiteData))
       V1
              V2
             V3
         Min.
   : 0.50
   Min.
   : 1.0
   Min.
   :0.0000
 
 1st Qu.: 0.60
   1st Qu.:13.0
   1st Qu.:0.0000
 
 Median : 0.75
   Median :25.5
   Median :0.8457
 
 Mean
   : 0.75
   Mean
   :25.5
   Mean
   :0.8364
 
 3rd Qu.: 0.90
   3rd Qu.:38.0
   3rd Qu.:1.5337
 
 Max.
   : 1.00
   Max.
   :50.0
   Max.
   :2.9008
  
```
