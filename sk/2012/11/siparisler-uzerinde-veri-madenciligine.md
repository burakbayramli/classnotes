# Siparisler Uzerinde Veri Madenciligi, Regresyon

Regresyon, veri zenginleştirme, yapay öğrenim tekniklerinden
bahsettik. Bu yazıda bu tekniklerden bazılarını örnek üzerinde
işleyeceğiz.

Örnek olarak dellstore2 adlı tabandaki siparişleri kullanarak bir
müşterinin ne kadar bize "uğramayacağını", yani atıl kalacağını tahmin
etmek istiyoruz. Bunun için sipariş ve bağlantılı tablodaki verileri
genişletilmiş bir tabloya alarak, bu tablo üzerinde yapay öğrenim
tekniklerini uygulayabiliriz. Regresyon hedef değişkeni müşterinin ne
kadar atıl kaldığı, diğer değişkenler ise sipariş ve bağlantılı diğer
kolonlar. Önce veriyi oluşturalım, SQL şöyle olabilir

```
select rank() over (partition by o.customerid order by ol.orderlineid) as rank,
count(*) over (partition by o.customerid) as per_customer_count,
sum(o.totalamount) over (partition by o.customerid) as total_total_amount,
o.orderid,
o.customerid,
o.totalamount,
o.netamount,c.zip,'c' || c.creditcardtype as creditcardtype,
c.gender,
c.age,
c.income,
o.orderdate,
cat.categoryname,
extract(year from o.orderdate) as year,extract(month from o.orderdate) as month,
extract(day from o.orderdate) as day,
extract(dow from o.orderdate)+1 as day_of_week,
case when extract(month from o.orderdate) in (3,4,5) then 'SPRING'
     when extract(month from o.orderdate) in (6,7,8) then 'SUMMER'
     when extract(month from o.orderdate) in (9,10,11) then 'AUTUMN'
     when extract(month from o.orderdate) in (12,1,2) then 'WINTER'
     end as season,count(*) over (partition by o.customerid,cat.categoryname) as cat_freq,
first_last.min,
first_last.max,

extract('epoch' from ('2005-01-01'::timestamp - first_last.max)) / 60 / 60 / 24 as last_visitfrom orders o
join customers c on o.customerid = c.customerid
join orderlines ol on ol.orderid = o.orderid
join products p on ol.prod_id = p.prod_id
join categories cat on p.category = cat.category
left outer join(
 select o.orderid as oid,
 min(o.orderdate) over (partition by o.customerid) as
min, max(o.orderdate) over (partition by o.customerid) as max from
orders o) as first_last on first_last.oid = o.orderidorder by
customerid,orderlineid
```

Burada aklimiza ne geliyorsa onu siparise baglayarak tek satir olarak
sunmaya calistik, bir suru ek kolon yarattik, mesela mevsim, haftanin
gunu gibi. Bazi aciklamalar:

first_last adindaki alt sorgu, her musterinin "ilk ve en son siparis
tarihini" almak icin kullanildi. Sonra bu tarih, customerid uzerinden
dis sorgudaki siparise baglanacak, ve now() - __bu son tarih__ hesabi
uzerinden atillik ortaya cikartilacak (not, bu ornekte simdi icin
2005-01-01 kullandik, gercek dunya verisi uzerinde now() olurdu.

per_customer_count musterinin tum siparislerini sayarak o musteriye
ait her siparise bu toplami yazar.

rank() ile musterinin siparisleri siraya dizilir, ve 1,2,3,.. gibi bir
sira sayisi bu siparislere verilir. Bu sira sayisi onemli olabilir,
1. sipariste olan olaylar, 2. siparisteki olanlardan ayirilabilir bu
sekilde, mesela..

_freq ile biten kolonlar en ilginci: Ama ondan sonra kategorik
(metinsel) veriyi regresyonda nasil kullanacagiz onu dusunelim:

Musterinin siparis ettigi film genre'sini (Aksiyon, Korku, Dram gibi
-ki bunlar metinsel kategoriler-) biliyoruz. Peki bu verileri, bir
sekilde, numerik hale cevirip kullanabilir miyiz? Ilk akla gelen
cozum, her genre'ya rasgele bir sayi atamak. Fakat regresyon bu
sayilarla ne yapacak? Regresyon icin 2'nin 1'den buyuk olmasinin bir
anlami var, fakat tiplere rasgele sayi atadigimizda bu tur irdeleme
sacma.

Cozum genre kolonunundaki mumkun her degeri bir ek kolon haline
getirmek ve siparis hangi genre icinde tekabul eden kolon altina '1'
degeri vermek, yoksa '0' degeri kullanmak. Yani kolon degerlerini
"yana dogru acarak" ek kolonlar haline getirmek (iyi bir
numaradir). Bu kodlama sekline literaturde 1-in-n, ya da 1-hot
kodlamasi (encoding) ismi veriliyor. Bu durumda bu ek kolonun 1/0
degeri tasimasi anlamli, deger ya vardir, ya yoktur, ve regresyon, bu
degerlerle is yapabilir.

Simdi _freq isine gelelim: kategorik veri uzerinde bir takla daha
atmak mumkun. Mesela musterinin siparisleri soyle gidiyor: Aksiyon,
Aksiyon, Dram, Aksiyon. En cok aksiyon seyrediliyor, ama bir ara drama
geciliyor sonra geri donuluyor. Bu degisimi nasil yakalariz? Soyle -
her genre icin alim frekansini hesaplariz, ve onu Postgres analitik
fonksiyonu ile tum tekabul eden satirlara yayariz. Yani Aksiyon,
Aksiyon, Dram, Aksiyon yerine 3,3,1,3 kullanilir. Kabaca bu soyle dis
dunyaya gozukur: musteri en cok aldigi genre'yi ardi ardina iki kez
almis, sonra cok az aldigi bir genre'ya gecmis, sonra geri
donmus. Burada bir onemli bir bilgi olabilir - bu degisimin oldugu
siparisteki diger bilgilerle birleserek belki regresyon bize yeni bir
takim seyler sunabilir belki de. Bu teknikte de sayilarin buyuklukleri
anlamli, en cok, en az gibi irdelemeler var ve bunlar veriyle uyumlu,
en cok denen sey 3, en az denen 1, vs.

Bu teknik pek cok diger kategorik veri icin kullanilabilir, mesela
musterinin o siparis icin kullandigi isletim sistemi, baglanti cihazi,
kullandigi adres, kullandigi isim, vs.

Verinin satir sayisi hakkinda bir not:

orders (siparis) ile orderlines'i (siparis kalemleri / alt siparis)
birlestirdigimiz icin eldeki veri sayisi orderlines ile ayni hale
geldi, bu sebeple orders uzerindeki veriler alt siparis uzerinde
tekrar etti. Bu birlesimi yapmak gerekti cunku ornek icin genre
kategorisi alt siparise bagli bir bilgiydi. Bu veride tekrara sebep
olsa da bu ornek icin fazla onemli degil.

Artik regresyon kullanabiliriz. R dilini kullanacagiz, bu dil
kategorik verileri 1-in-n kodlamasina otomatik olarak
ceviriyor. Egitim ve test setleri lazim, tum script soyle:

```
psql dellstore2 -h localhost -p 5432 -U postgres -c "COPY (`cat data.sql`) TO stdout with delimiter ',' CSV HEADER "  > dell.csv
```

```
head -1 dell.csv > dell2.csvsed -n 2,30000p $HOME/dell.csv >> dell2.csvhead -1 dell.csv > dell-validate.csvsed -n 30000,60000p dell.csv >> dell-validate.csv
```

Veri uretildikten sonra onu iki parcaya ayiriyoruz, dell2.csv egitim
seti, dell-validate.csv test seti. head -1 kullaniminin sebebi ilk
satirdaki kolon isimlerini almak. sed ile belli satir no'lari
arasindaki satirlari cekip cikariyoruz. Eger rasgele satirlar cekip
cikartmak istesek,

```
cat dell.csv | perl -n -e 'print if (rand() < .008)' >> dell-sample.csv
```

kullanabilirdik. Simdi R koduna gelelim:

Kod 

Cikti soyle:

```
                          Estimate Std. Error  t value Pr(>|t|)
    (Intercept)
              3.695e+02  1.910e+00  193.428  < 2e-16 ***month
                   -2.315e+01  9.398e-02 -246.384  < 2e-16 ***netamount
                5.027e+02  9.166e+01
    5.484 4.17e-08 ***genderM
                  1.195e+00  4.835e-01
    2.471  0.01348 *
  day
                     -7.253e-01  2.733e-02  -26.538  < 2e-16 ***day_of_week
             -7.094e-02  1.207e-01
   -0.588  0.55668
    totalamount
             -4.644e+02  8.467e+01
   -5.484 4.17e-08 ***rank
                     3.222e-01
  7.645e-02
    4.214 2.51e-05 ***categorynameAnimation
    2.181e-01
  1.355e+00
    0.161  0.87213
    categorynameChildren
    -7.310e-01
  1.364e+00
   -0.536
  0.59188
    categorynameClassics
    -2.954e-01
  1.353e+00
   -0.218
  0.82721
    categorynameComedy
      -3.352e-01
  1.365e+00
   -0.246
  0.80602
    categorynameDocumentary -1.131e+00
  1.357e+00
   -0.833  0.40477
    categorynameDrama
        7.911e-01
  1.389e+00
    0.570
  0.56888
    categorynameFamily
      -1.221e+00
  1.363e+00
   -0.896
  0.37007
    categorynameForeign
     -8.789e-01
  1.354e+00
   -0.649
  0.51636
    categorynameGames
       -7.045e-01
  1.368e+00
   -0.515
  0.60652
    categorynameHorror
      -2.934e+00
  1.386e+00
   -2.117
  0.03428 *
  categorynameMusic
        8.873e-01
  1.367e+00
    0.649
  0.51623
    categorynameNew
          3.155e-01
  1.377e+00
    0.229
  0.81872
    categorynameSci-Fi
      -6.511e-01
  1.376e+00
   -0.473
  0.63616
    categorynameSports
      -1.550e+00
  1.372e+00
   -1.130
  0.25856
    categorynameTravel
      -1.273e+00
  1.368e+00
   -0.931
  0.35187
    per_customer_count
      -5.879e+00  1.023e-01
  -57.456  < 2e-16 ***total_total_amount
       3.218e-04
  3.876e-04
    0.830
  0.40640
    income
                   1.244e-05  8.590e-06
    1.448
  0.14769
    seasonSPRING
            -4.909e-02
  8.853e-01
   -0.055
  0.95578
    seasonSUMMER
             3.973e+00
  7.396e-01
    5.371 7.84e-08 ***seasonWINTER
            -1.032e+01
  8.319e-01
  -12.404  < 2e-16 ***cat_freq
                 2.047e-01  3.374e-01
    0.607
  0.54411
    creditcardtypec2
        -2.199e+00
  7.668e-01
   -2.868
  0.00414 ** creditcardtypec3
        -1.874e+00  7.665e-01
   -2.445  0.01449 *
  creditcardtypec4
        -8.769e-01
  7.713e-01
   -1.137
  0.25560
    creditcardtypec5
        -9.416e-01
  7.715e-01
   -1.221
  0.22228
   
```

Bu sonucu nasil irdeleriz? Bildigimiz gibi regresyon cevap
degiskenlerini (response variables) ilk basta bilinmeyen katsayilar
ile carpip bu carpimlarin toplanarak hedefin hesaplandigini varsayan
bir tekniktir. Eldeki veri kullanilarak bu carpma / toplama = hedef
formuluna en cok "uyan" katsayilar hesaplanir. Ustteki cikti bize bu
katsayilari vermektedir, mesela totalamount icin -4.644e+02 katsayisi
verilmis, bu eksi yonde buyuk bir sayi, yani siparisin toplam
fiyatindaki bir birimlik degisim, atillik uzerinde eksi yonde buyuk
bir degisim getirir. Std Error kolonuna bakiyoruz, 8.467e+01 sayisi
var. Bu bilgi soyle kullanilir, eger katsayi degerinin sifirdan
uzakligi Std. Error'un iki katindan fazla ise katsayi istatistiki
olarak anlamli  / degerli (significant) demektir ve kullanilabilir
[2].

Ayrica, daha once bahsettigimiz gibi kategorik kolonlar, mesela mevsim
(season), tasidigi degerlerle birlestirilerek yeni kolonlar haline
geldi, mesela yaz icin seasonSUMMER var, kis icin seasonWINTER. Bu
yapay kolonlar ustteki raporda tek basina gercek bir kolon olan mesela
gun (day) ya da rank ile ayni seviyede verilmis.

Ciktinin sonlarinda su satir gorulur:

Residual standard error: 47.32

Bu deger regresyonun RMSE hatasidir [1], yani hedefin hesabina kabaca
47 gun yaklasilabilmistir. Yani 20 gun atillik tahmini yapildigi
zaman, belki gercek hesap aslinda 67. Daha az da olabilir, fazla da
(ya da tam hedef tutturulabilir).

Not: RMSE bu tur problemlerde uygun olmayabilir. Netflix verisi icin
regresyon hedefi 1-5 arasi degismeyen bir sayiydi, orada daha uygun;
atillik rakami ust siniri olmayan bir hedef degiskeni. Diger bir hata
hesabi, hatalarin tam degerlerinin (absolute value) ortalamasini
dondurur. Bu sekilde hesaplaninca sonuc 36 cikacaktir.

Peki bu atillik hesabi yeterli midir? Burada sirketteki karar
vericileri dusunur, eger bu hata payi iyi, yeterli bulunmazsa
alternatiflere bakilir. Veri madenciligi acisindan farkli teknikler
kullanilabilir. Mesela yapay sinir aglari (neural net). Bu arada,
ustteki rakamlarin uretilmis veriden geldigini unutmayalim, yontemin
basarisini irdelemek icin gercek dunya verisine bakmak daha iyi
olacaktir.

YSA 

R ile kurmak icin install.packages("nnet") 

Kod

YSA girdileri bir ara katman uzerinden ciktiya baglar, egitilmesi
backprop denen iteratif bir algoritma iledir. YSA ve nnet kullanirken
bilinmesi iyi olacak bazi puf noktalari: Her iterasyon / dongu (epoch)
YSA'nin hatasini azaltmali. Her dongude bu sayi ekrana basilir, eger
hic azalma olmadan dongu pat diye biterse (YSA tamam oldu dese de),
aslinda hicbir sey yapilmamistir. Bu durumda decay parametresi ile
oynamaya baslamak gerekir. Bu parametrenin ne olacagi hedef olarak
hesaplanan seyin buyuklugune oranla dusunulebilir, eger hedef 100'lu,
200'lu bir sayi ise, decay, yani her dongude uygulanan azalmanin (bu
konunun detaylari icin bir YSA kitabina danisabilirsiniz), 1 ve 2
civarinda olmasi mantikli olabilir. Eger 0.5, 0.6 gibi bir hedef var
ise, decay belki 0.01 olmalidir, vs. Bu sayi deneme yanilmayla
bulunabilir. Fakat onemli olan dongunun en az birkac kez "donmesi" ve
hata sayisinda azalma gorebilmemiz.

Ustteki YSA isletimi ortala hata olarak 53 sayisini dondurdu.

GLM

Regresyon yapmanin bir yolu daha, genel lineer modeller (generalized
linear models - GLM). Bu modellerde regresyondaki katsayilar ve
onlarin carptigi ogeler toplanarak bir "baglanti fonksiyonuna (link
function)" gecilir, ve hedef degiskeni carpimlarin toplami degil bu
suzulmus yeni degerdir. R dilinde GLM kullanmak icin mesela

model ->

Burada link fonksiyonu Gaussian uzerinden log, yani hedef degiskeninin log'u alinmis hali islenecek. Konunun detaylari icin R dokumanlarina bakilabilir.

Dikkat: onemli bir konu su, bazen normal regresyon lm'e log'u alinmis hedef degiskenlerin verildigini gorebilirsiniz, bu durum ustteki durumdan farklidir, glm log  icin disaridan bir log islemine gerek yok, hedef degiskenini oldugu gibi verirsiniz, ve iceride o islem yapilir. Link fonksiyonlarinin varligi GLM cozum matematigini etkilemektedir - bir link fonksiyonunun olacagi bilindigi icin cozum icin buna gore taktikler izlenir; lm durumunda degiskenler bazi filtrelerden geciriliyor olsa bile metotun kendisinin bu degisimlerden haberi olmayacagi icin farkli sonuclar ortaya cikabilir.

GLM durumunda link log olunca, predict ile verilen degerler de log'lu
geri gelecektir, onlari normal sayiya dondurmek icin exp (log'un
tersi) uygulamak gerekir.

Kod

Ustteki kodun ortalama hatasi 19 gun.

Not: Hata hesabini R lm() egitim verisi uzerinde yapti, ve bu hesap
RMSE hesabidir, yani hatalarin karesinin toplaminin karekokunun veri
sayisina bolunmesiyle elde edilir. RMSE hesabi cogunlukla egitim
sonrasi egitilmis model kullanilarak test verisi uzerinde uygulanir.

Not: Tabii burada biraz daha ek detay var; mesela kisilerin
arabalarinin beygir gucunu kazandiklari maasa baglayan bir regresyon,
beygir gucu katsayisi icin beygir basina $10  ve std. hata $2 vermisse
bu istatistiki olarak onemli, ama pratikte onemsizdir. Benzer sekilde
eger beygir katsayisi icin $10,000 ve std. hata $10,000 bulmussak, bu
istatistiki olarak onemsiz, ama pratikte onemlidir. Yani R lm()
sonuclarini rapor ettikten sonra bu ek irdelemeleri de akilda tutmak
gerekir.

