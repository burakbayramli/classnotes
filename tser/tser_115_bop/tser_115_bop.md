# Kur Fiyatı Modellemesi, Para Dengesi

Uluslararası Finans

Bir ülkenin ödemeler dengesi o ülkenin vatandaşları ve dünyadaki diğer
kişiler arasındaki tüm alışverişi, ticari işlemleri yıllık bazda gösteren
bir tablodur [9]. Malların, servislerin, sermayenin, insanı yardım gibi her
türlü parasal transfer bu tabloda bir şekilde gösterilir. Ödemeler dengesi
muhasabesi IMF standartlarına göre yapılır.

Aynen şirket muhasabesinde olduğu gibi her ticari işlem (transaction)
"defterde'' iki kayıda sebep olur (çift taraflı, bilanço usulü kayıt
sistemi). Bu kayıtların biri kredi (credit) diğeri borç (debit). Defterleme
sisteminde tüm borçlar ve krediler toplanınca sonuç sıfır olmalıdır, bu bir tür
sağlama, kontrol yöntemidir, eğer sıfır değilse eksik olanlar kalemler aranır,
kaydedilir, böylece her işlemin defterde bir yerde olması garantilenir.

Kredi kalem örneği mesela ihracat gelirleri, ülkenin herhangi bir şekilde
elde ettiği gelirler (mesela millileştirilmiş petrol gelirleri), yabancı
varlıklarda (mesela elde tutulan döviz) azalma, yükümlülüklerde (liability)
artış, vs. Borç örnekleri ithalat ürünleri, yabancı varlıklarda artış,
yükümlülüklerde azalış gibi.

Ülkenin girdilerini, çıktılarını bu şekilde düşünmek aydınlatıcıdır. Ne
elde edilmekte, ne verilmektedir, döviz nedir? Mesela ABD olduğumuzu
düşünelim, bizim vatandaşımız Bill Gates bir Japon'a Windows sattı, Japon
Bill Gates'e bu satış için o anki kura göre 5000 yen ödedi. Gates ABD
vatandaşı, bu ABD için ihracat. Peki bu 5000 yen ile ne yapacak?  Diyelim
ki Gates bu parayı yastığının altına koydu. Bu durumda Gates
tasarruflarının bir kısmını kendi ülkesinde yatırım yerine Japon
ekonomisinde yatırıma çevirmiş oldu (Japon para birimi yen üzerinden). ABD
net ihracata karşı ABD net sermaye akış kaleminde (çıkış) birbirine eşit
hale geldi. Bir diğer seçenek Bill Gates'in kendi ülkesindeki bir yerel
bankaya gidip bu parayı dolara çevirmesi. Fakat bu yaptığı ödeme dengeleri
muhasebesi için fark yaratmıyor, çünkü şimdi o yerel bankanın bu 5000 yenle
bir şeyler yapması lazım [10, sf. 119].

Ek bazı örnekler:

1. Bir yabancıya 100 liralık mal sattık. Deftere ihracat altında 100
(kredi), dövizde (yabancı varlık) 100 liralık artış (kredi) yazıyoruz.

2. Bir yabancıya 50 lira tutarında bir yerli şirketin hisselerini sattık.
Deftere yerli varlıklarda 50 liralık azalma (borç) dövizde / finansal
varlıklarda 50 liralık artış (kredi) yazıyoruz.

3. Vatandaşımız 70 liralık dövizi borç alıyor. Vatandaş için yükümlülüklerde
70 liralık artış (kredi), finansal varlıklarda artış (borç) olarak yazılır.

Ödemeler dengesinin alt kalemleri şunlardır:


   * Cari işlemler hesabı (current account)     
   
     * Mallarda ticari ödeme dengesi (balance on goods)
     * Servis kalemlerinde ticari ödeme dengesi (balance on services)
     * Kazanç dengesi (balance on income)
     * Transferlerde ödeme dengesi (balance on current transfers)
   
   * Sermaye hesabı (capital account)
   * Finans hesabı (financial account)
   
     * Doğrudan sermaye giriş (foreign direct investment)
     * Portföy yatırımı
     * Diğer yatırımlar
     * Finansal türevsel enstrümanlarına yatırım
        
   * Net hatalar ve dışarıda kalanlar (net errors and omissions)
   * Finans reserv varlıkları (financial reserve assets)


Ödeme dengeleri (balance of payments) hem cari işlemler hesabı, hem de
sermaye (ve finans) hesabını içerir. Toplamı sıfır olmalıdır. 

Sayısal örnek olarak elimizde Yunanistan'ın 2000-2010 arasındaki ödemeler
dengesi var. Bu verilerin ham hali 2000 senesine bakalım. 

```python
import pandas as pd, csv
df = pd.read_csv('greece_acct.csv',sep='\\s+',index_col=0)
print ((df.T[[2000]]))
```

```
Year                              2000
Goods                         -21927.5
Services                        8711.1
Income                          -955.3
Current Transfers               3553.3
Capital Transfers               2246.0
Direct Investment Abroad       -2319.0
Direct Investment Home          1202.8
Portfolio Inv Assets            -933.0
Portfolio Inv Liabilities      10040.5
Other Investments Assets       -1060.6
Other Investments Liabilities  -3796.2
Loans of General Government     -437.7
Change in Reserve Assets        5771.7
Reserve Assets (Stock)         13208.0
```

Ödemeler dengesinin temel eşitliği şudur:

$$ CA + KA + FA + SD + RES = 0 $$

CA: Cari işlemler hesabı

KA: Sermaye hesabı

FA: Finans hesabı

SD: İstatistiki uyumsuzluk

RES: Rezervler

Duruma muhasebe açısından bakmak gerekirse ödemeler dengesinin her zaman sıfır
olması gerekir. Kalemleri ait olduğu gruplara göre toplarsak ve üstteki formülü
uygularsak,

```python
df['Current Account'] = df['Goods'] + df['Services'] + \
                        df['Income'] + df['Current Transfers']

df['Current Account and Capital Transfers'] = df['Current Account'] + \
                                              df['Capital Transfers']

df['Direct Investment'] = df['Direct Investment Abroad'] + \
                          df['Direct Investment Home']

df['Portfolio Investment'] = df['Portfolio Inv Assets'] + \
                             df['Portfolio Inv Liabilities']

df['Other Investments'] = df['Other Investments Assets'] + \
                          df['Other Investments Liabilities']

df['Financial Account'] = df['Direct Investment'] + \
                          df['Portfolio Investment'] + \
                          df['Other Investments'] + \
                          df['Change in Reserve Assets']

df['Balancing Item'] = -1*(df['Current Account'] + \
                           df['Capital Transfers'] + \
                           df['Financial Account'])
print ((df.T[[2000,2001,2002,2003]]))
```

```
Year                                      2000     2001     2002     2003
Goods                                 -21927.5 -21610.9 -22708.7 -22643.5
Services                                8711.1   9150.0  10755.4  11506.5
Income                                  -955.3  -1981.3  -2073.4  -3975.8
Current Transfers                       3553.3   3856.9   3822.0   3848.7
Capital Transfers                       2246.0   2416.0   1633.5   1239.4
Direct Investment Abroad               -2319.0   -688.5   -696.3   -365.2
Direct Investment Home                  1202.8   1776.1     53.4   1129.9
Portfolio Inv Assets                    -933.0   -514.7  -2230.0  -8737.9
Portfolio Inv Liabilities              10040.5   9979.5  13167.8  21071.8
Other Investments Assets               -1060.6  -1467.0  -7481.9  -4034.5
Other Investments Liabilities          -3796.2  -8327.6   9480.5  -3589.4
Loans of General Government             -437.7  -2809.7  -4510.1  -2618.4
Change in Reserve Assets                5771.7   6177.0  -1983.0   4409.0
Reserve Assets (Stock)                 13208.0   7031.0   9014.0   4605.0
Current Account                       -10618.4 -10585.3 -10204.7 -11264.1
Current Account and Capital Transfers  -8372.4  -8169.3  -8571.2 -10024.7
Direct Investment                      -1116.2   1087.6   -642.9    764.7
Portfolio Investment                    9107.5   9464.8  10937.8  12333.9
Other Investments                      -4856.8  -9794.6   1998.6  -7623.9
Financial Account                       8906.2   6934.8  10310.5   9883.7
Balancing Item                          -533.8   1234.5  -1739.3    141.0
```

```python
print ((df.T[[2004,2005,2006,2007]]))
```

```
Year                                      2004     2005     2006     2007
Goods                                 -25435.8 -27558.9 -35286.3 -41499.2
Services                               15467.0  15391.1  15337.1  16591.7
Income                                 -4377.4  -5676.1  -7209.4  -9285.8
Current Transfers                       3629.0   3100.4   3399.9   1591.1
Capital Transfers                       2386.1   2048.6   3041.3   4332.3
Direct Investment Abroad                -828.8  -1180.4  -3224.4  -3832.9
Direct Investment Home                  1692.4    501.3   4268.8   1542.7
Portfolio Inv Assets                  -11489.4 -18459.7  -6961.2 -16351.1
Portfolio Inv Liabilities              25216.9  25782.3  15076.6  33792.8
Other Investments Assets               -6215.7  -6301.5  -5851.0 -16266.1
Other Investments Liabilities          -2888.4  12215.5  17369.5  29006.8
Loans of General Government            -1027.4   -447.0   -447.7  -2341.7
Change in Reserve Assets                2611.0     49.0   -224.0   -322.0
Reserve Assets (Stock)                  1994.0   1945.0   2169.0   2491.0
Current Account                       -10717.2 -14743.5 -23758.7 -32602.2
Current Account and Capital Transfers  -8331.1 -12694.9 -20717.4 -28269.9
Direct Investment                        863.6   -679.1   1044.4  -2290.2
Portfolio Investment                   13727.5   7322.6   8115.4  17441.7
Other Investments                      -9104.1   5914.0  11518.5  12740.7
Financial Account                       8098.0  12606.5  20454.3  27570.2
Balancing Item                           233.1     88.4    263.1    699.7
```

```python
print ((df.T[[2008,2009,2010]]))
```

```
Year                                      2008     2009     2010
Goods                                 -44048.8 -30767.3 -28279.6
Services                               17135.6  12640.2  13248.5
Income                                -10643.0  -8984.3  -8143.4
Current Transfers                       2758.6   1292.6    198.9
Capital Transfers                       4090.8   2017.4   2071.5
Direct Investment Abroad               -1650.4  -1479.3   -738.8
Direct Investment Home                  3071.1   1753.8    281.4
Portfolio Inv Assets                    -268.9  -8973.0  13278.7
Portfolio Inv Liabilities              16696.9  31636.8 -34133.6
Other Investments Assets              -27823.3 -23875.7   7658.7
Other Investments Liabilities          39917.8  25438.8  34880.2
Loans of General Government             -572.7   2865.0  29978.2
Change in Reserve Assets                 -29.0   -106.0     97.0
Reserve Assets (Stock)                  2521.0   3857.0   4777.0
Current Account                       -34797.6 -25818.8 -22975.6
Current Account and Capital Transfers -30706.8 -23801.4 -20904.1
Direct Investment                       1420.7    274.5   -457.4
Portfolio Investment                   16428.0  22663.8 -20854.9
Other Investments                      12094.5   1563.1  42538.9
Financial Account                      29914.2  24395.4  21323.6
Balancing Item                           792.6   -594.0   -419.5
```

sonucunu görüyoruz. Nihai toplamı, defterdeki dengeyi temsil eden
"balancing item'''ın sıfıra ne kadar yakın olduğunu görüyoruz, bu iyi
haber demek ki bu veri ödemelerdeki hareketleri iyi yakalayabilmiş. 

Kriz

Yunanistan'da bilindiği gibi devlet borçlanmasıyla alakalı bir kriz 2010
yılında ortaya çıktı. Bu krize giderken olanları yukarıdaki tabloda
görebiliyor muyuz?  Yunanistan krize giderken müthiş bir cari açık yaşamış,
ithalatı ihracatından çok daha fazla. Bu açık nasıl kapanıyor? Deftere göre
finans hesabı kaleminde cari açıkla beraber giden büyük değişiklikler
var. Aşırı ithalat yapılmış, ona karşılık rezervlerde değişim yok, ama
Yunanistan'a giren yatırım (portföy yatırımları bölümünde görülen) değişim
var, oraya giden para bu açıkları kapatmış. Fakat her sene tahvile giden
para borç paradır, bir süre sonra bu biriken borcun karşılanamayacağı
ortaya çıkınca yatırımcı paniklemiş, Yunanistan'ın tahvillerini satmaya
başlamış, ve kriz başlamış.

Yunanistan Avro bölgesinde olduğu için orada devlet borcu üzerinden kriz
görüldü, ama benzer dinamikler cari açık, kurları kapsayacak şekilde farklı
para bölgesindeki ülkeler arasında da meydana gelebilir. Mesela Arjantin'in
yüksek cari açıgı var diyelim, ithalatı ihracatından çok fazla. Bu noktada
neoklasik ekonomistler "dalgalı kur varsa bu cari açıga göre döviz kuru
kendini hemen otomatik olarak dengeler'' derler. Bu her zaman doğru
değildir. Üstteki örneklerde gördüğümüz gibi yabancı ithalat için aldığı
pezoyu Arjantin'de yatırım yaparsa döviz kurunda o anda bir baskı
oluşmaz. Bu şekilde bir ülke yıllarca kuru üzerinde baskı yaşamadan
gidebilir. Ama Arjantin'de biriken, biriken yabancı parası bir şekilde
ürküp çıkışa başlarsa, yani elindeki pezoyu satıp mesela dolar alırsa
satılan şeyin fiyatı düşecektir, o anda Arjantin'in para birimi çakılmaya
başlayacaktır.

Türkiye

Benzer analizi Türkiye için yapabiliriz. Kaynak olarak [11] kullanıldı,
rakamlar dolar bazında. Burada bir fark net finans hareketlerinde işaret
değişimi var, bu sebeple nihai toplamdan finans hesabını çıkartmak
gerekiyor, bu usule BPM6 usulü deniyor, detaylar için [12]. Hesapların
toplanma, çıkartılması ardından hata payı da (net errors and omissions)
hesaba katıldıktan sonra geri kalan rezerv hareketlerine eşit olmalı, ve
rezervi bu rakamdan çıkartınca sonucun sıfır olduğunu görebiliyoruz.

```python
import pandas as pd
pd.options.display.float_format = '{:20,.2f}'.format
df = pd.read_csv('tr_account.csv',sep=',',index_col=0)
```

```python
df2 = df.copy()
df2['tmp1'] = df2["Direct_Investment_Net_acquisition_of_financial_assets"] - \
              df["Direct_Investment_Net__incurrence_of_liabilities"]
df2['tmp2'] = df2["Portfolio_Investment_Net_acquisition_of_financial_assets"] - \
              df["Portfolio_Invesment_Net_incurrence_of_liabilities"]
df2['tmp3'] = df2["Other_Investment_Net_acquisition_of_financial_assets"] - \
              df["Other_Investment_Net_incurrence_of_liabilities"]

df2['FA'] =  df2['tmp1']+df2['tmp2']+df2['tmp3']

df2['All_Accounts'] = df2["CURRENT_ACCOUNT"] + \
                      df2["CAPITAL_ACCOUNT"] - \
                      df2["FA"] + \
                      df2['NET_ERRORS_AND_OMISSIONS'] 

df2['Sum'] = df2['All_Accounts'] - df2["RESERVE_ASSETS"]

print (df2[['All_Accounts','Sum']])
```

```
             All_Accounts                  Sum
1999                 5.21                -0.00
2000                -3.00                 0.00
2001               -12.92                 0.00
2002                -0.21                 0.00
2003                 4.10                 0.00
2004                 4.34                 0.00
2005                23.20                 0.00
2006                10.62                -0.00
2007                12.01                -0.00
2008                -2.76                 0.00
2009                 0.79                 0.00
2010                14.97                -0.00
2011                 1.01                -0.00
2012                22.82                -0.00
2013                10.76                 0.00
2014                -0.47                 0.00
2015               -11.83                -0.00
2016                 0.81                 0.00
2017                -8.21                 0.00
```

Bu verinin genel analizi ilginç olabilir. Dosyaya bakarsak, aynen
Yunanistan gibi, hatta belki daha fazla olarak, Türkiye'nin de ciddi bir
cari açıgı (current account deficit) olduğunu görüyoruz. Bu açık neyle
kapanmış?  Finans hesabı altında doğrudan yatırıma (direct investment) ve
borç enstrümanlarına (tahvil -debt securities- gibi) giriş var, bu şekilde
kapanmış.

[9] Rokicki, *Open Economy Macroeconomics Lecture, Balance of Payments*, 
    [http://coin.wne.uw.edu.pl/brokicki/open_economy.html](http://coin.wne.uw.edu.pl/brokicki/open_economy.html)

[10] Mankiw, *Principles of Macroeconomics, 5th Edition, 2008* 

[11] Turkish Central Bank, *BALANCE OF PAYMENTS STATISTICS*, 
     [http://www.tcmb.gov.tr/wps/wcm/connect/2c897137-850e-4d15-9a2a-c0d4735d9986/bop.pdf?MOD=AJPERES&CACHEID=ROOTWORKSPACE-2c897137-850e-4d15-9a2a-c0d4735d9986-m6j7eNQ](http://www.tcmb.gov.tr/wps/wcm/connect/2c897137-850e-4d15-9a2a-c0d4735d9986/bop.pdf?MOD=AJPERES&CACHEID=ROOTWORKSPACE-2c897137-850e-4d15-9a2a-c0d4735d9986-m6j7eNQ)

[12] EU, *BPM6 Implementation*, 
     [http://ec.europa.eu/eurostat/documents/39118/40189/Changes-introduced-BPM6-methodology.pdf/c3621a69-6b2f-4b33-9834-619fb1ae5d9c](http://ec.europa.eu/eurostat/documents/39118/40189/Changes-introduced-BPM6-methodology.pdf/c3621a69-6b2f-4b33-9834-619fb1ae5d9c)









