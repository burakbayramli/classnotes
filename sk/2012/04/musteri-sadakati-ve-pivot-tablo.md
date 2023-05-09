# Musteri Sadakati ve Pivot Tablo Kullanimi


Musteri Sadakati ve Pivot Tablo Kullanimi




Onceki yazida Pandas ile nasil pivot tablosu yaratabilecegimizi gorduk. Bir baska yazida dellstore tabaninda ilk siparis tarihi ve guncel siparis tarihleri yanyana dokebilecegimizi gorduk, ve ay bazinda bu bilgiyi gosterdik. Simdi ilginc bir ek daha: eger bu iki ay kolonunu x,y kordinati gibi kullanip ucuncu bir musteri sayisi kolonunu pivot tablosunda gosterseydik ne olurdu?

Onceki yazidaki (date_trunc kullanan sorgu) SQL'in ciktisini kolon isimleriyle ve virgullu ayrilmis sekile CSV olarak kaydedin. Simdi alttaki kod ile
import numpy as np
from pandas import *
df = read_csv('/tmp/out')

pv_cust = df.pivot('reg_date','cdate','customer_count')
pv_cust.to_csv('out1.csv',  sep='\t', na_rep='')
pivot dosyasini out1.csv'e basiyoruz.
reg_date        2004-01-01      2004-02-01      2004-03-01
2004-01-01      979.0   43.0    46.0    54.0    43.0    47.0
2004-02-01              930.0   42.0    49.0    57.0    50.0
2004-03-01                      882.0   38.0    47.0    26.0
2004-04-01                              835.0   36.0    45.0
2004-05-01                                      792.0   42.0
2004-06-01                                              763.0
Ciktinin bir merdiven goruntusu vermesi raslanti degil, tarihler solde ve ustte sirali, ve tanim itibariyle musteri ilk alim yaptigi tarihten once alim yapmis olamayacagina gore, ustteki cikti ortaya cikacak. Tablonun faydasi, herhangi bir reg_date icin gerekli satiri bulup o satiri saga dogru takip etmek, ve arka arkaya her ay neler oldugunu hemen gorebilmek.

Ustteki ciktiya ilginc bir ek caprazdaki sayiyi bir yuzde 100 olarak almak, ve ondan sonraki sayilari "eski musterilerin kacta kaci tekrar alim yapti" sorusuna cevap olarak daha dusuk yuzdeler olarak gostermek (keske onlar da yuzde 100 olsa!). Bu ek kod
pv_cust_perc = pv_cust.copy()
for date in pv_cust_perc.index:
for val in pv_cust_perc.T[date]:
    if np.isnan(val) == False:
        pv_cust_perc.T[date] = (pv_cust_perc.T[date] / val) * 100
        break

pv_cust_perc.to_csv('out2.csv',  sep='\t', na_rep='')
Bu ciktiyi da out2.csv de bulabilirsiniz.





