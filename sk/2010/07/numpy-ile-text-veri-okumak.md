# Numpy ile Text Veri Okumak


Numpy ile Text Veri Okumak





Tablo formatinda duz text dosyasi okumak istiyorsak, numpy kutuphanesinde loadtxt fonksiyonu kullanilabilir. Soyle bir dosya oldugunu farzedelim:
Website hits from May 23 to June 22  
Day    Count
Sat    7
Sun    12
Mon    20
Tues    30

Bu dosyanin ilk iki satirini okumak istemeyiz, o kisim baslik (header) kismi. Onlari atlamak, sol kolonu string, sag kolonu integer tipinde alip, tamamini tek bir Numpy matrisi icine atmak istiyorsak, komut soyle:

import numpy

type = numpy.dtype([('column1', 'S10'),
                   ('column2', 'uint32')])
fin = numpy.loadtxt("veri.txt",  dtype=type, skiprows=2)
 
Mumkun diger bazi ornekler 
 

dtype=[('first_channel', 'S10'), 
                         ('returning_customer', 'S10'), 
                         ('order_month', 'i8'),
                         ('order_year', 'i8'),
                         ('payment','S10'),
                         ('amount','f8'),
                         ('order_count','i8'),
                         ('total_amount','f8'),
                         ('order_zip','S10'),
                         ('restaurant_zip','S10'),
                         ('referrer','S100'),
                         ('channel','S10'),
                         ('front_end','S4'),
                         ('commision','f8'),
                         ('coupon_code','S20'),
                         ('campaign_description','S200'),
                         ('preorder','S5'),
                         ('season','S10'),
                         ('did_not_come_back_past_6_months','S1')
                         ]






