# Veriyi Ayriksallastirmak (Discretization)


Veriyi Ayriksallastirmak (Discretization)




Pandas ile eldeki veriyi ayriksallastirmak icin herhangi bir sayisal kolon uzerinde qcut kullanilabilir. Bu cagri istatistiksel bolgeler (quartile) uzerinden hesap yapan bir cagridir, belli bir kolon icin numerik degerler belirlenen araliklara bolunur ve eslemesi geri rapor edilir. 

import pandas as pdimport numpy as npdata = {'state': ['Ohio', 'Ohio', 'Ohio', 'Nevada', 'Nevada'],        'year': [2000, 2001, 2002, 2001, 2002],        'pop': [1.5, 1.7, 3.6, 2.4, 2.9]}df = pd.DataFrame(data)cats = pd.qcut(df['pop'], 3)df['pop_cat'] = catsdf['pop_cat'] = "pop_" + df['pop_cat']

Bu cagridan sonra her satirin belli bir araliga eslendigini gorecegiz (cikti altta). Cagri qcuts'dan gelen objeye bakmak ilginc olabilir, icinde bir dizin (array) bir de "seviyeler (levels)" adinda 2. bir dizin var. Ana dizin icinde her nokta icin (veriyle ayni sirada) hangi araligin secildigi gosteriliyor. Siranin veri ile ayni olmasi sayesinde zaten pop_cat adli yeni bir kolona atama yaptigimiz anda Pandas o kolonu ana veriyle hemen esleyebiliyor. Bu iyi bir ozellik bu arada, elde mesela bir Numpy vektoru var ise, ve bu vektor DataFrame'imiz satir sayisi ile esdeger tane veri iceriyorsa, o zaman o vektoru alip cat diye aninda DataFrame'e yeni bir kolonmus gibi verebiliriz (ya da eskisinin uzerine yazabiliriz).

Levels ise o araliklari teker teker listeliyor, ustteki ornek icin 3 tane.

Biz ayrica ufak bir ek yaparak araligin hangi kolona ait oldugu gozuksun diye onu da veri icine ekledik. DataFrame icinde zaten belli oluyor ama duz CSV'ye versek ve mesela fpgrowth gibi bir algoritma onun uzerinde islese sonuclari rapor ederken aralik ve veri baglantisi daha rahat belli olur.


   pop   state  year             pop_cat0  1.5    Ohio  2000    pop_[1.5, 1.933]1  1.7    Ohio  2001    pop_[1.5, 1.933]2  3.6    Ohio  2002    pop_(2.733, 3.6]3  2.4  Nevada  2001  pop_(1.933, 2.733]4  2.9  Nevada  2002    pop_(2.733, 3.6]

Istatistiksel bolgeler bu arada verinin frekansina hassastir (ki bu iyi), median ve mode arasindaki fark gibi, quartile islemi mode gibi hesaplanir, mode bilindigi gibi veri noktalarini siraya dizip ortadaki noktayi dondurur. Ortalama gibi toplama bolme yapmaz. 




