# Anlik Veri Ambari


Anlik Veri Ambari




Suradaki prezentasyondan notlar:

Eski veri ambari teknolojileri / firmalariyla 1 terabayt depolamak, islemek $10,000'a varabilecek bir masraftir. Ucuz makinalari birlestirerek ortaya cikartilan Hadoop kumeleri ile ayni veriyi birkac yuz dolarlik masrafla isleyebilirsiniz. Bu buyuk bir degisim. 

Bunun sonucu olarak yaklasimda bazi degisiklikler oldu: Hadoop durumunda ambar "semasi" onceden tanimlanmaz, bu sebeple veri transferi (ETL) ile ugrasmak gerekmez. Bu kararin onceden verilmesine gerek yoktur. Elde ne veri varsa, duz dosya, zip formatinda dosya, vs. olarak Hadoop'a tikilir.

Daha sonra, gereken analize gore esle / indirge kodlari ile veriler istenildigi sekilde birlestirilir. Bu yapilabilir cunku 300, 400, 1000 makinayi yanyana getirmek kolaydir (bulut servisinde mesela), ve performans artik ucuz satin alinabilen bir seydir. Hadoop durumunda  her sorgu aslinda bir "anlik ambar" yaratabilir.

Hatta Platfora, Zoomdata gibi teknolojiler, urunler bu sorgularin gorsel araclar uzerinden yapilmasina izin veriyor. Tiklama, secme gibi aksiyonlar, arka planda esle / indirge uretiyorlar ve gelen cevap gorsel sekilde onunuzdeki raporda gosteriliyor. 






