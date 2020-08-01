# Cok Cekirdekli Islemciler

Mahout sitesinde bir makalenin islemciler ile alakali bolumunde guzel
bir saptama:

"Mikroislemcilerde frekans bazli olcekleme yapmak, yani islemcinin
saat hizini daha yukselterek daha yuksek performans elde etmek guc
kaynagi kullanimi sinirlarina carpmaya basladi. Islemcinin boyutu
kuculdukce guc sizmasi (power leakage) yasaniyor. Diger taraftan Moore
kanununa gore islemcilerin yogunlugu her nesilde ikiye
katlanacaktir. O zaman frekansi sabit tutarak ama her cipteki islemci
cekirdegi ikiye katlayarak hala dusuk guc kullaniminda devam
edebiliriz ve islemci gucunu ikiye katlamis oluruz. Bu durum, mikrocip
endustrisini cok cekirdekli mimarilere itmistir".Yani soylenmek
istenen tek islemcinin transistor yogunlugu ile oynayarak artik daha
fazla hiz elde edemiyoruz, bazi sinirlara toslamaya basladik. O zaman
cok cekirdekli mimari, ayni hizda ama paralel calisan ek cekirdekler
koyarak hizlanmayi saglayacaktir.Bunun programcilar icin getirdikleri
faydalar / sorunlar nelerdir? Programcilar daha paralel kodlama
baglaminda dusunmeye alismali, programlama dilleri bu servisleri daha
rahat saglayabilmeli. Referans verilen makale paralelizasyon fikrini
yapay ogrenim konularina uygulamaya calisiyor mesela. Yelpazenin diger
ucunda ise, daha fazla paralel calisan "makinalarin" sayesinde devasa
boyutlarda veri isleyebilme konulari var. Aslinda bu iki trend
birbiriyle yakinda alakali. Hem tekil makinalarin fiyati ucuzlarken,
hem de tekil makinalarin icindeki paralel cekirdeklerin sayisi
artiyor. Yelpazenin her noktasinda paralelizasyona dogru bir gidisat
yasaniyor.Bu konu hakkinda hemen bir sey yapilmasi gerekli demiyoruz;
sadece akilda tutulmasi gereken bir konu.




