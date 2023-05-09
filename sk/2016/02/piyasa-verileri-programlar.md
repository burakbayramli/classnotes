# Piyasa Verileri, Programlar, dand

Vadeli Islem Sozlesmeleri (futures contracts), ABD'nin buyuk
piyasalarinda (NYSE, AMEX, NASDAQ) kayitli sirketlerin senet verileri,
ETF (exchange traded funds) fiyatlari, sirketlerin kar acikladigi
gunlerin kayitlarini acik kaynaklardan (Yahoo, Google) toplamak ve
erisimini kolaylastirmak icin quant_at projesi altinda bazi ekler
yaptik.

Veriler

Veri kaynaklarina baglanir, MongoDB'de olan son kayitlara gore sadece
farki indirir, yoksa 2006'dan baslar, ve gun bazindaki fiyat
verilerini Mongo'ya ekler. Veriye basit erisim icin yardimci
fonksiyonlar saglar. Vadeli islem sozlesmeleri icin futures.py'a
bakilabilir, ayrica yuksek frekansli (dakika bazli) fiyatlarin
indirilmesi destekleniyor.

Dand

Isminin sonundaki 'd' geleneksel olarak bu tur takipci programlara
verilen 'deamon' sozunden ileri geliyor, inetd ornegindeki gibi. Not:
dand kelimesinin Ingilizce okunusu "dandy" ile benzesir, ki bu kelime
argoda 'hersey iyi gidiyor' demek - boylece bir espri de yapmis
oluyoruz.

Dand bir surec takipcisi, cron ve supervisord programlarinin ise
yarayan ozelliklerinin bir birlesimi. Cron'un ayar usulu, mesela * * *
kullanimi, gun icin sayi kullanmak bir acaiptir bilindigi gibi; dand
tum ayarlari yaml formatinda yazilmis tek basit bir conf ile
yapar. Belli gun, saat, dakika, vs icin isletim mumkun. Coken programi
otomatik olarak tekrar baslatabilir, kac kez bunu yapmak istenildigi
conf icinde ayarlanabilir. Dand bir conf icinde gordugu tum surecleri
paralel olarak baslatir, takvimli olsun, takvimsiz olsun. Yani en
basit kullanimda en azindan hizli bir sekilde paralelize etmeye
yardimci olmasi cok faydali.






