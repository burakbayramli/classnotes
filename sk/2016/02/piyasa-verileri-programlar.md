# Piyasa Verileri, Programlar

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

