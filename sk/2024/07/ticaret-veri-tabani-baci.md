# Ticaret Veri Tabanını İşlemek, BACI

200 ülke için ikili ticaret ilişkilerinin verisi BACI tabanında
paylaşılıyor [1].  Bu yazıda tabanın 2022 yılı için olan verisini
işleyeceğiz, veri 5000 ürün kategorisini içermeke, kategori sistemi
"harmonize edilmiş sistem" adı verilen 6 sayılık bir kod kullanıyor.

BACI kayıtları yönsel bilgi içerir, yani her ülke ikilisi için ihraç
edici / ithal edici kayıtlanmıştır, ve bu ilişkide her ürünün
ihraç/ithal miktarı ve değeri de tabanda vardır. Taban tek bir csv
dosyası, kolonlar,

```
t: yıl
i: ihraççı
j: ithalcı
k: ürün
v: değer
q: miktar
```

Para degeri 1000 Amerikan doları, miktar ise metrik ton.











Kaynaklar

[1] http://www.cepii.fr/cepii/en/bdd_modele/bdd_modele_item.asp?id=37

[2] Bayramlı, [Paralel, Satır Bazlı Dosya İşlemek](../../2016/02/toptan-islemler-paralelizasyon.html)

