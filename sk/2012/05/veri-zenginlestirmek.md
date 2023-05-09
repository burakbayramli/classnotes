# Veri Zenginlestirmek

Simdiye kadar gordugumuz partition..over ile nasil ek kolon, bilgi
yaratabildigimizi gorduk. Veriyi zenginlestirmenin veri madenciligi
acisindan faydalari var. Mesela dellstore2 tabaninda orders tablosu
uzerinde orderdate (siparis tarihi) var. Fakat mesela siparisin hangi
mevsimde yapildigi yok. Belki yaz aylari ile musterinin yaptigi
siparisler arasinda bir baglanti vardir. O zaman bu tur akla
gelebilecek pek cok ek bilginin mevcut bilgiden cekip cikartilmasi, ya
da diger tabanlardan birlestirilerek getirilmesi gerekir. Bazi
numaralar:

PG case-when-then ibaresi cok ise yariyor. Bu ifade SELECT icinde sart
irdelemesi yaparak ilginc sartlara gore ek bilgi yaratmayi
kolaylastirir. Mesela

```
SELECT .., case when amount > 2000  then 'VIP' else 'NORMAL' end as status, ..
```

gibi bir ifade miktar (amount) kolonuna bakiyor, eger 20 uzeri ise
musterinin statusunu VIP, yani "en onemli musterilerden" diye
isaretliyor.

Bir kayiti akla gelebilecek her turlu sekilde isaretlemeye ugrasmak
lazim, bu seceneklerden bazilari ilk basta akla yatkin gelmese de
ilginc faydalar saglayabilirler. Mesela "son 6 aydir siparis vermemis
musterileri" isaretlemek. Kod

```
SELECT .., last_value(orderdate) over (partition by customerid) < (now() - interval '6 months') as did_not_come_back_past_6_months, ..
```

Onceden first_value gormustuk, simdi de last_value goruyoruz). Iste bu
sekilde belli bir musterinin verdigi son siparis tarihinin son 6 aya
gelip gelmedigini dogru / yanlis (t/f) seklinde yeni bir kolon olarak
yarattik. Ve dikkat: bu bilgi sadece musterinin en son siparisine
tekabul eden kayda degil, tum kayitlarina ekleniyor. Eger musteri son
6 ayda alisveris yaptiysa, tum kayitlari 't' diyecek, yoksa hepsi 'f'
diyecek.
