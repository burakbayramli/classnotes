# HTML içinde HTML nasıl gösterilir?

Bu sitede olduğu gibi, öğretmek icin XML/HTML kodları gösteriyorsanız,
bir süre sonra fark edeceksiniz. HTML içinde HTML gösterilmiyor!
Bunun sebebi gayet basit. Internet sayfa programınız `<H1>` gibi bir
kelime görünce, bunun kendisi HTML kodu için, onu işlemeye
uğraşıyor. Halbuki biz sadece komutun kendisini göstermeye
uğraşıyorduk öğretmek için.  Bu problemin bir kaç çözümü var... Öteki
HTML "korsan" metodları bir tarafa atarsak, kalan en temiz ve ileri
İnternet yayıncılığına uygun çözüm XML içinde CDATA etiketine
kullanmakla olur.  XML ile HTML farklı diyebilirsiniz, ve
haklısınız. HTML bizim öğretmek için göstermeye uğraştığımız
etiketleri direk görsel şekline çevirmeye uğraşıyordu. XML dökümanlari
ise bu ogretilen etiketleri kendinden zannedip, birbirine uydurdurmaya
uğraşacaktır. Tabii ki başaramayınca, hata mesajı verecektir.  XML
standartına göre, CDATA işaretini kullanırsanız, onun takibindeki
kelimeler etiket olarak sayılmaz.  `<!  CDATA[ <baslik>` HTML icinde
HTML nasil gosterilir </baslik> ]] >`

Çözüm bundan ibaret.




