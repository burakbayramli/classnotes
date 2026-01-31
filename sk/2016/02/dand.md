# DanD

İsminin sonundaki 'd' geleneksel olarak bu tür takipçi programlara
verilen 'deamon' sözünden ileri geliyor, inetd örneğindeki gibi. Not:
dand kelimesinin İngilizce okunuşu "dandy" ile benzeşir, ki bu kelime
argoda 'herşey iyi gidiyor' demek - böylece bir espri de yapmış
oluyoruz.

Dand bir süreç takipçisi, `cron` ve `supervisord` programlarının ise
yarayan özelliklerinin bir birleşimi. Cron'un ayar usulü, mesela `* *
*` kullanımı, gün için sayı kullanmak bir acaiptir bilindiği gibi;
dand tüm ayarları yaml formatında yazılmış tek basit bir conf ile
yapar. Belli gün, saat, dakika, vs için işletim mümkün. Çöken programı
otomatik olarak tekrar başlatabilir, kaç kez bunu yapmak istenildiği
conf içinde ayarlanabilir. Dand bir conf içinde gördüğü tüm süreçleri
paralel olarak başlatır, takvimli olsun, takvimsiz olsun. Yani en
basit kullanımda en azından hızlı bir şekilde paralelize etmeye
yardımcı olması çok faydalı.

Kaynaklar

[dand.zip](dand.zip)
