# Yabancı Dil Cümleleri ve Tercümeleri Kaydetmek

Dil öğreniminde kelime ezberlemek yerine tam cümleleri (tercümesi ile
birlikte) duymanın öğrenimde daha iyi olduğu söyleniyor. Bu amaç için
acaba metin bir dosyasında duran yabancı dil cümleler + tercümelerini
okuyu, otomatik olarak onlardan bir ses kaydı yaratan bir program
yazabilir miyiz acaba?

Rusça için bazı cümleler ve tercümelerini [1]'den indirdik. Dosya
yapıları tüm diller için ve istenen iki çift için onları eşleştirmek
gerekiyor, bizim örnekte İngilizce ve Rusça. Dosyaları indirip,
zıpleri açıp `/opt/Downloads/rü_lang` altına koyarsak, `lang_data.py`
kodu bu eşleştirmeyi yapar.

Nihai dosya `en_ru_pairs.csv` içinde, her satırda önce Rusça cümle,
sonra virgülden sonra onun İngilizcesi var. Bu dosya çok fazla ise,
oradan seçip istenilenleri biz `en_ru.csv` içine koyulabilir.

Dosyaların ses dosyası haline çevirilmesi için gerekli paketler,

```
pip install edge-tts
pip install pydub
```

Altta görülen `reclang.py` kodu `en_ru.csv` içindeki cümleleri okuyup
bir MP3 dosyasına kaydeder. Cümleler rasgele sırada seçilir, eğer
dinleyici hep aynı sıradan sıkılırsa farklı sıradaki bir mp3
yaratabilir.

Ses dosyasında her cümle önce İngilizce, sonra iki kez Rusça
kaydediliyor, ikinci Rusça okuma birinciden biraz daha yavaş, eğer
dinleyen kelimeleri tam duymadıysa ikinci kez onları duyma şansını 
böylece elde eder.

Kodlar

[reclang.py](reclang.py), [lang_data.py](lang_data.py)

Kaynaklar

[1] https://tatoeba.org/en/downloads

