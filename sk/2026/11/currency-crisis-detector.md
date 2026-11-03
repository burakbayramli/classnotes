# Kur Krizi Alarm Yazılımı

Sabitlenmiş kurlar devaluasyon yasamaya yakın midir? Bir ülkenin kur
krizi yaşama ihtmalı nedir? The Global Macro Database [1] adı verilen
veriyi kullanarak kriz tahmini yapma amaçlayan bir yazılımı
paylaşacağız.

GMD verisi IMF, Dünya Bankası, OECD verilerini birleştirir ve finans,
kur krizlerini etiketleyerek araştırmacıların rahatça erisebileceği
bir kaynak sağlar. Veriyi direk [2]'den zip / csv olarak
indirebilirdik, bir diğer yöntem,

```
pip install global_macro_data
```

sonrası

```python
from global_macro_data import gmd
df = gmd(fast="yes")
df.to_csv('/opt/Downloads/gmd/gmd.csv')
```

kullanımı. Bu yöntem eğer güncel yeni veri varsa en son versiyonu
indirir, yoksa eski versiyonu tutar.













[devam edecek]

Kaynaklar

[1] <a href="https://github.com/KMueller-Lab/Global-Macro-Database">GMF</a>

[2] https://www.globalmacrodata.com/