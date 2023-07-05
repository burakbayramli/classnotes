# Google Tercümesi, googletrans

Google tercüme servisi Google Translate'e komut satırından, program
üzerinden erisebilmek için,

```
pip install googletrans==4.0.0-rc1
```

Anadilden İngilizce'ye,

```python
from googletrans import Translator
translator = Translator(service_urls=[
      'translate.google.com'
    ])
res = translator.translate('dam üstünde saksağan', src='tr', dest='en')
print (res.text)
```

```text
pot on the roof
```

İngilizce'den İspanyolca'ya

```python
translator.translate('hello', src='en', dest='es').text
```

```text
Out[1]: 'Hola'
```

Birkac tane tercume blogunu bir paket icinde gondermek,

```python
res = translator.translate(['The quick brown fox', 'jumps over', 'the lazy dog'], src='en', dest='tr')
```


