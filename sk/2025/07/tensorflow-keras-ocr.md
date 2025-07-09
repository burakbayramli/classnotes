# Keras OCR, Hazır YSA Ağırlıkları, Tensorflow

Derin Yapay Sinir Ağları (YSA) ile çalışmak isteyenler için en son
kurulum, kullanım kılavuzu alttadır.. Ubuntu 24 Python 3.12.3 nispeten
en son versiyonlar ve bu versiyonların `pip` ile kurduğu paketler
birbiri ile uyumlu olmayabiliyor. En stabil kombinasyon,

Python 3.11

Tensorflow 2.15.1

Keras 2.15.x

ve bunun için bazı versiyonlarda geriye gitmek lazım. [1] ile yeni bir
izole ortam yaratalım, orası için yeni Python derlemek gerekir, biz
alttakini kullandık. Kurulum ardından faydalı olabilecek ve "önceden
pişirilmiş" bir YSA kullanacağız, Keras OCR.

```
wget https://www.python.org/ftp/python/3.11.9/Python-3.11.9.tgz
```

Derlemeden önce dikkat, alttakiler lazım, yoksa SSL işlemiyor,

```
sudo apt install libssl-dev libffi-dev zlib1g-dev
```

[1]'i takip ederek derlemeyi yapalım, sanal ortamı kuralım. Bitince
ortama girip,

```
pip install tensorflow==2.15.1 keras-ocr matplotlib
```

gerekli paketleri kurar.

Pür CPU kullananlar için başlangıç mesajlarını bastırmak iyi olabilir,

```python
import os
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3' 
os.environ['CUDA_VISIBLE_DEVICES'] = '-1'
import tensorflow as tf
```

### Keras OCR

İmaja bakarak içindeki yazı karakterleri tanımak YSA'lar ile
yapılabiliyor. Fakat bilindiği gibi modern derin YSA'ların eğitimi
çoğunlukla çok veri gerektirir, bu sebeple eğitim zamanlar saatlar,
hatta bazen günler sürebiliyor. Fakat eğitimi bitmiş bir YSA'nın
kullanımı, yani tahmin fazi daha hızlıdır. YSA eğitmek demek onun iç
ağırlık değerlerini bulmaktan ibarettir, o zaman bir YSA hazır
ağırlıklarla yüklenebilirse, eğitim fazını kendimiz yapmaktan
kurtuluruz.

Keras birkaç problem alanı için böyle hazır ağırlıkları paylaşır,
mesela Keras OCR [2]. Bu paket üstteki `pip` komutu ile kuruldu, ve
gerekli hazır pişmiş YSA ağırlıklarını ilk işlediğinde otomatik olarak
indirecektir.

Mesela alttaki resmi işleyelim,

![](street1.jpg)

Kodlar

```python
import os
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3' 
os.environ['CUDA_VISIBLE_DEVICES'] = '-1'
import keras_ocr

pipeline = keras_ocr.pipeline.Pipeline()

images = [
    keras_ocr.tools.read(url) for url in [
        'street1.jpg'
    ]
]

prediction_groups = pipeline.recognize(images)

ps = prediction_groups[0]

for p in ps: print (p[0])
```

```
Looking for HOME/.keras-ocr/craft_mlt_25k.h5
Looking for HOME/.keras-ocr/crnn_kurapan.h5
1/1 [==============================] - 1s 778ms/step
1/1 [==============================] - 1s 1s/step
sample
text
```

Çıktıda "sample" "text" sonuçlarını görüyoruz, demek ki Keras OCR
doğru sonucu buldu.

Kaynaklar

[1] [Virtualenv](../../2018/08/virtualenv-python-izole-sanal-calsma.html)

[2] [Keras OCR](https://keras-ocr.readthedocs.io/en/latest/examples/using_pretrained_models.html)



