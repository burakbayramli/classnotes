# TensorFlow.js, Javascript ile TensorFlow, Keras Modelleri

TensorFlow.js, Javascript ile TensorFlow, Keras Modelleri

Kullanıcı / masaüstü / cep ortamında yapay zeka modelleri kullanmak
için Tensorflow.js var. Mesela Android cep ortamında YZ modellerini
Java üzerinden kullanmak akla gelebilirdi, fakat Javascript ve
tarayıcı üzerinden bu iş daha rahat. Javascript çoğunun bildiği dil
(HTML ve sayfa kodlamasi icin kullanılır), ve dinamik tiplemesi
sayesinde daha hızlı geliştirmeye imkan verir.

Ayrıca Javascript'in, hatta servis tarafında Node'un bu kadar kullanım
bulmasının arkasında bir sebep daha var; Google, Chrome'u yazarken o
zamanki Javascript motorlarının performansını beğenmedi. Bu sebeple
Chrome için V8 projesiyle Javascript motorunu tekrar baştan
yazdılar. V8 çok hızlı performansı sayesinde beğeni gördü, Node biraz
da bu sebeple on plana çıkabildi (servis tarafında Javascript!).

Bir örnekle başlayalım. Ama ondan önce bir kullanım gözlemi: bu
alandaki örneklerin çoğu tarayıcı üzerinde model eğitmekten
bahsediyor. Fakat gerçek dünyada olacak şudur; GPU, TPU, vs. üzerinden
Python ile model servis tarafında eğitilecektir, kaydedilecektir. Bu
model Tensorflow.js üzerinden kullanıcı tarafında yüklenecek, ve model
tahmin amaçlı olarak Javascript üzerinden kullanılacaktır. Servis
tarafında model eğitmek için Javascript kullanımı herhalde çok
görülmez; tabii şöyle bir kullanım olabilir, eğitilmiş model kullanıcı
verisiyle "zenginleştirmek" için ufak veri parçaları ile eğitimi devam
ettirilebilir.

Biz yine de JS ile eğiten bir program görelim, basit bir lineer regresyon örneği,

[https://js.tensorflow.org](https://js.tensorflow.org)

Önce bir geliştirme "ortamı" hazırlayalım. HTML ve Javascript bazlı
çalışacağız, bir html dosyası olacak, kodları ayrı js dosyasında yazıp
HTML'den dahil ederiz, sayfayı F5 ile tekrar yükletince kod tamamen
işler halde olsun.  Çıktıları göstermek için bir fonksiyon yazalım,
çıktıyı HTML div öğesi içine yazsın. Ayrıca dosyaları Flask üzerinden
servis edelim, statik dosyaları bile, ki bunlar /static/ URL'i altında
olsunlar, bunun sebebi ileride bir modeli TensorFlow.js'e yüklettirmek
gerektiğinde bu dosyanın http üzerinden servis edilmesi gerektiği. O
yüzden baştan bir (mikro)servis ortamında ise başlayalım.

```
<html>
  <head>
    <script src="https://cdn.jsdelivr.net/npm/@tensorflow/tfjs@0.13.3/dist/tf.min.js"> </script>
    <script src="/static/test.js"></script>
  </head>
  <body>
    <div id="output">
    </div>
  </body>
</html>
```

Eğer İnternet bağlantısı olmadan yerel Web uygulaması servis edilmek
istenirse, üstteki tf.min.js dosyası wget ile indirilir, /static/
altına konur, o zaman 

```
<html>
  <head>
    <script src="/static/tf.min.js"> </script>
    <script src="/static/test1.js"></script>
  </head>
  <body>
    <div id="output">
    </div>
  </body>
</html>
```

Javascript kodlari test1.js icinde

```
function dprint(s) {
  document.getElementById("output").innerHTML += s + "<br/>";
}

const model = tf.sequential();
model.add(tf.layers.dense({units: 1, inputShape: [1]}));

model.compile({loss: 'meanSquaredError', optimizer: 'sgd'});

const xs = tf.tensor2d([1, 2, 3, 4], [4, 1]);
const ys = tf.tensor2d([1, 3, 5, 7], [4, 1]);

model.fit(xs, ys).then(() => {
  res = (model.predict(tf.tensor2d([5], [1, 1])));
  dprint(res);
});
```

Bir model eğitiyoruz, sonra test verisini tahmin için modele
soruyoruz. HTML sayfasını ziyaret edince 

```
Tensor
     [[5.8115611],]
```

sonucunu gorecegiz. Tensorflow.js isliyor demektir.


Mevcut Keras Modeli

Şimdi Python ile model eğitip JS ile yükleme örneği görelim. Su
yazidaki en basit MNİST modelini alalım, eğitelim. Eğittikten sonra
modeli Tensorflow.js'in tanıyacağı hale getirmek için tensorflowjs
paketi lazım.

`pip install tensorflowjs`

ile kurulur (Collab ortamında hücre içinde başa ünlem konur). Eğitim kodu içinde 

```
import tensorflowjs as tfjs
..
model = ...
...
tfjs.converters.save_keras_model(model, "/vs/vs/out")
```

Simdi `/vs/vs/out` adli yeni bir dizin görülecek. Bu dizin altında iki
dosya var, model.json ve group1-shard1of1, bunların ikisini de Flask
/static/ altına alırız. Şimdi test amaçlı modele geçilecek MNIST
verisi lazım. MNIST verileri 28x28 boyutunda bir görüntü verisidir,
önceki eğittiğimiz Keras modeli bu veriyi (1,784) boyutunda bekler, o
zaman Python kodu ile birkaç örnek görüntü verisi çekip çıkartalım,
mesela

```
from keras.datasets import mnist

(x_train, y_train), (x_test, y_test) = mnist.load_data()

idx = 0
print (y_test[idx])
print (list(x_test[idx].reshape(784,)))

idx = 100
print (y_test[idx])
print (list(x_test[idx].reshape(784,)))

idx = 3
print (y_test[idx])
print (list(x_test[idx].reshape(784,)))
```

bize gerekli veriyi gösterecek. Sırasıyla 7, 6 ve 0 sayılarının
görüntü verisi bunlar. Veriyi alıp Javascript'e koyarız, ve modele
tahmin için sorarız. Bakalım Keras MNİST modeli ne diyecek?

```
function dprint(s) {
  document.getElementById("output").innerHTML += s + "<br/>";
}

const test1 = tf.tensor2d([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 84, 185, 159, 151, 60, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 222, 254, 254, 254, 254, 241, 198,
198, 198, 198, 198, 198, 198, 198, 170, 52, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 67, 114, 72, 114, 163, 227, 254, 225, 254, 254, 254, 250,
229, 254, 254, 140, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
17, 66, 14, 67, 67, 67, 59, 21, 236, 254, 106, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 83, 253, 209, 18, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 22,
233, 255, 83, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 129, 254, 238, 44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 59, 249, 254, 62, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 133, 254, 187,
5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 9, 205, 248, 58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 126, 254, 182, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 75, 251, 240, 57, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 19, 221, 254,
166, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 3, 203, 254, 219, 35, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 38, 254, 254, 77, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 31, 224, 254, 115, 1, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 133,
254, 254, 52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 61, 242, 254, 254, 52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 121, 254, 254, 219, 40, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 121, 254,
207, 18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], [1,
784]);

const test2 = tf.tensor2d([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 127, 221, 52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 229, 219, 104, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 235, 140,
4, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 118, 227, 25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 236, 133, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 243, 93, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 85, 243, 21,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 189, 236, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 1, 208, 169, 0, 0, 0, 0, 0, 0, 64, 151, 151, 135,
74, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 26, 254, 89, 0, 0, 0, 0,
6, 142, 254, 224, 211, 181, 241, 70, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 26, 254, 68, 0, 0, 0, 2, 161, 254, 104, 7, 0, 0, 80, 223, 15, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 57, 254, 15, 0, 0, 0, 150, 231, 68,
1, 0, 0, 0, 9, 231, 26, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 79, 254,
15, 0, 0, 24, 228, 66, 0, 0, 0, 0, 0, 0, 196, 87, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 73, 254, 43, 0, 0, 116, 251, 7, 0, 0, 0, 0, 0, 0, 196,
100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 13, 230, 147, 0, 0, 60, 255,
70, 0, 0, 0, 0, 0, 4, 209, 84, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
203, 232, 4, 0, 42, 253, 74, 0, 0, 0, 0, 0, 114, 233, 17, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 87, 252, 147, 0, 0, 154, 229, 132, 123,
123, 63, 93, 248, 65, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
169, 249, 137, 23, 8, 80, 100, 101, 107, 145, 192, 51, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 139, 251, 224, 144, 115, 115, 195,
254, 187, 48, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
55, 141, 203, 244, 180, 129, 67, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0], [1, 784]);

const test3 = tf.tensor2d([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 150, 253,
202, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 37, 251, 251, 253, 107, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 21, 197, 251, 251, 253, 107, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 110, 190, 251, 251,
251, 253, 169, 109, 62, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 253, 251, 251, 251, 251, 253, 251, 251, 220, 51, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 182, 255, 253, 253, 253,
253, 234, 222, 253, 253, 253, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 63, 221, 253, 251, 251, 251, 147, 77, 62, 128, 251, 251, 105,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, 231, 251, 253, 251, 220,
137, 10, 0, 0, 31, 230, 251, 243, 113, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 37, 251, 251, 253, 188, 20, 0, 0, 0, 0, 0, 109, 251, 253,
251, 35, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 37, 251, 251, 201, 30, 0,
0, 0, 0, 0, 0, 31, 200, 253, 251, 35, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 37, 253, 253, 0, 0, 0, 0, 0, 0, 0, 0, 32, 202, 255, 253, 164, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 140, 251, 251, 0, 0, 0, 0, 0, 0, 0, 0,
109, 251, 253, 251, 35, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 217, 251,
251, 0, 0, 0, 0, 0, 0, 21, 63, 231, 251, 253, 230, 30, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 217, 251, 251, 0, 0, 0, 0, 0, 0, 144, 251, 251,
251, 221, 61, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 217, 251, 251, 0,
0, 0, 0, 0, 182, 221, 251, 251, 251, 180, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 218, 253, 253, 73, 73, 228, 253, 253, 255, 253, 253,
253, 253, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 113, 251, 251,
253, 251, 251, 251, 251, 253, 251, 251, 251, 147, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 31, 230, 251, 253, 251, 251, 251, 251, 253,
230, 189, 35, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 62,
142, 253, 251, 251, 251, 251, 253, 107, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 72, 174, 251, 173, 71, 72, 30, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0], [1, 784]);

tf.loadModel('http://localhost:5000/static/model.json').then(model => {
  res = (model.predict(test1));
  dprint (res);
  res = (model.predict(test2));
  dprint (res);
  res = (model.predict(test3));
  dprint (res);
});
```

Sonuc

```
Tensor
     [[0, 0, 0, 0, 0, 0, 0, 1, 0, 0],]Tensor
     [[0, 0, 0, 0, 0, 0, 1, 0, 0, 0],]Tensor
     [[1, 0, 0, 0, 0, 0, 0, 0, 0, 0],]
```

Görüldüğü gibi doğru bit 1 halinde, sırasıyla 7., 6. ve 0. bitler.




