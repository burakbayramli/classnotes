# Web, HTML, Görüntü Dosyaları, Tıklama Kordinatları, Görsel Veri Gömmek

### Resim Verisi Gömmek

HTML sayfaları içinde `<img src=.../>` ile jpg, png dosyalarını
gösterebiliyoruz, `img` etiketinin `width` gibi seçenekleri var, imaj
dosyasının pek çok özelliği etiket içinde tanımlanabiliyor.

Seçenek `src=..` ile bir dosya ismi verilir, dosya İnternet'ten
erişilebilen `https://www.site.com/files/dosya.png` gibi bir
isimdir. Fakat aslında görüntü dosyasının verisi direk etiket içine de
gömülebiliyor. Bir ikisel veri bu tabii ki bu ikisel veriyi bir
şekilde metin formatında verebilmek lazım (ancak o şekilde HTML içine
koyabiliriz), bu da base64 formatı ile yapılabilir.

Üstteki yaklaşım özellikle önizleme görseli (thumbnail) yaratırken işe
yarayabilir, büyük resimler vardır, onların ufak hali ise direk HTML
içine gömülmüş halde servis edilebilir.

Bir `images.png` dosyası  içeriğini base64 olarak 

```python
import base64, imageio, io
from PIL import Image

img = imageio.imread('images.png')
img = Image.fromarray(img).resize((10, 10))
buffer = io.BytesIO()
img.save(buffer, "PNG")
encoded_string = base64.b64encode(buffer.getvalue())
print (encoded_string)
```

```text
b'iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAYAAACNMs+9AAAAOklEQVR4nO3KMQ3AMAwAwVcpVDKahJuDrVLIZAgDfyl0yNibjyq1mqNSfIx1u2Ir3WQIU8CLj/54Jr5UQBkFUixHywAAAABJRU5ErkJggg=='
```

ile alabiliriz. Resmi küçülttük ve base64 formatını aldık. Şimdi bu
çıktıyı alıp altta `img` içine `data:image/png;base64,` ifadesinden
sonra koyuyoruz,

```
<img width="100" src='data:image/png;base64,iVBORw0KGgoA...'/>
```

Bu HTML'e baktığımızda ufaltılmış resmi göreceğiz.

[Örnek](test1.html)

### Resim Üzeri Tıklama Kordinatı

Resim bölgesi üzerindeki tıklama verisini Javascript üzerinden
`onclick` olayına (event) bağlanan bir fonksiyon ile alabiliriz. Altta
bir örneği görülüyor,

```
<html> 
  <script>
    function func1(event) {
        let x = event.clientX;
        let y = event.clientY;
        document.getElementById('output').innerHTML = x + "," + y;
    }
  </script>
  <body>
    <img width="300" onclick="func1(event)" src="images.png"/>
    <div id='output'></div>
  </body>
</html>
```

[Örnek](test2.html)
